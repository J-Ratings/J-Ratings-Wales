
# ── Packages ──────────────────────────────────────────────────────────────
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(curl)
library(tidyr)
library(jsonlite)
library(purrr)
library(tibble)
library(tictoc)
options(stringsAsFactors = FALSE)
tic()
# simple helper for null-coalesce used below (to avoid relying on rlang)
`%||%` <- function(x, y) if (is.null(x)) y else x

# ── USER KNOBS (only these) ───────────────────────────────────────────────
# Calculation:

# ── USER KNOBS (only these) ───────────────────────────────────────────────

RUN_DATE <- Sys.Date()                         # Guard: do not include dates after this.

# Recalculate from one month before the run date.
# CALC_FROM_DATE <- RUN_DATE %m-% months(1)
CALC_FROM_DATE <- as.Date("2026-03-01")

# Normal weekly scrape: look back 7 days.
NORMAL_SCRAPE_DAYS <- 7

# Scheduled deep scrape: every 4 ISO weeks, look back 1 month.
DEEP_REFRESH_EVERY_WEEKS <- 4
DEEP_REFRESH_MONTHS <- 1

# Temporary repair scrape.
# This lets GitHub Actions automatically scrape deeper for a few days,
# then return to the normal short scrape without manual intervention.
FORCE_DEEP_REFRESH_UNTIL <- as.Date("2026-06-25")

is_deep_refresh_week <- function(d = Sys.Date()) {
  lubridate::isoweek(d) %% DEEP_REFRESH_EVERY_WEEKS == 0
}

is_forced_deep_refresh <- function(d = Sys.Date()) {
  !is.na(FORCE_DEEP_REFRESH_UNTIL) && d <= FORCE_DEEP_REFRESH_UNTIL
}

SCRAPE_FROM_DATE <- if (
  is_forced_deep_refresh(RUN_DATE) ||
  is_deep_refresh_week(RUN_DATE)
) {
  RUN_DATE %m-% months(DEEP_REFRESH_MONTHS)
} else {
  RUN_DATE - NORMAL_SCRAPE_DAYS
}

FORCE_SCRAPE_NAMES <- c()                      # If empty, SCRAPE_FROM_DATE is used instead.

# Anyone who has not played since this date is not shown.
EXPORT_ACTIVE_SINCE <- RUN_DATE %m-% months(24)

FORCE_REFRESH_CURRENT <- TRUE
WRITE_SNAPSHOT <- TRUE

cat("Run date:", format(RUN_DATE, "%Y-%m-%d"), "\n")
cat("Calc from:", format(CALC_FROM_DATE, "%Y-%m-%d"), "\n")
cat("Scrape from:", format(SCRAPE_FROM_DATE, "%Y-%m-%d"), "\n")
cat("Deep refresh week:", is_deep_refresh_week(RUN_DATE), "\n")

# ── Constants & paths ─────────────────────────────────────────────────────
PRE_2024_CUTOFF   <- as.Date("2024-07-01")
PROFILE_CUTOFF    <- as.Date("2022-01-01")
DIFF_REPORT_DATE  <- as.Date("2025-07-01")  # optional report date; set NA to skip

# ── External rating-source overrides ──────────────────────────────────────
# These players remain WCU players for identity/profile purposes.
# Their own post-from_date rating feed is rebuilt from an external source.
# WCU games are kept only on dates where the external source has no game.
external_rating_overrides <- tribble(
  ~name,          ~player_key,        ~from_date,            ~source, ~profile_url,
  "Dave McGhee",  "e1 dave mcghee",   as.Date("2025-02-01"), "ECF",   "https://rating.englishchess.org.uk/players/games?domain=S&year=ALL&show_games=on&show_ratings=on&ECF_code=327912B"
)

external_override_names <- external_rating_overrides$name

player_rebuild_start <- function(nm) {
  d <- external_rating_overrides$from_date[
    match(nm, external_rating_overrides$name)
  ]
  
  if (length(d) == 1 && !is.na(d)) {
    as.Date(d)
  } else {
    as.Date(effective_calc_start)
  }
}

# ── CSV schemas (required for empty Current files) ────────────────────────

games_col_types <- cols(
  GameNo            = col_integer(),
  Date              = col_date(),
  Player1           = col_character(),
  Player2           = col_character(),
  OppKey            = col_character(),
  OppRating_Display = col_double(),
  RatingType        = col_character(),
  Result            = col_double(),
  New               = col_double(),
  Tot               = col_double(),
  Exp               = col_double(),
  Pts               = col_double(),
  daily_ord         = col_integer()
)


players_col_types <- cols(
  name        = col_character(),
  zone        = col_character(),
  club        = col_character(),
  grade       = col_character(),
  title       = col_character(),
  last_played = col_date(),
  player_key  = col_character(),
  profile_url = col_character(),
  Pld         = col_integer()
)


repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

cache_dir <- file.path(repo_dir, "pipeline_data", "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

site_root <- repo_dir
data_dir  <- file.path(site_root, "data")
hist_dir  <- file.path(data_dir, "history")
wcu_hist_dir <- hist_dir
dir.create(hist_dir, recursive = TRUE, showWarnings = FALSE)

SNAPSHOT_FILE <- file.path(cache_dir, "ratings_snapshot.rds")

# ── Half-year definitions (2009–CURRENT) ──────────────────────────────────
half_path <- function(y, h) ifelse(h == 1, paste0("JanJune", y), paste0("JulyDec", y))

legacy_zones <- tribble(
  ~zone_dir, ~players_file,
  "east",    "players.php",
  "west",    "playersA.php",
  "north",   "players.php",
  "gwent",   "players.php",
  "dyfed",   "players.php"
)

extra_legacy_halves <- bind_rows(
  expand_grid(year = 2009:2014, h = 1:2) %>%
    transmute(
      label        = paste0(year, ifelse(h == 1, "H1", "H2"), "_west"),
      start_date   = as.Date(ifelse(h == 1, paste0(year, "-01-01"), paste0(year, "-07-01"))),
      base_url     = paste0("https://www.welshchessunion.uk/Grading/xarchive/west/", half_path(year, h), "/"),
      players_file = "playersA.php",
      legacy       = TRUE
    ),
  expand_grid(year = 2009:2014, h = 1:2) %>%
    filter(!(year == 2009 & h == 1)) %>%
    transmute(
      label        = paste0(year, ifelse(h == 1, "H1", "H2"), "_gwent"),
      start_date   = as.Date(ifelse(h == 1, paste0(year, "-01-01"), paste0(year, "-07-01"))),
      base_url     = paste0("https://www.welshchessunion.uk/Grading/xarchive/gwent/", half_path(year, h), "/"),
      players_file = "players.php",
      legacy       = TRUE
    ),
  expand_grid(year = 2010:2014, h = 1:2) %>%
    transmute(
      label        = paste0(year, ifelse(h == 1, "H1", "H2"), "_dyfed"),
      start_date   = as.Date(ifelse(h == 1, paste0(year, "-01-01"), paste0(year, "-07-01"))),
      base_url     = paste0("https://www.welshchessunion.uk/Grading/xarchive/dyfed/", half_path(year, h), "/"),
      players_file = "players.php",
      legacy       = TRUE
    ),
  expand_grid(year = 2011:2014, h = 1:2) %>%
    transmute(
      label        = paste0(year, ifelse(h == 1, "H1", "H2"), "_north"),
      start_date   = as.Date(ifelse(h == 1, paste0(year, "-01-01"), paste0(year, "-07-01"))),
      base_url     = paste0("https://www.welshchessunion.uk/Grading/xarchive/north/", half_path(year, h), "/"),
      players_file = "players.php",
      legacy       = TRUE
    )
)

legacy_halves <- expand_grid(year = 2015:2019, h = 1:2, legacy_zones) %>%
  transmute(
    label        = paste0(year, ifelse(h == 1, "H1", "H2"), "_", zone_dir),
    start_date   = as.Date(ifelse(h == 1, paste0(year, "-01-01"), paste0(year, "-07-01"))),
    base_url     = paste0("https://www.welshchessunion.uk/Grading/xarchive/", zone_dir, "/", half_path(year, h), "/"),
    players_file = players_file,
    legacy       = TRUE
  )

modern_halves <- tribble(
  ~label,   ~start_date,    ~base_url,                                                        ~special,
  "2020H1", as.Date("2020-01-01"), "https://www.welshchessunion.uk/Grading/xarchive/JanJune2020/",      NA,
  "2020H2", as.Date("2020-07-01"), "https://www.welshchessunion.uk/Grading/xarchive/JulyDec2020/",      NA,
  # 2021H1 skipped – no games
  "2021H2", as.Date("2021-07-01"), "https://www.welshchessunion.uk/Grading/xarchive/JulyDec2021/",     "zones",
  "2022H1", as.Date("2022-01-01"), "https://www.welshchessunion.uk/Grading/xarchive/JanJune2022/",      NA,
  "2022H2", as.Date("2022-07-01"), "https://www.welshchessunion.uk/Grading/xarchive/JulyDec2022/",      NA,
  "2023H1", as.Date("2023-01-01"), "https://www.welshchessunion.uk/Grading/xarchive/JanJune2023/",      NA,
  "2023H2", as.Date("2023-07-01"), "https://www.welshchessunion.uk/Grading/xarchive/JulyDec2023/",      NA,
  "2024H1", as.Date("2024-01-01"), "https://www.welshchessunion.uk/Grading/xarchive/JanJune2024/",      NA,
  "2024H2", as.Date("2024-07-01"), "https://www.welshchessunion.uk/Grading/xarchive/JulyDec2024/",      NA,
  "2025H1", as.Date("2025-01-01"), "https://www.welshchessunion.uk/Grading/xarchive/JanJune2025/",      NA,
  "2025H2", as.Date("2025-01-01"), "https://www.welshchessunion.uk/Grading/xarchive/JanJune2025/",      NA,
  "CURRENT",as.Date("2025-07-01"), "https://www.welshchessunion.uk/Grading/",                           NA
) %>%
  mutate(players_file = NA_character_, legacy = FALSE)

half_defs_all <- bind_rows(extra_legacy_halves, legacy_halves, modern_halves)

# ── Constants & helpers for ratings ───────────────────────────────────────
is_welsh_type <- function(x) grepl("^[DEGNW]\\d+$", toupper(trimws(x)))
conv_pre_2024 <- function(x) ifelse(is.na(x), NA_real_, ifelse(x < 2000, x*0.6 + 800, x))

conv_ecf_to_wcu <- function(x) {
  ifelse(is.na(x), NA_real_, ifelse(x < 1750, x * 0.6 + 700, x))
}

update_elo <- function(Ra, Rb, result, k = 20) {
  Ea <- 1 / (1 + 10 ^ ((Rb - Ra) / 400))
  Ra + k * (result - Ea)
}

elo_expected <- function(player_elo, opponent_elo) {
  1 / (1 + 10 ^ ((opponent_elo - player_elo) / 400))
}

clamp <- function(x, lo, hi) {
  pmax(lo, pmin(hi, x))
}

draw_rate_from_gap_base <- function(abs_gap) {
  abs_gap <- as.numeric(abs_gap)
  
  max_draw <- 0.33
  scale <- 340
  shape <- 1.60
  
  max_draw * exp(-((abs_gap / scale) ^ shape))
}

rating_draw_multiplier <- function(avg_elo) {
  case_when(
    avg_elo < 1500 ~ 0.60,
    avg_elo < 1700 ~ 0.98,
    avg_elo < 1900 ~ 1.10,
    avg_elo < 2100 ~ 1.26,
    avg_elo < 2300 ~ 1.28,
    TRUE ~ 1.30
  )
}

draw_rate_from_gap <- function(abs_gap, avg_elo) {
  raw <- draw_rate_from_gap_base(abs_gap) * rating_draw_multiplier(avg_elo)
  
  pmin(raw, 0.46)
}

expected_wdl_from_elo <- function(player_elo, opponent_elo) {
  expected <- elo_expected(player_elo, opponent_elo)
  abs_gap <- abs(player_elo - opponent_elo)
  avg_elo <- (player_elo + opponent_elo) / 2
  
  draw_prob <- draw_rate_from_gap(abs_gap, avg_elo)
  
  max_allowed_draw <- 2 * pmin(expected, 1 - expected)
  draw_prob <- pmin(draw_prob, max_allowed_draw)
  
  win_prob <- expected - draw_prob / 2
  loss_prob <- 1 - expected - draw_prob / 2
  
  win_prob <- clamp(win_prob, 0, 1)
  draw_prob <- clamp(draw_prob, 0, 1)
  loss_prob <- clamp(loss_prob, 0, 1)
  
  total <- win_prob + draw_prob + loss_prob
  
  tibble(
    win_prob = win_prob / total,
    draw_prob = draw_prob / total,
    loss_prob = loss_prob / total
  )
}

pick_k <- function(r) {
  ifelse(is.na(r), NA_real_, ifelse(r >= 2200, 10, ifelse(r >= 1800, 20, 30)))
}
prefer_new <- function(old_df, new_df, keys) {
  if (!nrow(new_df)) return(old_df)
  if (!nrow(old_df)) return(new_df)
  old_df$.is_new <- FALSE
  new_df$.is_new <- TRUE
  bind_rows(old_df, new_df) |>
    arrange(across(all_of(keys)), .is_new) |>
    group_by(across(all_of(keys))) |>
    slice_tail(n = 1) |>
    ungroup() |>
    select(-.is_new)
}

is_snap_date <- function(d) lubridate::day(d) == 1 & lubridate::month(d) %in% c(1, 7)
snap_dates_between <- function(from, to){
  if (is.na(from) || is.na(to) || to <= from) return(as.Date(character()))
  yrs <- seq(lubridate::year(from), lubridate::year(to), by = 1)
  s <- as.Date(c(outer(yrs, c("-01-01","-07-01"), paste0)))
  s[s > from & s <= to]
}
safe_html <- function(url) tryCatch(read_html(url), error = function(e) NULL)

normalize_players_frame <- function(df){
  must <- c("name","zone","club","grade","title","last_played","player_key","profile_url")
  if (!is.data.frame(df) || !ncol(df)) {
    out <- as.list(setNames(rep(list(NULL), length(must)), must))
    return(tibble::as_tibble(out)[0,])
  }
  for (m in must) if (!m %in% names(df)) df[[m]] <- NA
  df %>%
    mutate(
      name        = as.character(.data$name),
      zone        = as.character(.data$zone),
      club        = as.character(.data$club),
      player_key  = as.character(.data$player_key),
      grade       = suppressWarnings(as.numeric(.data$grade)),
      title       = as.character(.data$title),
      last_played = as.Date(.data$last_played),
      profile_url = as.character(.data$profile_url)
    ) %>%
    select(all_of(must))
}

normalize_games_frame <- function(df){
  must <- c("GameNo","Date","Player1","Player2","OppKey",
            "OppRating_Display","RatingType","Result","New","Tot","Exp","Pts","daily_ord")
  if (is.null(df) || !nrow(df)) {
    return(tibble(!!!setNames(vector("list", length(must)), must))[0,])
  }
  for (m in must) if (!m %in% names(df)) df[[m]] <- NA
  df %>%
    mutate(
      GameNo            = suppressWarnings(as.integer(GameNo)),
      Date              = as.Date(Date),
      Player1           = as.character(Player1),
      Player2           = as.character(Player2),
      OppKey            = as.character(OppKey),
      OppRating_Display = suppressWarnings(as.numeric(OppRating_Display)),
      RatingType        = as.character(RatingType),
      Result            = suppressWarnings(as.numeric(Result)),
      New               = suppressWarnings(as.numeric(New)),
      Tot               = suppressWarnings(as.numeric(Tot)),
      Exp               = suppressWarnings(as.numeric(Exp)),
      Pts               = suppressWarnings(as.numeric(Pts)),
      daily_ord = suppressWarnings(as.integer(daily_ord))
      
    ) %>%
    select(all_of(must))
}

# ── Scrapers ──────────────────────────────────────────────────────────────
scrape_players <- function(base_or_full){
  list_url <- if (grepl("playerSummaryView\\.php", base_or_full, ignore.case = TRUE)) {
    base_or_full
  } else {
    paste0(base_or_full, "playerSummaryView.php")
  }
  pg <- safe_html(list_url); if (is.null(pg)) return(tibble())
  tbl_node <- pg %>% html_element("#my-table"); if (inherits(tbl_node, "xml_missing") || is.null(tbl_node)) return(tibble())
  tbl <- tbl_node %>% html_table(fill = TRUE); if (!nrow(tbl)) return(tibble())
  col_or_na <- function(nm){
    i <- match(tolower(nm), tolower(names(tbl)))
    if (is.na(i)) rep(NA_character_, nrow(tbl)) else as.character(tbl[[i]])
  }
  a_nodes <- tbl_node %>% html_elements(xpath = ".//tr[td]/td[1]/a")
  hrefs   <- a_nodes %>% html_attr("href")
  if (length(hrefs) < nrow(tbl)) hrefs <- c(hrefs, rep(NA_character_, nrow(tbl) - length(hrefs)))
  key_raw <- sub(".*[?&]key=", "", hrefs); key_dec <- URLdecode(key_raw)
  dir_base    <- sub("playerSummaryView\\.php.*$", "", list_url, ignore.case = TRUE)
  profile_url <- ifelse(is.na(key_dec) | key_dec == "", NA_character_,
                        paste0(dir_base, "playerDetailView.php?key=", curl_escape(key_dec)))
  tibble(
    name        = str_squish(col_or_na("Player")),
    zone        = col_or_na("zone"),
    club        = col_or_na("club"),
    grade       = suppressWarnings(as.numeric(col_or_na("Grade"))),
    title       = na_if(str_replace_all(col_or_na("Title"), '^"|"$', ""), ""),
    last_played = suppressWarnings(lubridate::dmy(col_or_na("Last played"))),
    player_key  = key_dec,
    profile_url = profile_url,
    Pld         = suppressWarnings(as.integer(col_or_na("Pld")))
  ) %>% filter(!is.na(name) & nzchar(name))
}

scrape_players_legacy <- function(base_url, players_file){
  std_cols <- tibble(
    name        = character(),
    zone        = character(),
    club        = character(),
    grade       = numeric(),
    title       = character(),
    last_played = as.Date(character()),
    player_key  = character(),
    profile_url = character(),
    Pld         = integer()
  )
  parse_legacy_list_page <- function(list_url){
    pg <- safe_html(list_url); if (is.null(pg)) return(std_cols[0, ])
    tables <- pg %>% html_elements("table"); if (!length(tables)) return(std_cols[0, ])
    has_prof <- vapply(
      tables,
      function(tb){
        hrefs <- tb %>% html_elements("a") %>% html_attr("href")
        any(
          grepl("pr\\.php", hrefs %||% character(), ignore.case = TRUE) |
            grepl("(^|/)p[0-9]+\\.php($|\\?)", hrefs %||% character(), ignore.case = TRUE)
        )
      }, logical(1)
    )
    if (!any(has_prof)) return(std_cols[0, ])
    tbl <- tables[which(has_prof)[1]]
    hdrs <- tbl %>% html_elements("tr") %>% .[[1]] %>% html_elements("th,td") %>% html_text(trim = TRUE)
    zone_re <- "^(E1|W1|G1|N1|D1)\\b"
    zone_from_header <- { z <- hdrs[grepl(zone_re, hdrs)]; if (length(z)) sub(zone_re, "\\1", z[1]) else NA_character_ }
    hdrs_lower <- tolower(str_squish(hdrs)); iPld <- match("pld", hdrs_lower)
    a_nodes <- tbl %>% html_elements(xpath = ".//a[contains(translate(@href,'PR','pr'),'pr.php')]")
    if (!length(a_nodes)) return(std_cols[0, ])
    rows <- lapply(a_nodes, function(a){
      nm   <- a %>% html_text(trim = TRUE) %>% str_squish()
      href <- a %>% html_attr("href")
      key_q <- URLdecode(sub(".*[?&](?:id|key|player|pid|pk)=?([^&]+).*", "\\1", href))
      key <- key_q
      if (identical(key, href) || is.na(key) || key == "") key <- sub("\\.php.*$", "", basename(href))
      prof <- xml2::url_absolute(href, list_url)
      if (!nzchar(nm)) {
        prof_html <- safe_html(prof)
        if (!is.null(prof_html)) nm <- prof_html %>% html_elements("h1,h2,h3") %>% html_text(trim = TRUE) %>% .[1] %>% str_squish()
      }
      tr  <- a %>% html_element(xpath = "ancestor::tr[1]"); tds <- tr %>% html_elements("td")
      num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x)))
      first_numeric_after <- function(tds, start_idx, max_lookahead = 6L){
        lim <- min(length(tds), start_idx + max_lookahead)
        for (j in (start_idx + 1L):lim){
          v <- num(html_text(tds[[j]], trim = TRUE))
          if (!is.na(v)) return(v)
        }
        NA_real_
      }
      td_paths <- xml2::xml_path(tds)
      a_td <- a %>% html_element(xpath = "ancestor::td[1]")
      a_idx <- match(xml2::xml_path(a_td), td_paths)
      rating_guess <- if (!is.na(a_idx)) first_numeric_after(tds, a_idx) else NA_real_
      pld_val <- if (!is.na(iPld) && length(tds) >= iPld) suppressWarnings(as.integer(gsub("[^0-9-]", "", html_text(tds[[iPld]], trim = TRUE)))) else NA_integer_
      club <- NA_character_
      if (length(tds) >= 6){
        last_txt <- html_text(tds[[length(tds)]], trim = TRUE) %>% str_squish()
        if (nzchar(last_txt) && is.na(num(last_txt))) club <- last_txt
      }
      tibble(
        name        = nm,
        zone        = zone_from_header,
        club        = ifelse(nzchar(club), club, NA_character_),
        grade       = rating_guess,
        title       = NA_character_,
        last_played = as.Date(NA),
        player_key  = key,
        profile_url = prof,
        Pld         = pld_val
      )
    })
    bind_rows(rows) %>% filter(nzchar(.data$name))
  }
  if (grepl("/east/JanJune2016/", base_url, fixed = TRUE)) {
    letter_urls <- paste0(base_url, "le.php?id=", LETTERS)
    pieces <- lapply(letter_urls, parse_legacy_list_page)
    out <- bind_rows(pieces)
  } else {
    list_url <- paste0(base_url, players_file)
    out <- parse_legacy_list_page(list_url)
  }
  out %>%
    as_tibble() %>%
    mutate(
      name        = as.character(.data$name),
      zone        = as.character(.data$zone),
      club        = as.character(.data$club),
      player_key  = as.character(.data$player_key),
      grade       = suppressWarnings(as.numeric(.data$grade)),
      title       = as.character(.data$title),
      last_played = as.Date(.data$last_played),
      profile_url = as.character(.data$profile_url),
      Pld         = as.integer(.data$Pld)
    ) %>%
    group_by(.data$name) %>% slice_max(coalesce(.data$grade, -Inf), n = 1, with_ties = FALSE) %>% ungroup()
}

scrape_games <- function(players_df){
  if (!nrow(players_df)) return(tibble())
  out <- vector("list", nrow(players_df))
  for (i in seq_len(nrow(players_df))){
    this_url  <- players_df$profile_url[i]
    this_name <- players_df$name[i]
    message(sprintf("[%d/%d] %s", i, nrow(players_df), this_name))
    pg_prof <- safe_html(this_url)
    if (is.null(pg_prof)) { out[[i]] <- tibble(); next }
    nodes <- pg_prof %>% html_elements(
      xpath = "//table[.//th[normalize-space()='No']
                    and .//th[normalize-space()='Date']
                    and .//th[normalize-space()='Opponent']]"
    )
    if (!length(nodes)) { out[[i]] <- tibble(); next }
    num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x)))
    first_numeric_after <- function(tds, start_idx){
      n <- length(tds)
      for (j in seq.int(start_idx + 1L, n)){
        v <- num(html_text(tds[[j]], trim = TRUE))
        if (!is.na(v)) return(list(val = v, idx = j))
      }
      list(val = NA_real_, idx = NA_integer_)
    }
    df <- bind_rows(lapply(nodes, function(node){
      rows <- node %>% html_elements(xpath = ".//tr[td]")
      if (!length(rows)) return(NULL)
      bind_rows(lapply(rows, function(tr){
        tds <- tr %>% html_elements("td"); if (!length(tds)) return(NULL)
        n <- length(tds)
        iNo  <- 1L; iDate <- 2L; iRes <- 5L; iOpp <- 6L
        iNew <- n;  iTot  <- n-1L; iExp <- n-2L; iPts <- n-3L; iType <- n-4L
        opp_a <- tds[[iOpp]] %>% html_element("a")
        opp_h <- if (!is.null(opp_a) && length(opp_a) > 0) html_attr(opp_a, "href") else NA_character_
        oppkey <- if (!is.na(opp_h)) {
          k <- URLdecode(sub(".*[?&](?:id|key|player|pid|pk)=?([^&]+).*", "\\1", opp_h))
          if (identical(k, opp_h) || is.na(k) || k == "") k <- sub("\\.php.*$", "", basename(opp_h))
          k
        } else NA_character_
        fr <- first_numeric_after(tds, iOpp)
        tibble(
          GameNo  = suppressWarnings(as.integer(html_text(tds[[iNo]],  trim = TRUE))),
          Date    = suppressWarnings(lubridate::dmy(html_text(tds[[iDate]], trim = TRUE))),
          Player1 = this_name,
          Player2 = str_squish(html_text(tds[[iOpp]], trim = TRUE)),
          OppKey  = oppkey,
          OppRating_Display = fr$val,
          RatingType = if (iType >= 1 && iType <= n) html_text(tds[[iType]], trim = TRUE) else NA_character_,
          Result  = suppressWarnings(as.numeric(html_text(tds[[iRes]],  trim = TRUE))),
          New     = if (iNew >= 1 && iNew <= n) num(html_text(tds[[iNew]], trim = TRUE)) else NA_real_,
          Tot     = if (iTot >= 1 && iTot <= n) num(html_text(tds[[iTot]], trim = TRUE)) else NA_real_,
          Exp     = if (iExp >= 1 && iExp <= n) num(html_text(tds[[iExp]], trim = TRUE)) else NA_real_,
          Pts     = if (iPts >= 1 && iPts <= n) num(html_text(tds[[iPts]], trim = TRUE)) else NA_real_
        )
      }))
    }))
    if (is.null(df) || !is.data.frame(df) || !nrow(df) || !"Result" %in% names(df)) {
      out[[i]] <- tibble(); next
    }
    out[[i]] <- df %>%
      filter(!is.na(.data$Result)) %>%
      group_by(.data$Date) %>%
      mutate(daily_ord = row_number()) %>%
      ungroup()
    
    Sys.sleep(runif(1, 0.5, 1))
  }
  bind_rows(out)
}


# ── ECF scraper for external override players ─────────────────────────────
parse_ecf_result <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_replace_all(x, "\u00bd", "1/2")
  
  case_when(
    x %in% c("1", "1-0", "+")          ~ 1,
    x %in% c("0", "0-1", "-")          ~ 0,
    x %in% c("1/2", "0.5", ".5", "=") ~ 0.5,
    TRUE                                 ~ suppressWarnings(as.numeric(x))
  )
}

scrape_ecf_games <- function(player_name, profile_url, from_date, run_date = RUN_DATE) {
  pg <- safe_html(profile_url)
  if (is.null(pg)) return(normalize_games_frame(tibble()))
  
  rows <- pg %>% html_elements("tr.game_row")
  if (!length(rows)) return(normalize_games_frame(tibble()))
  
  num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x)))
  
  out <- bind_rows(lapply(seq_along(rows), function(i) {
    tr <- rows[[i]]
    tds <- tr %>% html_elements("td")
    if (length(tds) < 8) return(NULL)
    
    date_order <- tds[[1]] %>% html_attr("data-order")
    date_txt <- tds[[1]] %>%
      html_text(trim = TRUE) %>%
      str_replace_all("\u00a0", " ") %>%
      str_squish()
    
    game_date <- suppressWarnings(lubridate::ymd(date_order))
    if (is.na(game_date)) {
      game_date <- suppressWarnings(lubridate::dmy(date_txt))
    }
    
    # ECF game rows are usually:
    # Date, colour/board marker, result, +/-, player rating, opponent, ...,
    # opponent revised rating, ECF category, federation, sex, ECF code, event.
    result_txt <- tds[[3]] %>% html_text(trim = TRUE)
    
    opp_node <- tr %>% html_element(xpath = ".//a[contains(@href, '/players/games')]")
    opponent <- if (!inherits(opp_node, "xml_missing") && !is.null(opp_node)) {
      opp_node %>% html_text(trim = TRUE) %>% str_squish()
    } else {
      NA_character_
    }
    
    rating_nodes <- tr %>% html_elements("td.revised_rating")
    opp_rating_txt <- if (length(rating_nodes)) {
      rating_nodes[[length(rating_nodes)]] %>% html_text(trim = TRUE)
    } else {
      NA_character_
    }
    
    tibble(
      source_row         = i,
      GameNo             = i,
      Date               = as.Date(game_date),
      Player1            = player_name,
      Player2            = opponent,
      OppKey             = NA_character_,
      OppRating_Display  = conv_ecf_to_wcu(num(opp_rating_txt)),
      RatingType         = "ECF",
      Result             = parse_ecf_result(result_txt),
      New                = NA_real_,
      Tot                = NA_real_,
      Exp                = NA_real_,
      Pts                = NA_real_,
      daily_ord          = NA_integer_
    )
    }))
  
  if (!nrow(out)) return(normalize_games_frame(tibble()))
  
  out %>%
    filter(
      !is.na(.data$Date),
      .data$Date >= as.Date(from_date),
      .data$Date <= as.Date(run_date),
      !is.na(.data$Result),
      !is.na(.data$Player2),
      nzchar(.data$Player2),
      !is.na(.data$OppRating_Display),
      .data$OppRating_Display >= 500
    ) %>%
    arrange(.data$Date, .data$source_row) %>%
    group_by(.data$Date) %>%
    mutate(daily_ord = row_number()) %>%
    ungroup() %>%
    mutate(GameNo = row_number()) %>%
    select(-source_row) %>%
    normalize_games_frame()
}

scrape_external_override_games <- function(overrides) {
  if (is.null(overrides) || !nrow(overrides)) {
    return(normalize_games_frame(tibble()))
  }
  
  bind_rows(lapply(seq_len(nrow(overrides)), function(i) {
    x <- overrides[i, ]
    
    if (!identical(x$source, "ECF")) {
      return(normalize_games_frame(tibble()))
    }
    
    message("ECF override scrape: ", x$name)
    
    scrape_ecf_games(
      player_name = x$name,
      profile_url = x$profile_url,
      from_date = x$from_date,
      run_date = RUN_DATE
    )
  })) %>%
    normalize_games_frame()
}

# ── Utility: choose half-start boundary ────────────────────────────────────
half_start_for <- function(d){
  if (is.na(d)) return(NA_Date_)
  if (month(d) <= 6) as.Date(paste0(year(d), "-01-01")) else as.Date(paste0(year(d), "-07-01"))
}

# ── Decide snapshot vs rebuild ────────────────────────────────────────────
snapshot <- NULL
if (is.null(CALC_FROM_DATE) && file.exists(SNAPSHOT_FILE)) {
  snapshot <- readRDS(SNAPSHOT_FILE)
  message("Loaded snapshot at cutoff: ", as.character(snapshot$cutoff))
}

effective_calc_start <- if (!is.null(snapshot)) {
  as.Date(snapshot$cutoff)
} else if (!is.null(CALC_FROM_DATE)) {
  as.Date(CALC_FROM_DATE)
} else {
  # No snapshot and no explicit calc date: default to start of CURRENT half
  modern_halves_current_start <- modern_halves$start_date[modern_halves$label == "CURRENT"]
  as.Date(modern_halves_current_start)
}

# Filter halves to those on/after the half containing effective_calc_start
half_defs <- half_defs_all %>%
  filter(label == "CURRENT")

# ── Cache paths keyed by label ────────────────────────────────────────────
players_csv <- setNames(file.path(cache_dir, paste0("players_", tolower(half_defs$label), ".csv")), half_defs$label)
games_csv   <- setNames(file.path(cache_dir, paste0("games_",   tolower(half_defs$label), ".csv")), half_defs$label)

# ── STATE HOLDERS ─────────────────────────────────────────────────────────
players_by_half <- list()
games_by_half   <- list()

# ── 1) SCRAPE/CACHE CURRENT (respects SCRAPE_FROM_DATE + FORCE_REFRESH) ──
{
  h <- "CURRENT"
  if (!h %in% half_defs$label) {
    message("Skipping CURRENT: not in selected halves for effective start.")
  } else {
    p_csv <- players_csv[[h]]
    g_csv <- games_csv[[h]]
    base  <- half_defs$base_url[half_defs$label == h]
    
    have_cache <- file.exists(p_csv) && file.exists(g_csv)
    
    if (!FORCE_REFRESH_CURRENT && have_cache) {
      players_by_half[[h]] <- read_csv(p_csv, col_types = players_col_types, show_col_types = FALSE)
      games_by_half[[h]]   <- read_csv(g_csv, col_types = games_col_types,   show_col_types = FALSE) %>%
        mutate(Date = lubridate::ymd(.data$Date))
    } else {
      if (is.null(SCRAPE_FROM_DATE) && !have_cache) {
        message("No CURRENT cache found; forcing a lightweight scrape of list only.")
      }
      prev_games <- if (file.exists(g_csv)) {
        read_csv(g_csv, col_types = games_col_types, show_col_types = FALSE)
      } else {
        tibble(
          GameNo            = integer(),
          Date              = as.Date(character()),
          Player1           = character(),
          Player2           = character(),
          OppKey            = character(),
          OppRating_Display = double(),
          RatingType        = character(),
          Result            = double(),
          New               = double(),
          Tot               = double(),
          Exp               = double(),
          Pts               = double(),
          daily_ord         = integer()
        )
      }
      
      players_now <- scrape_players(base)
      
      to_scrape <- {
        # 1) Explicit name override always wins
        if (length(FORCE_SCRAPE_NAMES)) {
          players_now %>%
            filter(name %in% FORCE_SCRAPE_NAMES)
          
          # 2) Existing behaviour
        } else if (is.null(SCRAPE_FROM_DATE)) {
          players_now[0, ]
          
        } else if (identical(SCRAPE_FROM_DATE, "all")) {
          players_now %>%
            { if ("Pld" %in% names(.)) filter(., !is.na(Pld) & Pld > 0) else . }
          
        } else {
          cutoff <- as.Date(SCRAPE_FROM_DATE)
          players_now %>%
            filter(!is.na(last_played)) %>%
            filter(last_played >= cutoff) %>%
            { if ("Pld" %in% names(.)) filter(., !is.na(Pld) & Pld > 0) else . }
        }
      }
      
      
      message(sprintf(
        "CURRENT: scraping %d of %d players (mode: %s)",
        nrow(to_scrape),
        nrow(players_now %>% filter(!is.na(last_played))),
        if (is.null(SCRAPE_FROM_DATE)) "no-scrape"
        else if (identical(SCRAPE_FROM_DATE, "all")) "all"
        else paste0("since ", format(as.Date(SCRAPE_FROM_DATE), "%Y-%m-%d"))
      ))
      
      # Scrape selected current players, but when a player has been scraped,
      # replace that player's cached games from effective_calc_start onwards.
      # SCRAPE_FROM_DATE decides who to scrape; effective_calc_start decides
      # how much of their rating-relevant game cache must be refreshed.
      new_games_raw <- if (nrow(to_scrape)) scrape_games(to_scrape) else prev_games[0, ]
      
      if (!is.null(SCRAPE_FROM_DATE) && !identical(SCRAPE_FROM_DATE, "all")) {
        replace_from <- as.Date(effective_calc_start)
        
        scraped_names <- new_games_raw %>%
          filter(!is.na(.data$Player1), nzchar(.data$Player1)) %>%
          distinct(.data$Player1) %>%
          pull(.data$Player1)
        
        if (length(scraped_names)) {
          prev_keep <- prev_games %>%
            mutate(Date = as.Date(.data$Date)) %>%
            filter(!(.data$Player1 %in% scraped_names & .data$Date >= replace_from))
          
          new_keep <- new_games_raw %>%
            mutate(Date = as.Date(.data$Date)) %>%
            filter(.data$Player1 %in% scraped_names, .data$Date >= replace_from)
          
          games_current <- bind_rows(prev_keep, new_keep) %>%
            arrange(.data$Player1, .data$Date, coalesce(.data$GameNo, 999999L)) %>%
            group_by(.data$Player1, .data$Date, .data$Player2, .data$GameNo) %>%
            slice_tail(n = 1) %>%
            ungroup()
        } else {
          games_current <- prev_games
        }
        
      } else if (identical(SCRAPE_FROM_DATE, "all")) {
        games_current <- new_games_raw
        
      } else {
        games_current <- prefer_new(
          prev_games,
          new_games_raw,
          c("Player1","Date","Player2","GameNo")
        )
      }
      
      games_current <- games_current %>%
        mutate(Date = lubridate::ymd(.data$Date)) %>%
        arrange(Player1, Date, GameNo)
      
      games_current <- games_current %>%
        filter(
          !is.na(OppRating_Display) |
            (!is.na(OppKey) & nzchar(OppKey))
        )
      
      
      
      write_csv(players_now, p_csv)
      write_csv(games_current, g_csv)
      players_by_half[[h]] <- players_now
      games_by_half[[h]]   <- games_current
    }
  }
}

players_current <- players_by_half[["CURRENT"]] %||% tibble()
if (!nrow(players_current)) message("Warning: no CURRENT players loaded.")

# ── 2) BACKFILL (other halves on/after effective_calc_start) ──────────────
players_by_half_all <- players_by_half
games_by_half_all   <- games_by_half

backfill_labels <- setdiff(half_defs$label, "CURRENT")
for (h in backfill_labels) {
  p_csv <- players_csv[[h]]
  g_csv <- games_csv[[h]]
  base  <- half_defs$base_url[half_defs$label == h]
  legacy <- half_defs$legacy[half_defs$label == h]
  
  if (file.exists(p_csv) && file.exists(g_csv)) {
    message("Backfill (cached): ", h)
    players_by_half_all[[h]] <- read_csv(p_csv, show_col_types = FALSE)
    games_by_half_all[[h]]   <- read_csv(g_csv, show_col_types = FALSE)
    next
  }
  
  message("Backfill (scraping): ", h)
  if (h == "2021H2") {
    zones <- c("E1","G1","D1","N1")
    dfs <- lapply(zones, function(z){
      scrape_players(paste0(base, "playerSummaryView.php?zone=", z))
    })
    df_p <- bind_rows(dfs) %>% distinct(player_key, .keep_all = TRUE)
  } else if (legacy) {
    df_p <- scrape_players_legacy(base, half_defs$players_file[half_defs$label == h])
  } else {
    df_p <- scrape_players(base)
  }
  df_p <- df_p %>%
    mutate(name_clean = str_squish(tolower(name))) %>%
    { if ("Pld" %in% names(.)) filter(., !is.na(Pld) & Pld > 0) else . }
  
  df_g <- if (nrow(df_p)) scrape_games(df_p) else tibble()
  write_csv(df_p, p_csv); write_csv(df_g, g_csv)
  players_by_half_all[[h]] <- df_p
  games_by_half_all[[h]]   <- df_g
}

# Normalise
players_by_half_all <- lapply(players_by_half_all, normalize_players_frame)
games_by_half_all   <- lapply(games_by_half_all,   normalize_games_frame)

# FULL WCU games spanning all loaded/cached halves, no date filter.
games_all_by_page_full_wcu <- bind_rows(games_by_half_all) %>%
  mutate(
    Date = as_date(.data$Date),
    RatingType = toupper(trimws(.data$RatingType))
  ) %>%
  filter(!is.na(.data$Result)) %>%
  filter(!is.na(.data$Date), .data$Date <= RUN_DATE) %>%
  arrange(.data$Player1, .data$Date, .data$GameNo) %>%
  mutate(Date = as.Date(.data$Date)) %>%
  normalize_games_frame()

# Existing exported games are used only as a fallback for override players.
# This preserves Welsh-only games that are not on ECF and may no longer be
# available from the short WCU scrape/cache window.

result_text_to_num <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    x == "Win"  ~ 1,
    x == "Draw" ~ 0.5,
    x == "Loss" ~ 0,
    TRUE        ~ suppressWarnings(as.numeric(x))
  )
}

read_existing_override_games_export <- function(overrides) {
  if (is.null(overrides) || !nrow(overrides)) {
    return(normalize_games_frame(tibble()))
  }
  
  bind_rows(lapply(seq_len(nrow(overrides)), function(i) {
    nm <- overrides$name[i]
    pid <- overrides$player_key[i]
    from_date <- as.Date(overrides$from_date[i])
    
    if (is.na(pid) || !nzchar(pid)) {
      return(normalize_games_frame(tibble()))
    }
    
    p <- file.path(data_dir, "games", paste0(pid, ".json"))
    if (!file.exists(p)) return(normalize_games_frame(tibble()))
    
    old <- tryCatch(
      jsonlite::read_json(p, simplifyVector = TRUE),
      error = function(e) NULL
    )
    
    if (is.null(old) || !is.data.frame(old) || !nrow(old)) {
      return(normalize_games_frame(tibble()))
    }
    
    old <- tibble::as_tibble(old)
    
    required <- c("date", "player", "opponent", "opponentElo", "result")
    if (!all(required %in% names(old))) {
      return(normalize_games_frame(tibble()))
    }
    
    old %>%
      mutate(Date = as.Date(.data$date)) %>%
      filter(
        .data$player == nm,
        !is.na(.data$Date),
        .data$Date >= from_date,
        .data$Date <= RUN_DATE
      ) %>%
      arrange(.data$Date) %>%
      transmute(
        GameNo             = row_number(),
        Date               = .data$Date,
        Player1            = as.character(.data$player),
        Player2            = as.character(.data$opponent),
        OppKey             = NA_character_,
        OppRating_Display  = suppressWarnings(as.numeric(.data$opponentElo)),
        RatingType         = "EXPORT_FALLBACK",
        Result             = result_text_to_num(.data$result),
        New                = NA_real_,
        Tot                = NA_real_,
        Exp                = NA_real_,
        Pts                = NA_real_,
        daily_ord          = NA_integer_
      ) %>%
      filter(
        !is.na(.data$Date),
        !is.na(.data$Player2),
        nzchar(.data$Player2),
        !is.na(.data$OppRating_Display),
        !is.na(.data$Result)
      ) %>%
      group_by(.data$Date) %>%
      mutate(daily_ord = row_number()) %>%
      ungroup() %>%
      normalize_games_frame()
  })) %>%
    normalize_games_frame()
}

# External rows are scraped fresh every run. For Dave this means the ECF page
# is treated as the truth on dates where it has games.
ecf_override_games <- scrape_external_override_games(external_rating_overrides)

# Existing exported games are used only as a fallback for override players.
# ECF dates win; fallback rows are kept only where ECF has no game on that date.
export_override_fallback_games <- read_existing_override_games_export(
  external_rating_overrides
)

ecf_override_dates <- ecf_override_games %>%
  distinct(.data$Player1, .data$Date) %>%
  mutate(.ecf_override_date = TRUE)


# override player. On these dates the ECF rows are used for that player's feed.
ecf_wcu_date_conflicts <- games_all_by_page_full_wcu %>%
  inner_join(
    ecf_override_dates %>% select(Player1, Date),
    by = c("Player1", "Date")
  ) %>%
  filter(.data$Player1 %in% external_override_names) %>%
  group_by(.data$Player1, .data$Date) %>%
  summarise(
    wcu_rows = n(),
    wcu_opponents = paste(unique(.data$Player2), collapse = "; "),
    .groups = "drop"
  ) %>%
  left_join(
    ecf_override_games %>%
      group_by(.data$Player1, .data$Date) %>%
      summarise(
        ecf_rows = n(),
        ecf_opponents = paste(unique(.data$Player2), collapse = "; "),
        .groups = "drop"
      ),
    by = c("Player1", "Date")
  )

if (nrow(ecf_wcu_date_conflicts)) {
  write_csv(
    ecf_wcu_date_conflicts,
    file.path(cache_dir, "ecf_wcu_date_conflicts.csv")
  )
}

# Merge WCU and ECF feeds.
# For override players after from_date:
#   - ECF dates win.
#   - WCU-only dates are kept.
#   - Export fallback is used only where neither ECF nor WCU has that game.
#   - WCU before from_date is untouched.
wcu_kept_for_rating <- games_all_by_page_full_wcu %>%
  left_join(
    external_rating_overrides %>%
      filter(.data$source == "ECF") %>%
      select(Player1 = name, from_date),
    by = "Player1"
  ) %>%
  left_join(
    ecf_override_dates,
    by = c("Player1", "Date")
  ) %>%
  filter(
    is.na(.data$from_date) |
      .data$Date < .data$from_date |
      is.na(.data$.ecf_override_date)
  ) %>%
  select(-any_of(c("from_date", ".ecf_override_date")))

wcu_override_keys <- wcu_kept_for_rating %>%
  filter(.data$Player1 %in% external_override_names) %>%
  mutate(
    opponent_norm = str_squish(str_to_lower(.data$Player2))
  ) %>%
  distinct(.data$Player1, .data$Date, .data$opponent_norm) %>%
  mutate(.wcu_override_key = TRUE)

fallback_kept_for_rating <- export_override_fallback_games %>%
  mutate(
    opponent_norm = str_squish(str_to_lower(.data$Player2))
  ) %>%
  left_join(
    ecf_override_dates,
    by = c("Player1", "Date")
  ) %>%
  left_join(
    wcu_override_keys,
    by = c("Player1", "Date", "opponent_norm")
  ) %>%
  filter(
    is.na(.data$.ecf_override_date),
    is.na(.data$.wcu_override_key)
  ) %>%
  select(-any_of(c(".ecf_override_date", ".wcu_override_key", "opponent_norm")))

games_all_by_page_full <- bind_rows(
  wcu_kept_for_rating,
  fallback_kept_for_rating,
  ecf_override_games
) %>%
  normalize_games_frame() %>%
  mutate(
    Date = as.Date(.data$Date),
    RatingType = toupper(trimws(.data$RatingType))
  ) %>%
  filter(!is.na(.data$Result)) %>%
  filter(!is.na(.data$Date), .data$Date <= RUN_DATE) %>%
  arrange(.data$Player1, .data$Date, coalesce(.data$GameNo, 999999L))

filter_games_for_rating_window <- function(df) {
  df %>%
    left_join(
      external_rating_overrides %>%
        select(Player1 = name, calc_from = from_date),
      by = "Player1"
    ) %>%
    mutate(calc_from = coalesce(.data$calc_from, as.Date(effective_calc_start))) %>%
    filter(.data$Date >= .data$calc_from) %>%
    select(-calc_from)
}

# Engine set: normal current window for everyone, but a longer player-specific
# window for external override players.
games_all_by_page <- filter_games_for_rating_window(games_all_by_page_full)

# Who has played since EXPORT_ACTIVE_SINCE, across ALL halves (cached)
active_names <- games_all_by_page_full %>%
  mutate(Date = as.Date(.data$Date)) %>%
  filter(!is.na(.data$Date)) %>%
  group_by(.data$Player1) %>%
  summarise(last_game = max(.data$Date, na.rm = TRUE), .groups = "drop") %>%
  filter(.data$last_game >= EXPORT_ACTIVE_SINCE) %>%
  pull(.data$Player1) %>%
  unique()

current_names <- (players_by_half_all[["CURRENT"]] %||% tibble(name=character(), last_played=as.Date(character()))) %>%
  mutate(last_played = as.Date(.data$last_played)) %>%
  filter(!is.na(.data$last_played), .data$last_played >= EXPORT_ACTIVE_SINCE) %>%
  pull(name) %>%
  unique()

keep_names <- union(active_names, current_names)
keep_names <- union(keep_names, external_override_names)



# ── 3) Build cohort & baselines ───────────────────────────────────────────
if (!is.null(snapshot) && is.null(CALC_FROM_DATE)) {
  # Continue from snapshot
  players_master <- snapshot$players_master
} else {
  # Rebuild from effective_calc_start
  
  
  player_catalogue <- bind_rows(lapply(names(players_by_half_all), function(h){
    df <- players_by_half_all[[h]]
    if (!nrow(df)) return(tibble())
    df %>%
      mutate(half = h, half_start = half_defs$start_date[match(h, half_defs$label)]) %>%
      select(name, zone, club, player_key, grade, half, half_start)
  }))
  
  latest_meta <- player_catalogue %>%
    filter(name %in% keep_names) %>%
    arrange(name, desc(.data$half_start)) %>%
    group_by(name) %>% slice_head(n = 1) %>% ungroup() %>%
    transmute(name, zone, club, player_key)
  
  first_points <- bind_rows(games_by_half_all) %>%
    mutate(Date = as.Date(.data$Date)) %>%
    filter(.data$Player1 %in% keep_names, !is.na(.data$Date)) %>%
    filter(.data$Date >= effective_calc_start) %>%
    arrange(.data$Player1, .data$Date, coalesce(.data$GameNo, 999999L)) %>%
    group_by(.data$Player1) %>% slice_head(n = 1) %>% ungroup() %>%
    transmute(
      name           = .data$Player1,
      baseline_date  = as.Date(.data$Date),
      baseline_grade = if_else(as.Date(.data$Date) < PRE_2024_CUTOFF,
                               conv_pre_2024(.data$New), .data$New)
    )
  
  fallback_baseline <- player_catalogue %>%
    mutate(half_start = as.Date(.data$half_start)) %>%
    filter(.data$half_start <= effective_calc_start) %>%
    arrange(name, desc(.data$half_start)) %>%
    group_by(name) %>% slice_head(n = 1) %>% ungroup() %>%
    transmute(
      name,
      baseline_date  = as.Date(.data$half_start),
      baseline_grade = if_else(.data$half_start < PRE_2024_CUTOFF, conv_pre_2024(.data$grade), .data$grade)
    )
  
  players_master <- first_points %>%
    bind_rows(anti_join(fallback_baseline, first_points, by = "name")) %>%
    left_join(latest_meta, by = "name") %>%
    mutate(player_key = coalesce(player_key, player_catalogue$player_key[match(name, player_catalogue$name)])) %>%
    select(name, zone, club, player_key, baseline_grade, baseline_date) %>%
    filter(!is.na(baseline_grade), !is.na(baseline_date))
}


# ── 3b) Seed baselines from existing J-history to avoid July-1 jump ───────
read_hist_on_or_before <- function(pid, date_limit) {
  p <- file.path(hist_dir, paste0(pid, ".json"))
  if (!file.exists(p)) return(NULL)
  
  df <- tryCatch(
    jsonlite::read_json(p, simplifyVector = TRUE),
    error = function(e) NULL
  )
  
  if (
    is.null(df) ||
    !is.data.frame(df) ||
    !nrow(df) ||
    !all(c("date", "rating") %in% names(df))
  ) {
    return(NULL)
  }
  
  df <- tibble::as_tibble(df)
  suppressWarnings(df$date <- as.Date(df$date))
  
  df <- df %>%
    filter(!is.na(.data$date), .data$date <= as.Date(date_limit)) %>%
    arrange(.data$date)
  
  if (!nrow(df)) return(NULL)
  
  list(
    rating = as.numeric(df$rating[nrow(df)]),
    date   = as.Date(df$date[nrow(df)])
  )
}

if (!is.null(CALC_FROM_DATE)) {
  target_seed_date <- as.Date(CALC_FROM_DATE) - 1
  
  seed_rating <- rep(NA_real_, nrow(players_master))
  seed_date   <- rep(as.Date(NA), nrow(players_master))
  
  for (i in seq_len(nrow(players_master))) {
    pid <- players_master$player_key[i]
    if (is.na(pid) || !nzchar(pid)) next
    
    s <- read_hist_on_or_before(pid, target_seed_date)
    
    if (!is.null(s)) {
      seed_rating[i] <- s$rating
      seed_date[i]   <- s$date
    }
  }
  
  players_master <- players_master %>%
    mutate(
      baseline_grade = dplyr::coalesce(seed_rating, baseline_grade),
      baseline_date  = dplyr::coalesce(seed_date,   baseline_date)
    )
}

# External override players get their own older seed date.
# Dave is seeded from the last stored J-Rating on or before 2025-01-31,
# then rebuilt from 2025-02-01 onwards.
if (nrow(external_rating_overrides)) {
  for (i in seq_len(nrow(external_rating_overrides))) {
    nm <- external_rating_overrides$name[i]
    from_date <- as.Date(external_rating_overrides$from_date[i])
    
    idx <- match(nm, players_master$name)
    if (is.na(idx)) {
      warning("External override player not found in players_master: ", nm)
      next
    }
    
    pid <- players_master$player_key[idx]
    seed <- read_hist_on_or_before(pid, from_date - 1)
    
    if (is.null(seed)) {
      old_games_path <- file.path(data_dir, "games", paste0(pid, ".json"))
      
      old_games <- tryCatch(
        jsonlite::read_json(old_games_path, simplifyVector = TRUE),
        error = function(e) NULL
      )
      
      if (
        is.data.frame(old_games) &&
        nrow(old_games) &&
        all(c("date", "player", "playerElo") %in% names(old_games))
      ) {
        old_games <- tibble::as_tibble(old_games) %>%
          mutate(date = as.Date(.data$date)) %>%
          filter(
            .data$player == nm,
            !is.na(.data$date),
            .data$date >= from_date
          ) %>%
          arrange(.data$date)
        
        if (nrow(old_games)) {
          seed <- list(
            rating = as.numeric(old_games$playerElo[[1]]),
            date   = from_date - 1
          )
        }
      }
    }
    
    if (is.null(seed)) {
      warning(
        "Cannot find existing J-history seed or old games seed for ", nm,
        " on or before ",
        format(from_date - 1, "%Y-%m-%d"),
        ". Falling back to baseline_grade at ",
        format(from_date - 1, "%Y-%m-%d"),
        "."
      )
      
      seed <- list(
        rating = players_master$baseline_grade[idx],
        date   = from_date - 1
      )
    }
    
    players_master$baseline_grade[idx] <- seed$rating
    players_master$baseline_date[idx]  <- seed$date
  }
}

# ── 4) Union games and trim to start ──────────────────────────────────────
# games_all_by_page_full and games_all_by_page have already been built above
# from the merged WCU/ECF feed. Reapply the window here so later code keeps a
# single source of truth.
games_all_by_page <- filter_games_for_rating_window(games_all_by_page_full)

pkey_lookup <- setNames(players_master$player_key, players_master$name)

# Exclude specific WCU entries (player, date, opponent)
wcu_exclusions <- tribble(
  ~Player1,       ~Date,              ~Opponent,
  "Sam Jukes",    as.Date("2025-10-22"), "Jai Hrithvik Adithya Are"
)

wcu_points_source <- filter_games_for_rating_window(games_all_by_page_full_wcu)

wcu_points <- wcu_points_source %>%
  filter(!is.na(.data$Date), .data$Date <= RUN_DATE, !is.na(.data$New)) %>%
  # Drop the specific bad WCU update
  anti_join(
    wcu_exclusions,
    by = c("Player1" = "Player1", "Date" = "Date", "Player2" = "Opponent")
  ) %>%
  arrange(.data$Player1, .data$Date, coalesce(.data$GameNo, 999999L)) %>%
  group_by(.data$Player1, .data$Date) %>%
  summarise(raw = last(.data$New), .groups = "drop") %>%
  mutate(
    rating = if_else(
      .data$Date < PRE_2024_CUTOFF,
      conv_pre_2024(.data$raw),
      .data$raw
    )
  ) %>%
  select(Player1, Date, rating)


games <- games_all_by_page %>%
  mutate(
    P1Key   = pkey_lookup[.data$Player1],
    P2Key   = coalesce(.data$OppKey, pkey_lookup[.data$Player2]),
    PairKey = case_when(
      !is.na(P1Key) & !is.na(P2Key) ~ paste(pmin(P1Key, P2Key), pmax(P1Key, P2Key), sep = "|"),
      TRUE ~ paste(pmin(.data$Player1, .data$Player2), pmax(.data$Player1, .data$Player2), sep = "|")
    )
  ) %>%
  filter(!is.na(.data$Date)) %>%
  arrange(.data$Date, .data$PairKey, .data$GameNo) %>%
  distinct(.data$Date, .data$PairKey, .keep_all = TRUE) %>%
  mutate(Date = as.Date(.data$Date))

# ── 5) Rating engine seed ─────────────────────────────────────────────────
if (!is.null(snapshot) && is.null(CALC_FROM_DATE)) {
  current_ratings <- snapshot$current_ratings
  last_checked    <- snapshot$last_checked
  baseline_of     <- snapshot$baseline_of
  # ensure all players present
  missing <- setdiff(players_master$name, names(current_ratings))
  if (length(missing)) {
    current_ratings[missing] <- players_master$baseline_grade[match(missing, players_master$name)]
    last_checked[missing]    <- players_master$baseline_date[match(missing, players_master$name)]
    baseline_of[missing]     <- players_master$baseline_date[match(missing, players_master$name)]
  }
} else {
  current_ratings <- setNames(players_master$baseline_grade, players_master$name)
  players_master$baseline_date <- as.Date(players_master$baseline_date)
  last_checked <- setNames(players_master$baseline_date, players_master$name)
  baseline_of  <- setNames(players_master$baseline_date,  players_master$name)
}

apply_snaps_until <- function(player, upto_date){
  if (is.na(upto_date)) return(invisible())
  lc <- last_checked[player]
  if (is.na(lc)) { last_checked[player] <<- upto_date; return(invisible()) }
  sdates <- snap_dates_between(lc, upto_date)
  if (length(sdates)){
    if (!is.na(current_ratings[player]) && current_ratings[player] < 1310){
      current_ratings[player] <<- 1310
    }
  }
  last_checked[player] <<- upto_date
}

# ── 6) Rating pass ────────────────────────────────────────────────────────
game_history <- tibble(
  GameIndex      = integer(),
  Date           = as.Date(character()),
  Player1        = character(),
  Player2        = character(),
  Rating1_Before = double(),
  Rating2_Before = double(),
  K1             = double(),
  K2             = double(),
  Rating1_After  = double(),
  Rating2_After  = double(),
  Result         = double(),
  Note           = character()
)

for (i in seq_len(nrow(games))){
  g  <- games[i, ]
  p1 <- g$Player1
  p2_txt <- g$Player2
  res <- g$Result
  if (is.na(g$Date)) next
  if (!nzchar(p1) || is.na(current_ratings[p1]) || length(res) != 1 || is.na(res)) next
  b1 <- baseline_of[p1]
  if (!is.na(b1) && g$Date < b1) next
  apply_snaps_until(p1, g$Date)
  Ra <- current_ratings[p1]
  opponent_is_welsh <- is_welsh_type(g$RatingType)
  
  p2_name_live <- NA_character_
  if (opponent_is_welsh && !is.na(g$OppKey)){
    idx <- which(players_master$player_key == g$OppKey)
    if (length(idx) == 1) p2_name_live <- players_master$name[idx]
  }
  
  used_live_for_p2 <- FALSE
  Rb <- NA_real_
  
  if (opponent_is_welsh && !is.na(p2_name_live) && !is.na(current_ratings[p2_name_live])){
    b2 <- baseline_of[p2_name_live]
    if (!is.na(b2) && g$Date >= b2){
      apply_snaps_until(p2_name_live, g$Date)
      Rb <- current_ratings[p2_name_live]
      used_live_for_p2 <- TRUE
    }
  }
  if (!used_live_for_p2){
    Rb <- g$OppRating_Display
  }
  
  # HARD GUARD: drop bogus/missing opponent ratings
  if (!is.na(Rb) && Rb < 500) Rb <- NA_real_
  
  if (is.na(Rb)) next
  
  k1 <- pick_k(Ra)
  k2 <- if (used_live_for_p2) pick_k(Rb) else NA_real_
  
  if (used_live_for_p2){
    Ra_new <- update_elo(Ra, Rb, res, k = k1)
    Rb_new <- update_elo(Rb, Ra, 1 - res, k = k2)
  } else {
    Ra_new <- update_elo(Ra, Rb, res, k = k1)
    Rb_new <- NA_real_
  }
  
  game_history <- bind_rows(game_history, tibble(
    GameIndex      = i,
    Date           = g$Date,
    Player1        = p1,
    Player2        = if (!is.na(p2_name_live)) p2_name_live else p2_txt,
    Rating1_Before = Ra,
    Rating2_Before = Rb,
    K1             = k1,
    K2             = k2,
    Rating1_After  = Ra_new,
    Rating2_After  = if (used_live_for_p2) Rb_new else NA_real_,
    Result         = res,
    Note           = if (used_live_for_p2) "WLS–WLS (live vs live)" else paste0("vs ", g$RatingType, " (display)")
  ))
  
  current_ratings[p1] <- Ra_new
  if (used_live_for_p2 && !is.na(p2_name_live)) current_ratings[p2_name_live] <- Rb_new
}

# ── 7) Daily series & exports (safe with empties) ─────────────────────────
# ── 7) Daily series & exports from one source of truth ────────────────────
# Source of truth:
#   game_history
#
# Everything below is derived from that:
#   - players.json live/peak
#   - data/history/<id>.json graph
#   - data/games/<id>.json games table
#
# Old JSON is kept before each player's rebuild start.
# For normal players that is effective_calc_start.
# For external override players such as Dave McGhee, it is their from_date.

players_with_games <- union(game_history$Player1, game_history$Player2)
players_with_games <- players_with_games[!is.na(players_with_games) & players_with_games != ""]

safe_expected_wdl <- function(player_elo, opponent_elo) {
  if (is.na(player_elo) || is.na(opponent_elo)) {
    return(tibble(
      win_prob  = NA_real_,
      draw_prob = NA_real_,
      loss_prob = NA_real_
    ))
  }
  
  expected_wdl_from_elo(player_elo, opponent_elo)
}

normalise_games_export <- function(df) {
  empty <- tibble(
    gi          = integer(),
    date        = character(),
    player      = character(),
    playerId    = character(),
    playerElo   = integer(),
    opponent    = character(),
    opponentId  = character(),
    opponentElo = integer(),
    result      = character(),
    winPct      = integer(),
    drawPct     = integer(),
    lossPct     = integer(),
    delta       = character(),
    new         = integer()
  )
  
  if (is.null(df) || !nrow(df)) return(empty)
  
  must <- names(empty)
  
  for (m in must) {
    if (!m %in% names(df)) df[[m]] <- NA
  }
  
  df %>%
    transmute(
      gi          = suppressWarnings(as.integer(.data$gi)),
      date        = format(as.Date(.data$date), "%Y-%m-%d"),
      player      = as.character(.data$player),
      playerId    = as.character(.data$playerId),
      playerElo   = suppressWarnings(as.integer(round(as.numeric(.data$playerElo)))),
      opponent    = as.character(.data$opponent),
      opponentId  = as.character(.data$opponentId),
      opponentElo = suppressWarnings(as.integer(round(as.numeric(.data$opponentElo)))),
      result      = as.character(.data$result),
      winPct      = suppressWarnings(as.integer(.data$winPct)),
      drawPct     = suppressWarnings(as.integer(.data$drawPct)),
      lossPct     = suppressWarnings(as.integer(.data$lossPct)),
      delta       = as.character(.data$delta),
      new         = suppressWarnings(as.integer(round(as.numeric(.data$new))))
    )
}

# ── 7a) Build one per-game export table directly from game_history ────────

p1_view <- game_history %>%
  transmute(
    gi             = .data$GameIndex,
    date           = as.Date(.data$Date),
    player         = .data$Player1,
    playerId       = as.character(pkey_lookup[.data$Player1]),
    playerEloRaw   = .data$Rating1_Before,
    playerNewRaw   = .data$Rating1_After,
    opponent       = .data$Player2,
    opponentId     = as.character(pkey_lookup[.data$Player2]),
    opponentEloRaw = .data$Rating2_Before,
    result_num     = .data$Result
  )

p2_view <- game_history %>%
  filter(!is.na(.data$Rating2_Before), !is.na(.data$Rating2_After)) %>%
  transmute(
    gi             = .data$GameIndex,
    date           = as.Date(.data$Date),
    player         = .data$Player2,
    playerId       = as.character(pkey_lookup[.data$Player2]),
    playerEloRaw   = .data$Rating2_Before,
    playerNewRaw   = .data$Rating2_After,
    opponent       = .data$Player1,
    opponentId     = as.character(pkey_lookup[.data$Player1]),
    opponentEloRaw = .data$Rating1_Before,
    result_num     = 1 - .data$Result
  )

games_long <- bind_rows(p1_view, p2_view) %>%
  filter(!is.na(.data$date), !is.na(.data$player), nzchar(.data$player)) %>%
  arrange(.data$date, .data$gi, .data$player, .data$opponent) %>%
  mutate(
    result = case_when(
      is.na(.data$result_num)  ~ NA_character_,
      .data$result_num >= 0.75 ~ "Win",
      .data$result_num <= 0.25 ~ "Loss",
      TRUE                     ~ "Draw"
    ),
    
    delta_num = .data$playerNewRaw - .data$playerEloRaw,
    
    wdl = purrr::map2(.data$playerEloRaw, .data$opponentEloRaw, safe_expected_wdl),
    
    winPct  = as.integer(round(purrr::map_dbl(.data$wdl, ~ .x$win_prob[[1]])  * 100)),
    drawPct = as.integer(round(purrr::map_dbl(.data$wdl, ~ .x$draw_prob[[1]]) * 100)),
    lossPct = as.integer(round(purrr::map_dbl(.data$wdl, ~ .x$loss_prob[[1]]) * 100)),
    
    delta = ifelse(
      !is.na(.data$delta_num),
      sprintf("%+0.1f", round(.data$delta_num, 1)),
      NA_character_
    ),
    
    playerElo   = as.integer(round(.data$playerEloRaw)),
    opponentElo = as.integer(round(.data$opponentEloRaw)),
    new         = as.integer(round(.data$playerNewRaw)),
    date        = format(.data$date, "%Y-%m-%d")
  ) %>%
  select(
    gi,
    date,
    player,
    playerId,
    playerElo,
    opponent,
    opponentId,
    opponentElo,
    result,
    winPct,
    drawPct,
    lossPct,
    delta,
    new
  ) %>%
  normalise_games_export()

# ── 7b) Build daily graph series from the same game_history chain ─────────

p1_updates <- game_history %>%
  filter(.data$Player1 %in% players_with_games) %>%
  transmute(
    Date        = as.Date(.data$Date),
    player      = .data$Player1,
    rating      = .data$Rating1_After,
    event_order = as.integer(.data$GameIndex)
  )

p2_updates <- game_history %>%
  filter(.data$Player2 %in% players_with_games, !is.na(.data$Rating2_After)) %>%
  transmute(
    Date        = as.Date(.data$Date),
    player      = .data$Player2,
    rating      = .data$Rating2_After,
    event_order = as.integer(.data$GameIndex)
  )

core_updates <- bind_rows(p1_updates, p2_updates) %>%
  filter(!is.na(.data$Date), .data$Date <= RUN_DATE) %>%
  arrange(.data$player, .data$Date, .data$event_order)

snap_events <- tibble(
  Date        = as.Date(character()),
  player      = character(),
  rating      = double(),
  event_order = integer()
)

if (nrow(core_updates)) {
  snap_events <- bind_rows(lapply(unique(core_updates$player), function(pl) {
    u <- core_updates %>%
      filter(.data$player == pl) %>%
      arrange(.data$Date, .data$event_order)
    
    if (!nrow(u)) return(NULL)
    
    dmin <- min(u$Date, na.rm = TRUE)
    horizon <- RUN_DATE
    
    yrs <- seq(year(dmin), year(horizon), by = 1)
    sdates <- sort(as.Date(c(paste0(yrs, "-01-01"), paste0(yrs, "-07-01"))))
    
    out <- lapply(sdates, function(s) {
      prior <- u %>% filter(.data$Date <= s)
      
      if (!nrow(prior)) return(NULL)
      
      r_prev <- prior$rating[nrow(prior)]
      
      if (!is.na(r_prev) && r_prev < 1310) {
        tibble(
          Date        = s,
          player      = pl,
          rating      = 1310,
          event_order = 0L
        )
      } else {
        NULL
      }
    })
    
    bind_rows(out)
  }))
}

all_updates <- bind_rows(core_updates, snap_events) %>%
  filter(!is.na(.data$Date), !is.na(.data$player), !is.na(.data$rating)) %>%
  arrange(.data$player, .data$Date, .data$event_order)

daily_updates <- all_updates %>%
  arrange(.data$player, .data$Date, .data$event_order) %>%
  group_by(.data$player, .data$Date) %>%
  summarise(rating = last(.data$rating), .groups = "drop")

if (!nrow(daily_updates)) {
  ratings_daily <- tibble(
    player = character(),
    Date   = as.Date(character()),
    rating = double()
  )
} else {
  ratings_daily <- daily_updates %>%
    group_by(.data$player) %>%
    arrange(.data$Date) %>%
    complete(Date = seq(min(.data$Date), RUN_DATE, by = "day")) %>%
    tidyr::fill(.data$rating, .direction = "down") %>%
    filter(!is.na(.data$rating)) %>%
    ungroup()
}

ratings_wide_daily <- ratings_daily %>%
  group_by(.data$player, .data$Date) %>%
  summarise(rating = last(.data$rating), .groups = "drop") %>%
  pivot_wider(names_from = .data$player, values_from = .data$rating) %>%
  mutate(across(-Date, ~ round(.x, 0)))

write_csv(ratings_wide_daily, file.path(cache_dir, "ratings_wide_daily.csv"))

# Snap through today for live values.
for (nm in names(current_ratings)) {
  apply_snaps_until(nm, RUN_DATE)
}

live_from_engine <- tibble(
  name = names(current_ratings),
  rating_engine = round(as.numeric(current_ratings))
)

live_from_series <- if (nrow(ratings_daily)) {
  ratings_daily %>%
    group_by(.data$player) %>%
    arrange(.data$Date) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    transmute(player = .data$player, rating = round(.data$rating))
} else {
  tibble(player = character(), rating = double())
}

peak_from_series <- if (nrow(ratings_daily)) {
  ratings_daily %>%
    group_by(.data$player) %>%
    summarise(run_peak = max(.data$rating, na.rm = TRUE), .groups = "drop")
} else {
  tibble(player = character(), run_peak = double())
}

# Existing stored peak from players.json.
existing_players_path <- file.path(data_dir, "players.json")

existing_peak <- if (file.exists(existing_players_path)) {
  tryCatch(
    jsonlite::read_json(existing_players_path, simplifyVector = TRUE) %>%
      tibble::as_tibble() %>%
      transmute(
        id = as.character(.data$id),
        existing_peak = suppressWarnings(as.numeric(.data$peak))
      ),
    error = function(e) tibble(id = character(), existing_peak = double())
  )
} else {
  tibble(id = character(), existing_peak = double())
}

recent_names <- keep_names

players_json <- players_master %>%
  filter(.data$name %in% recent_names) %>%
  mutate(id = .data$player_key) %>%
  left_join(live_from_series, by = c("name" = "player")) %>%
  left_join(live_from_engine, by = "name") %>%
  left_join(peak_from_series, by = c("name" = "player")) %>%
  left_join(existing_peak, by = "id") %>%
  mutate(
    rating = coalesce(.data$rating, .data$rating_engine, round(.data$baseline_grade)),
    peak   = pmax(.data$existing_peak, .data$run_peak, .data$rating, na.rm = TRUE),
    k      = pick_k(.data$rating)
  ) %>%
  select(id, name, zone, club, rating, peak, k) %>%
  arrange(desc(.data$rating))

if (nrow(players_json)) {
  jsonlite::write_json(
    players_json,
    file.path(data_dir, "players.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
} else {
  message("No players_json rows; skipping players.json write to avoid wiping with empty content.")
}

name_to_id <- setNames(players_json$id %||% character(), players_json$name %||% character())

# ── 7c) WCU orange history export ─────────────────────────────────────────

wcu_wrote <- 0L
valid_names <- intersect(unique(wcu_points$Player1), names(name_to_id))

for (nm in valid_names) {
  pid <- unname(name_to_id[nm])
  
  df_new <- wcu_points %>%
    filter(.data$Player1 == nm) %>%
    arrange(.data$Date) %>%
    transmute(Date = as.Date(.data$Date), rating = as.numeric(.data$rating))
  
  old_path <- file.path(wcu_hist_dir, paste0(pid, "_wcu.json"))
  
  if (file.exists(old_path)) {
    old <- tryCatch(
      jsonlite::read_json(old_path, simplifyVector = TRUE),
      error = function(e) NULL
    )
    
    if (is.data.frame(old) && nrow(old)) {
      df_old <- tibble(
        Date   = as.Date(old$date),
        rating = as.numeric(old$rating)
      ) %>%
        filter(!is.na(.data$Date), .data$Date <= RUN_DATE)
      
      df_new <- bind_rows(df_old, df_new) %>%
        arrange(.data$Date) %>%
        group_by(.data$Date) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
  }
  
  if (nrow(df_new)) {
    df_out <- df_new %>%
      filter(!is.na(.data$Date), .data$Date <= RUN_DATE) %>%
      complete(Date = seq(min(.data$Date), RUN_DATE, by = "day")) %>%
      tidyr::fill(.data$rating, .direction = "down") %>%
      filter(!is.na(.data$rating)) %>%
      transmute(
        date   = format(.data$Date, "%Y-%m-%d"),
        rating = as.integer(round(.data$rating))
      )
    
    if (nrow(df_out)) {
      jsonlite::write_json(df_out, old_path, auto_unbox = TRUE, na = "null")
      wcu_wrote <- wcu_wrote + 1L
    }
  }
}

# ── 7d) J-Ratings blue history export from the same daily chain ───────────
# Keep old history before each player's rebuild start.
# From that date onwards, write ratings_daily only.

j_wrote <- 0L

if (nrow(players_json)) {
  for (i in seq_len(nrow(players_json))) {
    pid <- players_json$id[i]
    nm  <- players_json$name[i]
    rebuild_from <- player_rebuild_start(nm)
    
    out_path <- file.path(hist_dir, paste0(pid, ".json"))
    
    j_old <- tibble(date = as.Date(character()), rating = double())
    
    if (file.exists(out_path)) {
      old <- tryCatch(
        jsonlite::read_json(out_path, simplifyVector = TRUE),
        error = function(e) NULL
      )
      
      if (is.data.frame(old) && nrow(old) && all(c("date", "rating") %in% names(old))) {
        j_old <- tibble(
          date   = as.Date(old$date),
          rating = as.numeric(old$rating)
        ) %>%
          filter(
            !is.na(.data$date),
            !is.na(.data$rating),
            .data$date < rebuild_from,
            .data$date <= RUN_DATE
          ) %>%
          arrange(.data$date)
      }
    }
    
    j_new <- ratings_daily %>%
      filter(.data$player == nm) %>%
      transmute(
        date   = as.Date(.data$Date),
        rating = as.numeric(round(.data$rating, 0))
      ) %>%
      filter(
        !is.na(.data$date),
        !is.na(.data$rating),
        .data$date >= rebuild_from,
        .data$date <= RUN_DATE
      ) %>%
      arrange(.data$date)
    
    if (!nrow(j_old) && !nrow(j_new)) {
      seed_date <- as.Date(baseline_of[nm])
      
      if (is.na(seed_date) || seed_date > RUN_DATE) {
        seed_date <- RUN_DATE
      }
      
      seed_rating <- as.numeric(current_ratings[nm])
      
      if (is.na(seed_rating)) {
        seed_rating <- players_master$baseline_grade[match(nm, players_master$name)]
      }
      
      j_combined <- tibble(
        date   = seed_date,
        rating = round(seed_rating, 0)
      )
    } else {
      j_combined <- bind_rows(j_old, j_new) %>%
        filter(!is.na(.data$date), !is.na(.data$rating), .data$date <= RUN_DATE) %>%
        arrange(.data$date) %>%
        group_by(.data$date) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
    
    if (nrow(j_combined)) {
      j_df <- j_combined %>%
        complete(date = seq(min(.data$date), RUN_DATE, by = "day")) %>%
        tidyr::fill(.data$rating, .direction = "down") %>%
        filter(!is.na(.data$rating)) %>%
        transmute(
          date   = format(.data$date, "%Y-%m-%d"),
          rating = as.numeric(round(.data$rating, 0))
        )
      
      jsonlite::write_json(j_df, out_path, auto_unbox = TRUE, na = "null")
      j_wrote <- j_wrote + 1L
    }
  }
}

# ── 7e) Per-player games export from the same game_history chain ──────────
# Keep old games before each player's rebuild start.
# From that date onwards, write games_long only.

games_out_dir <- file.path(data_dir, "games")
dir.create(games_out_dir, recursive = TRUE, showWarnings = FALSE)

games_written <- 0L

if (nrow(players_json)) {
  for (nm in names(name_to_id)) {
    pid <- name_to_id[[nm]]
    out_path <- file.path(games_out_dir, paste0(pid, ".json"))
    rebuild_from <- player_rebuild_start(nm)
    
    new_rows <- games_long %>%
      filter(.data$player == nm) %>%
      normalise_games_export() %>%
      mutate(date_d = as.Date(.data$date)) %>%
      filter(!is.na(.data$date_d), .data$date_d >= rebuild_from) %>%
      select(-date_d)
    
    old_keep <- tibble(
      gi          = integer(),
      date        = character(),
      player      = character(),
      playerId    = character(),
      playerElo   = integer(),
      opponent    = character(),
      opponentId  = character(),
      opponentElo = integer(),
      result      = character(),
      winPct      = integer(),
      drawPct     = integer(),
      lossPct     = integer(),
      delta       = character(),
      new         = integer()
    )
    
    if (file.exists(out_path)) {
      old <- tryCatch(
        jsonlite::read_json(out_path, simplifyVector = TRUE),
        error = function(e) NULL
      )
      
      if (is.data.frame(old) && nrow(old)) {
        old_keep <- as_tibble(old) %>%
          normalise_games_export() %>%
          mutate(date_d = as.Date(.data$date)) %>%
          filter(!is.na(.data$date_d), .data$date_d < rebuild_from) %>%
          select(-date_d)
      }
    }
    
    out_rows <- bind_rows(old_keep, new_rows) %>%
      normalise_games_export() %>%
      mutate(date_d = as.Date(.data$date)) %>%
      arrange(desc(.data$date_d), desc(.data$gi)) %>%
      select(-date_d)
    
    if (nrow(out_rows)) {
      jsonlite::write_json(out_rows, out_path, auto_unbox = TRUE, na = "null")
      games_written <- games_written + 1L
    }
  }
}
# ── 8) Snapshot for future fast runs ──────────────────────────────────────
if (WRITE_SNAPSHOT) {
  snap_list <- list(
    cutoff          = Sys.Date(),
    players_master  = players_master,
    current_ratings = current_ratings,
    last_checked    = last_checked,
    baseline_of     = baseline_of
  )
  saveRDS(snap_list, SNAPSHOT_FILE)
  message("Snapshot written: ", SNAPSHOT_FILE, "  (cutoff=", as.character(snap_list$cutoff), ")")
}

# ── Diff report (optional) ────────────────────────────────────────────────
get_rating_on_or_before <- function(path, date_in){
  if (!file.exists(path)) return(NA_real_)
  df <- tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NA_real_)
  if (!all(c("date","rating") %in% names(df))) return(NA_real_)
  df <- tibble::as_tibble(df)
  suppressWarnings({
    df$date <- as.Date(df$date)
    target  <- as.Date(date_in)
  })
  df <- df %>% filter(!is.na(.data$date), !is.na(.data$rating), .data$date <= target) %>% arrange(.data$date)
  if (!nrow(df)) return(NA_real_)
  as.numeric(df$rating[nrow(df)])
}

if (!is.na(DIFF_REPORT_DATE) && nrow(players_json)) {
  d_chr <- as.character(DIFF_REPORT_DATE)
  report_df <- tibble(id = players_json$id, name = players_json$name) %>%
    mutate(
      wcu_rating = purrr::map_dbl(id, ~ get_rating_on_or_before(file.path(wcu_hist_dir, paste0(.x, "_wcu.json")), d_chr), .default = NA_real_),
      j_rating   = purrr::map_dbl(id, ~ get_rating_on_or_before(file.path(hist_dir,        paste0(.x, ".json")),        d_chr), .default = NA_real_),
      diff       = j_rating - wcu_rating,
      abs_diff   = abs(diff)
    ) %>%
    filter(!(is.na(.data$wcu_rating) & is.na(.data$j_rating))) %>%
    arrange(desc(.data$abs_diff), desc(.data$j_rating), .data$name)
  out_path <- file.path(cache_dir, "WCU_J_Diff.csv")
  readr::write_csv(report_df, out_path)
  cat("Wrote:", out_path, "\n")
}

# ── Site metadata ─────────────────────────────────────────────────────────

site_meta <- list(
  updated = format(RUN_DATE, "%Y-%m-%d")
)

jsonlite::write_json(
  site_meta,
  file.path(data_dir, "meta.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)

# ── Final logs ────────────────────────────────────────────────────────────
cat("J-Ratings: players exported =", nrow(players_json), "\n")
cat("WCU history files written:", wcu_wrote, "\n")
cat("J-Ratings history files written:", j_wrote, "\n")
cat("Per-player games files written:", games_written, "\n")
cat("Effective calc start:", format(effective_calc_start, "%Y-%m-%d"), "\n")
cat("Output:\n  -", file.path(data_dir, "players.json"),
    "\n  -", file.path(hist_dir, "<id>_wcu.json"),
    "\n  -", file.path(hist_dir, "<id>.json"),
    "\n  -", file.path(data_dir, "games", "<id>.json"),
    "\n  -", file.path(cache_dir, "ratings_wide_daily.csv"),
    "\n  -", SNAPSHOT_FILE, "\n")
toc()
