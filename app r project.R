#!/usr/bin/env Rscript
# =============================================================================
# Smart Campus Energy Intelligence & Waste Detection System
# Run from the project folder with:   Rscript app.R
# =============================================================================

## ---- 0. Startup: project directory, folders, packages ---------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

get_project_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f) == 1 && nzchar(f)) return(normalizePath(dirname(f), winslash = "/"))
  normalizePath(getwd(), winslash = "/")
}
PROJECT_DIR <- get_project_dir()
setwd(PROJECT_DIR)
for (d in c("data", "reports", "outputs")) dir.create(file.path(PROJECT_DIR, d), showWarnings = FALSE, recursive = TRUE)

message("== Smart Campus Energy Intelligence: startup check ==")
message("R version : ", R.version.string)
message("Project dir: ", PROJECT_DIR)
if (getRversion() < "4.0.0") stop("R >= 4.0.0 is required. Please update R from https://cran.r-project.org")

user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_lib, .libPaths()))
}

ensure_pkgs <- function(pkgs, required = TRUE) {
  have <- function(p) requireNamespace(p, quietly = TRUE)
  missing <- pkgs[!vapply(pkgs, have, logical(1))]
  if (length(missing)) {
    message("Installing missing packages (first run needs internet): ", paste(missing, collapse = ", "))
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    tryCatch(install.packages(missing, quiet = TRUE),
             error = function(e) message("Package installation error: ", conditionMessage(e)))
    missing <- missing[!vapply(missing, have, logical(1))]
  }
  if (length(missing) && required) {
    stop("Could not install required package(s): ", paste(missing, collapse = ", "),
         ".\nRecovery: connect to the internet and re-run `Rscript app.R`, or install manually in R with\n",
         "  install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
  }
  if (length(missing)) message("Optional package(s) unavailable, using fallback methods: ", paste(missing, collapse = ", "))
  missing
}
ensure_pkgs(c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly", "DT", "forecast", "lubridate", "readr"))
opt_missing <- ensure_pkgs("isotree", required = FALSE)
HAS_ISOTREE <- !("isotree" %in% opt_missing)
message("Anomaly engine: ", if (HAS_ISOTREE) "Isolation Forest (isotree)" else "robust statistical fallback")

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(tidyr); library(ggplot2)
  library(plotly); library(forecast); library(lubridate); library(readr)
})

## ---- 1. Constants ---------------------------------------------------------
SAMPLE_PATH   <- file.path("data", "sample_energy_data.csv")
DEFAULT_TARIFF <- 8
SIM_LABEL     <- "Simulated sample data"
PAL <- list(teal = "#0f766e", amber = "#d97706", slate = "#334155", red = "#dc2626", grey = "#94a3b8", blue = "#2563eb")
ALIASES <- list(
  timestamp          = c("timestamp", "datetime", "date_time", "date", "time", "ts"),
  building_id        = c("building_id", "buildingid", "bldg_id", "building_code", "meter_id"),
  building_name      = c("building_name", "building", "buildingname", "name"),
  building_type      = c("building_type", "type", "category"),
  electricity_kwh    = c("electricity_kwh", "kwh", "energy_kwh", "consumption_kwh", "consumption", "energy", "usage_kwh"),
  temperature_c      = c("temperature_c", "temperature", "temp", "temp_c"),
  occupancy          = c("occupancy", "occupants", "people"),
  operating_hours    = c("operating_hours", "op_hours", "hours_operating"),
  electricity_tariff = c("electricity_tariff", "tariff", "price_per_kwh", "rate"),
  is_weekend         = c("is_weekend", "weekend"))
THRESH <- list(night_ratio = 0.5, night_occ_ratio = 0.5, target_night_ratio = 0.4, temp_rel_effect = 0.01,
               lowocc_share = 0.05, lowocc_occ_frac = 0.2, lowocc_kwh_mult = 1.3,
               wkend_energy_ratio = 0.7, wkend_occ_ratio = 0.5, target_wkend_ratio = 0.6,
               trend_pct_week = 0.5, repeat_anom_n = 5, repeat_anom_share = 0.02)

fmt_num <- function(x, d = 0) ifelse(is.na(x), "n/a", formatC(x, format = "f", digits = d, big.mark = ","))

## ---- 2. Simulated data generator -------------------------------------------
BUILDINGS <- data.frame(
  building_id   = sprintf("B%02d", 1:6),
  building_name = c("Academic Block", "Computer Laboratory", "Library", "Hostel", "Administrative Block", "Auditorium"),
  building_type = c("Academic", "Laboratory", "Library", "Residential", "Administrative", "Event Hall"),
  base_kw = c(38, 55, 28, 45, 22, 30), capacity = c(900, 250, 400, 600, 200, 800), stringsAsFactors = FALSE)

occ_profile <- function(type, hr, wk, ev) {
  work <- as.numeric(hr >= 8 & hr < 18)
  switch(type,
    "Academic"       = ifelse(wk, 0.04, 0.08 + 0.82 * work),
    "Laboratory"     = ifelse(wk, 0.06, 0.05 + 0.75 * work),
    "Library"        = ifelse(wk, 0.25, 0.15) + 0.6 * as.numeric(hr >= 9 & hr < 21) * ifelse(wk, 0.6, 1),
    "Residential"    = ifelse(hr >= 22 | hr < 7, 0.92, ifelse(hr < 9 | hr >= 18, 0.65, 0.3)),
    "Administrative" = ifelse(wk, 0.02, 0.04 + 0.86 * as.numeric(hr >= 9 & hr < 17)),
    "Event Hall"     = 0.03 + 0.8 * as.numeric(ev & hr >= 15 & hr < 22),
    rep(0.3, length(hr)))
}

generate_sample_data <- function(seed = 42, days = 120, start = as.Date("2025-01-01")) {
  set.seed(seed)
  ts <- seq(as.POSIXct(paste(start, "00:00:00"), tz = "UTC"), by = "3 hours", length.out = days * 8)
  hr <- as.integer(format(ts, "%H")); wk <- as.integer(format(ts, "%u")) >= 6
  doy <- as.numeric(format(ts, "%j"))
  ev <- rep(runif(days) < 0.25, each = 8)
  temp <- 27 + 4 * sin(2 * pi * (doy - 80) / 365) + 5 * sin(2 * pi * (hr - 9) / 24) + rnorm(length(ts), 0, 0.8)
  rows <- lapply(seq_len(nrow(BUILDINGS)), function(i) {
    b <- BUILDINGS[i, ]; n <- length(ts)
    frac <- occ_profile(b$building_type, hr, wk, ev)
    t_b <- temp + rnorm(n, 0, 0.3)
    cooling <- pmax(0, t_b - 26) * 0.04 * b$base_kw * (0.3 + frac)
    kwh <- (b$base_kw * 0.35 + b$base_kw * 0.9 * frac + cooling) * 3 * (1 + rnorm(n, 0, 0.06))
    data.frame(timestamp = ts, building_id = b$building_id, building_name = b$building_name,
               building_type = b$building_type, electricity_kwh = kwh, temperature_c = t_b,
               occupancy = pmax(0, round(b$capacity * frac * (1 + rnorm(n, 0, 0.08)))),
               operating_hours = 3 * as.numeric(frac > 0.4),
               electricity_tariff = ifelse(hr >= 18 & hr < 22, 9.5, 7.5),
               is_weekend = wk, stringsAsFactors = FALSE)
  })
  dat <- do.call(rbind, rows)
  hrs <- as.integer(format(dat$timestamp, "%H"))
  idx <- unique(c(sample(which(hrs %in% c(0, 3)), 40), sample(seq_len(nrow(dat)), 40)))  # injected abnormal events
  dat$electricity_kwh[idx] <- dat$electricity_kwh[idx] * runif(length(idx), 1.7, 3.2)
  dat <- dat[order(dat$timestamp, dat$building_id), ]
  dat$timestamp <- format(dat$timestamp, "%Y-%m-%d %H:%M:%S")
  dat$electricity_kwh <- round(dat$electricity_kwh, 2); dat$temperature_c <- round(dat$temperature_c, 1)
  rownames(dat) <- NULL
  dat
}

## ---- 3. CSV validation & schema mapping -------------------------------------
impute_by_group <- function(x, g, fallback) {
  x <- ave(x, g, FUN = function(v) { v[is.na(v)] <- median(v, na.rm = TRUE); v })
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x[is.na(x)] <- fallback
  x
}

validate_upload <- function(raw_df) {
  fail <- function(msg) list(error = msg, data = NULL, report = character(), quality = NULL, meta = NULL)
  if (is.null(raw_df) || nrow(raw_df) == 0) return(fail("The file contains no data rows."))
  df <- raw_df
  names(df) <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(names(df)))))
  report <- character()
  for (canon in names(ALIASES)) {
    if (canon %in% names(df)) next
    hit <- intersect(ALIASES[[canon]], names(df))
    if (length(hit)) { names(df)[names(df) == hit[1]] <- canon; report <- c(report, sprintf("Column '%s' was mapped to '%s'.", hit[1], canon)) }
  }
  miss <- character()
  if (!"timestamp" %in% names(df)) miss <- c(miss, "timestamp")
  if (!"electricity_kwh" %in% names(df)) miss <- c(miss, "electricity_kwh")
  if (!any(c("building_id", "building_name") %in% names(df))) miss <- c(miss, "building_id (or building_name)")
  if (length(miss)) return(fail(paste0("Missing required column(s): ", paste(miss, collapse = ", "),
                                      ". Columns found: ", paste(names(raw_df), collapse = ", "), ".")))
  ts  <- suppressWarnings(lubridate::parse_date_time(df$timestamp, tz = "UTC", quiet = TRUE,
          orders = c("Ymd HMS", "Ymd HM", "Ymd H", "Ymd", "dmY HMS", "dmY HM", "dmY", "mdY HMS", "mdY HM", "mdY")))
  kwh <- suppressWarnings(as.numeric(df$electricity_kwh))
  bad_ts <- is.na(ts); bad_k <- is.na(kwh); neg <- !bad_k & kwh < 0
  invalid <- bad_ts | bad_k | neg
  if (all(invalid)) return(fail("No valid rows: every row has an unreadable timestamp or a missing / non-numeric / negative electricity_kwh value."))
  if (any(invalid)) report <- c(report, sprintf("Removed %d invalid row(s): %d bad timestamp, %d missing or non-numeric kWh, %d negative kWh (first offending file rows: %s).",
        sum(invalid), sum(bad_ts), sum(bad_k), sum(neg), paste(head(which(invalid) + 1, 5), collapse = ", ")))
  keep <- !invalid; nk <- sum(keep)
  getc <- function(col) if (col %in% names(df)) trimws(df[[col]][keep]) else rep(NA_character_, nk)
  num  <- function(col) if (col %in% names(df)) suppressWarnings(as.numeric(df[[col]][keep])) else rep(NA_real_, nk)
  d <- data.frame(timestamp = ts[keep], electricity_kwh = kwh[keep], stringsAsFactors = FALSE)
  bid <- getc("building_id"); bnm <- getc("building_name")
  bid[!is.na(bid) & bid == ""] <- NA; bnm[!is.na(bnm) & bnm == ""] <- NA
  d$building_id <- ifelse(is.na(bid), bnm, bid); d$building_name <- ifelse(is.na(bnm), bid, bnm)
  bt <- getc("building_type"); bt[is.na(bt) | bt == ""] <- "Unknown"; d$building_type <- bt
  temp_raw <- num("temperature_c"); occ_raw <- num("occupancy"); op_raw <- num("operating_hours"); tar_raw <- num("electricity_tariff")
  d$temperature_c <- temp_raw; d$occupancy <- occ_raw; d$operating_hours <- op_raw; d$electricity_tariff <- tar_raw
  derived_wk <- lubridate::wday(d$timestamp, week_start = 1) >= 6
  if ("is_weekend" %in% names(df)) {
    v <- tolower(trimws(df$is_weekend[keep])); yes <- c("true", "t", "1", "yes", "y"); no <- c("false", "f", "0", "no", "n")
    d$is_weekend <- ifelse(v %in% c(yes, no), v %in% yes, derived_wk)
  } else d$is_weekend <- derived_wk
  n_unid <- sum(is.na(d$building_id)); if (n_unid) { d <- d[!is.na(d$building_id), ]; report <- c(report, sprintf("Removed %d row(s) without a building identifier.", n_unid)) }
  dup <- duplicated(d[, c("timestamp", "building_id")]); n_dup <- sum(dup)
  if (n_dup) { d <- d[!dup, ]; report <- c(report, sprintf("Removed %d duplicate (timestamp, building) record(s).", n_dup)) }
  if (nrow(d) == 0) return(fail("No usable rows remain after cleaning."))
  has_temp <- any(!is.na(d$temperature_c)); has_occ <- any(!is.na(d$occupancy))
  miss_tbl <- data.frame(Column = c("temperature_c", "occupancy", "operating_hours", "electricity_tariff"),
                         Missing = c(sum(is.na(d$temperature_c)), sum(is.na(d$occupancy)), sum(is.na(d$operating_hours)), sum(is.na(d$electricity_tariff))),
                         Treatment = c("Median by building, then global median (25 if absent)", "Median by building (0 if absent; occupancy analyses disabled)",
                                       "Derived from occupancy and sampling interval", paste("Default tariff", DEFAULT_TARIFF, "per kWh")), stringsAsFactors = FALSE)
  d$temperature_c <- impute_by_group(d$temperature_c, d$building_id, 25)
  d$occupancy     <- impute_by_group(d$occupancy, d$building_id, 0)
  d$electricity_tariff[is.na(d$electricity_tariff)] <- DEFAULT_TARIFF
  interval_h <- tryCatch(as.numeric(median(diff(sort(unique(d$timestamp)))), units = "hours"), error = function(e) 1)
  if (!is.finite(interval_h) || interval_h <= 0) interval_h <- 1
  interval_h <- min(max(interval_h, 0.25), 24)
  if (any(is.na(d$operating_hours))) {
    ref <- ave(d$occupancy, d$building_id, FUN = function(v) quantile(v, 0.95))
    derived <- if (has_occ) interval_h * as.numeric(d$occupancy >= 0.25 * ref) else rep(interval_h, nrow(d))
    d$operating_hours[is.na(d$operating_hours)] <- derived[is.na(d$operating_hours)]
  }
  if (any(miss_tbl$Missing > 0)) report <- c(report, paste0("Imputed missing values: ", paste(sprintf("%s (%d)", miss_tbl$Column[miss_tbl$Missing > 0], miss_tbl$Missing[miss_tbl$Missing > 0]), collapse = ", "), "."))
  if (nrow(d) < 500) report <- c(report, "Small dataset (<500 rows): anomaly detection and forecasts may be unreliable.")
  d <- d[order(d$timestamp, d$building_id), ]
  d$date <- as.Date(d$timestamp, tz = "UTC"); d$hour <- as.integer(lubridate::hour(d$timestamp)); rownames(d) <- NULL
  quality <- data.frame(Check = c("Rows in file", "Invalid rows removed", "Duplicates removed", "Rows after cleaning", "Buildings", "Date range", "Sampling interval (h)"),
    Result = c(nrow(raw_df), sum(invalid), n_dup, nrow(d), length(unique(d$building_id)), paste(min(d$date), "to", max(d$date)), round(interval_h, 2)), stringsAsFactors = FALSE)
  list(error = NULL, data = d, report = report, quality = quality, missing = miss_tbl,
       meta = list(has_occ = has_occ, has_temp = has_temp, interval_h = interval_h))
}

read_and_validate <- function(path) {
  raw <- tryCatch(readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), progress = FALSE, show_col_types = FALSE),
                  error = function(e) e)
  if (inherits(raw, "error")) return(list(error = paste("Could not read the CSV file:", conditionMessage(raw)), data = NULL))
  validate_upload(as.data.frame(raw))
}

load_sample <- function(force = FALSE) {
  make <- function() { message("Generating simulated sample dataset (seed 42) ..."); readr::write_csv(generate_sample_data(), SAMPLE_PATH) }
  if (force || !file.exists(SAMPLE_PATH)) make()
  res <- read_and_validate(SAMPLE_PATH)
  if (!is.null(res$error)) { make(); res <- read_and_validate(SAMPLE_PATH) }
  if (!is.null(res$error)) stop("Sample dataset could not be created: ", res$error)
  res
}

## ---- 4. Analytics: anomalies, forecasting, recommendations ------------------
WD <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

compute_baseline <- function(df) {
  df %>% group_by(building_id, hour, is_weekend) %>%
    mutate(n1 = n(), b1 = median(electricity_kwh), s1 = mad(electricity_kwh)) %>%
    group_by(building_id, hour) %>%
    mutate(b2 = median(electricity_kwh), s2 = mad(electricity_kwh)) %>% ungroup() %>%
    mutate(baseline = ifelse(n1 >= 5, b1, b2),
           scale = pmax(ifelse(n1 >= 5, s1, s2), 0.05 * baseline, 1e-6)) %>%
    select(-n1, -b1, -s1, -b2, -s2)
}

detect_anomalies <- function(df, contamination = 0.03, use_iforest = HAS_ISOTREE) {
  df <- compute_baseline(df)
  df$deviation_pct <- (df$electricity_kwh - df$baseline) / pmax(df$baseline, 1e-6) * 100
  df$z <- (df$electricity_kwh - df$baseline) / df$scale
  score <- NULL; method <- "Robust z-score vs. baseline (fallback)"
  if (use_iforest && nrow(df) >= 50) {
    feats <- data.frame(kwh = df$electricity_kwh, hour = df$hour, occupancy = df$occupancy, temp = df$temperature_c,
                        ratio = df$electricity_kwh / pmax(df$baseline, 1e-6), weekend = as.numeric(df$is_weekend))
    score <- tryCatch({
      m <- isotree::isolation.forest(feats, ntrees = 100, sample_size = min(256, nrow(feats)), seed = 42, nthreads = 1)
      predict(m, feats, type = "score")
    }, error = function(e) { message("Isolation Forest failed (", conditionMessage(e), "); using fallback."); NULL })
    if (!is.null(score)) method <- "Isolation Forest (isotree)"
  }
  if (is.null(score)) score <- df$z
  cutoff <- as.numeric(quantile(score, 1 - contamination, na.rm = TRUE))
  df$score <- score
  df$is_anomaly <- !is.na(score) & score >= cutoff & df$z >= 2 & df$deviation_pct >= 20
  df$severity <- cut(df$deviation_pct, c(-Inf, 50, 100, Inf), labels = c("Low", "Medium", "High"), right = FALSE)
  df$reason <- NA_character_
  idx <- which(df$is_anomaly)
  if (length(idx)) {
    occ_ref  <- ave(df$occupancy, df$building_id, FUN = function(v) quantile(v, 0.95))
    temp_ref <- ave(df$temperature_c, df$building_id, FUN = function(v) quantile(v, 0.90))
    lowocc <- ifelse(occ_ref > 0 & df$occupancy < 0.25 * occ_ref, " Occupancy is low relative to this building's typical peak, so the load is not explained by use.", "")
    hot    <- ifelse(df$temperature_c >= temp_ref, " Temperature is in this building's hottest 10% of readings, which may explain part of the deviation.", "")
    df$reason[idx] <- sprintf("%s used %.1f kWh at %02d:00 on a %s versus an expected %.1f kWh (%+.0f%%, robust z = %.1f). Occupancy %.0f, temperature %.1f\u00b0C.%s%s Potential issue - requires investigation.",
      df$building_name[idx], df$electricity_kwh[idx], df$hour[idx], ifelse(df$is_weekend[idx], "weekend", "weekday"),
      df$baseline[idx], df$deviation_pct[idx], df$z[idx], df$occupancy[idx], df$temperature_c[idx], lowocc[idx], hot[idx])
  }
  list(data = df, method = method, cutoff = cutoff)
}

daily_totals <- function(df) {
  d <- df %>% group_by(date) %>% summarise(kwh = sum(electricity_kwh), n = n(), .groups = "drop") %>% arrange(date)
  thr <- 0.5 * median(d$n); dropped <- sum(d$n < thr)
  d <- d[d$n >= thr, ]; attr(d, "dropped") <- dropped; d
}

metric_set <- function(actual, pred) c(MAE = mean(abs(actual - pred)), RMSE = sqrt(mean((actual - pred)^2)))

run_forecast <- function(daily, h = 30) {
  dropped <- attr(daily, "dropped") %||% 0
  if (nrow(daily) < 2) return(list(ok = FALSE, msg = "Not enough daily observations to forecast."))
  dates <- seq(min(daily$date), max(daily$date), by = "day")
  y <- daily$kwh[match(dates, daily$date)]; n <- length(y); n_missing <- sum(is.na(y))
  if (n < 28) return(list(ok = FALSE, msg = sprintf("Only %d daily observations. At least 28 days are needed for a reliable weekly-seasonal forecast; the available data is insufficient.", n)))
  if (n_missing > 0.3 * n) return(list(ok = FALSE, msg = sprintf("%d of %d days are missing; the series has too many gaps for a reliable forecast.", n_missing, n)))
  y_f <- as.numeric(forecast::na.interp(ts(y, frequency = 7))); y_ts <- ts(y_f, frequency = 7)
  hh <- min(14, floor(n * 0.25))
  train <- ts(head(y_f, n - hh), frequency = 7); test <- tail(y_f, hh)
  models <- list(
    "ETS"            = function(x, k) forecast::forecast(forecast::ets(x), h = k, level = c(80, 95)),
    "ARIMA"          = function(x, k) forecast::forecast(forecast::auto.arima(x), h = k, level = c(80, 95)),
    "Seasonal naive" = function(x, k) forecast::snaive(x, h = k, level = c(80, 95)))
  mets <- list(); hold <- list()
  for (nm in names(models)) {
    f <- tryCatch(models[[nm]](train, hh), error = function(e) NULL)
    if (!is.null(f)) { p <- as.numeric(f$mean); hold[[nm]] <- p; mets[[nm]] <- data.frame(Model = nm, t(metric_set(test, p)), check.names = FALSE) }
  }
  mt <- do.call(rbind, mets); rownames(mt) <- NULL
  cand <- mt[mt$Model %in% c("ETS", "ARIMA"), ]
  best <- if (nrow(cand)) cand$Model[which.min(cand$RMSE)] else "Seasonal naive"
  base_rmse <- mt$RMSE[mt$Model == "Seasonal naive"]
  mt$Role <- ifelse(mt$Model == best, "Selected (lowest RMSE)", ifelse(mt$Model == "Seasonal naive", "Baseline", "Candidate"))
  fc <- tryCatch(models[[best]](y_ts, h), error = function(e) NULL)
  if (is.null(fc)) { best <- "Seasonal naive"; fc <- models[[best]](y_ts, h) }
  pick <- function(m, i) if (is.matrix(m)) as.numeric(m[, i]) else rep(NA_real_, h)
  fdf <- data.frame(date = max(dates) + seq_len(h), forecast = pmax(as.numeric(fc$mean), 0),
                    lo80 = pmax(pick(fc$lower, 1), 0), hi80 = pick(fc$upper, 1), lo95 = pmax(pick(fc$lower, 2), 0), hi95 = pick(fc$upper, 2))
  decomp <- tryCatch({
    s <- stl(y_ts, s.window = "periodic", robust = TRUE)
    data.frame(date = rep(dates, 4), component = rep(c("Observed", "Trend", "Weekly seasonal", "Remainder"), each = n),
               value = c(y_f, s$time.series[, "trend"], s$time.series[, "seasonal"], s$time.series[, "remainder"]))
  }, error = function(e) NULL)
  notes <- c(sprintf("Chronological holdout: models were trained on the first %d days and evaluated on the final %d days; the selected model was then refit on all %d days.", n - hh, hh, n))
  if (n_missing) notes <- c(notes, sprintf("%d missing day(s) were interpolated.", n_missing))
  if (dropped) notes <- c(notes, sprintf("%d partially observed day(s) were excluded.", dropped))
  if (n < 60) notes <- c(notes, "Limited history (<60 days): forecasts and intervals are less reliable.")
  if (is.finite(base_rmse) && min(mt$RMSE[mt$Model != "Seasonal naive"], Inf) >= base_rmse) notes <- c(notes, "The selected model does not beat the seasonal-naive baseline on the holdout; treat the forecast with caution.")
  list(ok = TRUE, hist = data.frame(date = dates, kwh = y_f, interpolated = is.na(y)), fc = fdf, metrics = mt, model = best, holdout_n = hh,
       holdout = data.frame(date = dates[(n - hh + 1):n], actual = test, predicted = hold[[best]] %||% NA_real_), decomp = decomp, notes = notes)
}

build_recommendations <- function(df, an, has_occ, has_temp) {
  out <- list()
  add <- function(priority, building, title, why, evidence, action, impact)
    out[[length(out) + 1]] <<- data.frame(Priority = priority, Building = building, Recommendation = title, Why = why,
                                         Evidence = evidence, Action = action, Impact = impact, stringsAsFactors = FALSE)
  for (b in unique(df$building_name)) {
    d <- df[df$building_name == b, ]
    night <- d[d$hour %in% 0:5, ]; day <- d[d$hour %in% 10:16 & !d$is_weekend, ]
    if (nrow(night) >= 10 && nrow(day) >= 10) {
      nk <- mean(night$electricity_kwh); dk <- mean(day$electricity_kwh)
      occ_ok <- !has_occ || median(night$occupancy) < THRESH$night_occ_ratio * median(day$occupancy)
      if (dk > 0 && nk / dk >= THRESH$night_ratio && occ_ok) {
        sv <- max(0, nk - THRESH$target_night_ratio * dk) * nrow(night)
        add(if (nk / dk >= 0.8) "High" else "Medium", b, "Investigate unusual overnight consumption",
            "Overnight load is high relative to daytime load while the building is largely unoccupied.",
            sprintf("Night (00-05h) mean %.1f kWh/reading = %.0f%% of weekday daytime mean %.1f kWh%s.", nk, 100 * nk / dk, dk, if (has_occ) sprintf("; night occupancy %.0f%% of daytime", 100 * median(night$occupancy) / max(median(day$occupancy), 1e-6)) else ""),
            "Walk the building after hours; check HVAC schedules, lighting and standby equipment.",
            sprintf("Assumption: night load reduced to %d%% of daytime load saves about %s kWh over the selected period.", round(100 * THRESH$target_night_ratio), fmt_num(sv)))
      }
    }
    if (has_temp && nrow(d) >= 100 && sd(d$temperature_c) > 0) {
      f <- tryCatch(lm(electricity_kwh ~ temperature_c + occupancy + factor(hour), data = d), error = function(e) NULL)
      cf <- if (!is.null(f)) summary(f)$coefficients else NULL
      if (!is.null(cf) && "temperature_c" %in% rownames(cf)) {
        est <- cf["temperature_c", "Estimate"]; p <- cf["temperature_c", "Pr(>|t|)"]; rel <- est / mean(d$electricity_kwh)
        if (is.finite(rel) && p < 0.05 && rel >= THRESH$temp_rel_effect)
          add(if (rel >= 0.03) "Medium" else "Low", b, "Review cooling demand during hot periods",
              "Consumption rises with temperature even after controlling for occupancy and hour of day.",
              sprintf("+%.2f kWh per reading per +1\u00b0C (%.1f%% of mean; p = %.3g).", est, 100 * rel, p),
              "Check cooling set-points, door/window practices and equipment efficiency on hot days.",
              "Not estimated (requires equipment and set-point data).")
      }
    }
    if (has_occ) {
      ref <- quantile(d$occupancy, 0.95); med <- median(d$electricity_kwh)
      idx <- d$occupancy < THRESH$lowocc_occ_frac * ref & d$electricity_kwh > THRESH$lowocc_kwh_mult * med
      if (ref > 0 && mean(idx) >= THRESH$lowocc_share)
        add(if (mean(idx) >= 0.15) "High" else "Medium", b, "Examine high consumption during low-occupancy hours",
            "Many readings combine very low occupancy with above-normal consumption.",
            sprintf("%d of %d readings (%.1f%%) have occupancy < %d%% of peak and consumption > %.0f%% of the building median.", sum(idx), nrow(d), 100 * mean(idx), round(100 * THRESH$lowocc_occ_frac), 100 * THRESH$lowocc_kwh_mult),
            "Review schedules for lights, projectors, computers and ventilation outside occupied periods.",
            sprintf("%s kWh above the building median in those readings (measured excess, not confirmed waste).", fmt_num(sum(d$electricity_kwh[idx] - med))))
    }
    wd <- d %>% group_by(date, is_weekend) %>% summarise(k = sum(electricity_kwh), o = mean(occupancy), .groups = "drop")
    if (sum(wd$is_weekend) >= 2 && sum(!wd$is_weekend) >= 5) {
      wk_k <- mean(wd$k[wd$is_weekend]); wd_k <- mean(wd$k[!wd$is_weekend]); er <- wk_k / wd_k
      occ_r <- if (has_occ) mean(wd$o[wd$is_weekend]) / mean(wd$o[!wd$is_weekend]) else NA_real_
      if (isTRUE(er >= THRESH$wkend_energy_ratio && (is.na(occ_r) || occ_r <= THRESH$wkend_occ_ratio)))
        add("Medium", b, "Compare weekend energy use against normal weekdays",
            "Weekend consumption stays close to weekday levels although the building is much less used.",
            sprintf("Weekend daily mean %s kWh = %.0f%% of weekday mean%s.", fmt_num(wk_k), 100 * er, if (is.finite(occ_r)) sprintf("; weekend occupancy only %.0f%% of weekday", 100 * occ_r) else ""),
            "Introduce weekend set-back schedules; verify what must stay on.",
            sprintf("Assumption: weekend use falls to %d%% of weekday use saves about %s kWh over the selected period.", round(100 * THRESH$target_wkend_ratio), fmt_num(max(0, wk_k - THRESH$target_wkend_ratio * wd_k) * sum(wd$is_weekend))))
    }
    wk <- d %>% mutate(wk = lubridate::floor_date(date, "week", week_start = 1)) %>% group_by(wk) %>%
      summarise(k = sum(electricity_kwh), o = sum(operating_hours), nd = n_distinct(date), .groups = "drop") %>% filter(nd == 7)
    if (nrow(wk) >= 8) {
      use_int <- all(wk$o > 0); wk$m <- if (use_int) wk$k / wk$o else wk$k; wk$i <- seq_len(nrow(wk))
      cf <- summary(lm(m ~ i, data = wk))$coefficients; pct <- cf["i", "Estimate"] / mean(wk$m) * 100; p <- cf["i", "Pr(>|t|)"]
      if (is.finite(pct) && p < 0.05 && pct >= THRESH$trend_pct_week)
        add("Medium", b, "Investigate a rising energy-intensity trend", "Weekly consumption per operating hour is trending upward.",
            sprintf("Trend %+.2f%% per week (p = %.3g) over %d full weeks (%s).", pct, p, nrow(wk), if (use_int) "kWh per operating hour" else "kWh per week"),
            "Check for added equipment, degraded efficiency or schedule changes; inspect before the trend compounds.", "Not estimated.")
    }
    a <- an[an$building_name == b, ]
    if (nrow(a) >= THRESH$repeat_anom_n && nrow(a) / nrow(d) >= THRESH$repeat_anom_share)
      add(if (sum(a$severity == "High") >= 3) "High" else "Medium", b, "Review repeated anomalies before scheduling maintenance",
          "The same building is flagged repeatedly, which may indicate a persistent fault or operating practice.",
          sprintf("%d flagged readings (%.1f%% of readings); %d High, %d Medium, %d Low severity.", nrow(a), 100 * nrow(a) / nrow(d), sum(a$severity == "High"), sum(a$severity == "Medium"), sum(a$severity == "Low")),
          "Cross-check flagged timestamps with maintenance logs, events and meter faults.",
          sprintf("Flagged readings exceeded baseline by %s kWh in total (measured, not confirmed waste).", fmt_num(sum(a$electricity_kwh - a$baseline))))
  }
  if (!length(out)) return(data.frame(Priority = "Info", Building = "All", Recommendation = "No configured threshold was breached",
      Why = "Consumption patterns look consistent with occupancy and weekly rhythm for the selected data.", Evidence = "All rule checks passed.",
      Action = "Keep monitoring; adjust the date range or sensitivity to explore further.", Impact = "n/a", stringsAsFactors = FALSE))
  r <- do.call(rbind, out)
  r[order(match(r$Priority, c("High", "Medium", "Low", "Info"))), ]
}

## ---- 5. UI -----------------------------------------------------------------
APP_CSS <- "
body{background:#f8fafc;}
.kpi-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:12px;margin-bottom:14px;}
.kpi{background:#fff;border:1px solid #e2e8f0;border-left:5px solid #0f766e;border-radius:8px;padding:12px 14px;}
.kpi-label{font-size:.75rem;text-transform:uppercase;letter-spacing:.04em;color:#64748b;}
.kpi-value{font-size:1.55rem;font-weight:700;color:#0f172a;line-height:1.25;}
.kpi-sub{font-size:.75rem;color:#64748b;}
.badge-src{display:block;padding:8px 10px;border-radius:8px;font-size:.8rem;margin-bottom:10px;}
.badge-sim{background:#fef3c7;color:#92400e;border:1px solid #fcd34d;}
.badge-up{background:#ccfbf1;color:#115e59;border:1px solid #5eead4;}
.rec{background:#fff;border:1px solid #e2e8f0;border-radius:8px;padding:12px 16px;margin-bottom:10px;}
.rec h5{margin:0 0 4px 0;font-size:1rem;} .rec p{margin:2px 0;font-size:.88rem;}
.pri{display:inline-block;padding:1px 9px;border-radius:10px;font-size:.72rem;font-weight:600;color:#fff;margin-right:6px;}
.pri-High{background:#dc2626;} .pri-Medium{background:#d97706;} .pri-Low{background:#2563eb;} .pri-Info{background:#64748b;}
.note{font-size:.82rem;color:#475569;} .card-header{font-weight:600;}"

app_theme <- bs_theme(version = 5, primary = PAL$teal, "navbar-bg" = "#0f172a",
  base_font = font_collection("Segoe UI", "system-ui", "Helvetica Neue", "Arial", "sans-serif"))

ui <- page_navbar(
  title = "Smart Campus Energy Intelligence", id = "nav", theme = app_theme, fillable = FALSE,
  header = tags$head(tags$style(HTML(APP_CSS))),
  sidebar = sidebar(width = 310,
    uiOutput("src_badge"),
    fileInput("upload", "Upload your CSV", accept = ".csv", buttonLabel = "Browse...", placeholder = "No file selected"),
    layout_columns(col_widths = c(6, 6),
      actionButton("use_sample", "Use sample", class = "btn-outline-secondary btn-sm w-100"),
      downloadButton("dl_sample", "Sample CSV", class = "btn-outline-secondary btn-sm w-100")),
    hr(),
    dateRangeInput("dates", "Date range", start = Sys.Date() - 30, end = Sys.Date()),
    selectizeInput("buildings", "Buildings", choices = NULL, multiple = TRUE),
    sliderInput("sens", "Anomaly sensitivity (max % of readings flagged)", min = 0.5, max = 10, value = 3, step = 0.5),
    numericInput("tariff_override", "Tariff override per kWh (blank = use data tariff)", value = NA, min = 0, step = 0.1),
    selectInput("currency", "Currency symbol", choices = c("INR (\u20b9)" = "\u20b9", "USD ($)" = "$", "EUR (\u20ac)" = "\u20ac", "GBP (\u00a3)" = "\u00a3")),
    actionButton("reset", "Reset filters", class = "btn-primary w-100")),

  nav_panel("Overview",
    uiOutput("kpis"),
    layout_columns(col_widths = c(8, 4),
      card(card_header("Consumption trend (kWh per day)"), plotlyOutput("p_trend", height = 300)),
      card(card_header("Consumption share by building"), plotlyOutput("p_share", height = 300))),
    layout_columns(col_widths = c(4, 4, 4),
      card(card_header("Building-wise consumption (kWh)"), plotlyOutput("p_bld", height = 280)),
      card(card_header("Peak-demand hours (avg campus kWh per hour-of-day slot)"), plotlyOutput("p_peak", height = 280)),
      card(card_header("Weekday vs weekend (avg kWh per day)"), plotlyOutput("p_dow", height = 280), card_footer(textOutput("wk_text"))))),

  nav_panel("Waste & Anomalies",
    layout_columns(col_widths = c(3, 9),
      card(card_header("Controls"),
        checkboxGroupInput("sev", "Severity", choices = c("Low", "Medium", "High"), selected = c("Low", "Medium", "High")),
        selectInput("anom_bld", "Building for time-series view", choices = NULL),
        uiOutput("method_note"),
        div(class = "note", tags$b("Severity rules (deviation above expected baseline): "), "Low 20 to <50%, Medium 50 to <100%, High >=100%.")),
      card(card_header("Actual vs expected consumption"), plotlyOutput("p_anom", height = 340))),
    card(card_header("Detected potential issues (click a row for an explanation)"),
      div(class = "note", "Flagged readings are potential issues that require investigation; they do not prove energy wastage."),
      DT::DTOutput("anom_table")),
    uiOutput("anom_explain")),

  nav_panel("Forecasting",
    layout_columns(col_widths = c(3, 9),
      card(card_header("Settings"), radioButtons("horizon", "Forecast horizon (days)", choices = c(7, 14, 30), selected = 14, inline = TRUE),
           uiOutput("fc_summary")),
      card(card_header("Daily consumption: history and forecast"), plotlyOutput("p_fc", height = 360))),
    layout_columns(col_widths = c(5, 7),
      card(card_header("Chronological holdout validation"), DT::DTOutput("fc_metrics"), uiOutput("fc_notes")),
      card(card_header("Time-series decomposition (STL)"), plotlyOutput("p_decomp", height = 380)))),

  nav_panel("Buildings",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Energy-intensity ranking"),
        selectInput("rank_metric", "Rank by", choices = c("kWh per operating hour" = "kwh_per_op_hour", "kWh per occupant (occupant-intervals)" = "kwh_per_occupant",
                     "Total kWh" = "total_kwh", "Average kWh per reading" = "avg_kwh_per_reading", "Estimated cost" = "cost")),
        plotlyOutput("p_rank", height = 300),
        div(class = "note", "Floor area is not part of the schema, so intensity is normalised by operating hours or occupancy instead of total consumption alone.")),
      card(card_header("Weekly consumption trend by building (full weeks only)"), plotlyOutput("p_wkly", height = 400))),
    card(card_header("Building comparison table"), DT::DTOutput("bld_table"))),

  nav_panel("Savings Simulator",
    layout_columns(col_widths = c(3, 9),
      card(card_header("Assumptions (editable)"),
        numericInput("sim_price", "Price per kWh (blank = tariff in data)", value = NA, min = 0, step = 0.1),
        sliderInput("sim_red", "Target reduction in consumption (%)", min = 0, max = 50, value = 10, step = 1),
        selectInput("sim_period", "Analysis period", choices = c("Sidebar date range" = "range", "Last 30 days of data" = "30", "Last 90 days of data" = "90")),
        selectizeInput("sim_bld", "Buildings", choices = NULL, multiple = TRUE),
        numericInput("sim_ef", "Emissions factor (kg CO2 per kWh)", value = 0.71, min = 0, step = 0.01),
        div(class = "note", "Cost = kWh x tariff per kWh. Potential savings = current cost x reduction %. The default emissions factor is an editable assumption; use your regional grid factor.")),
      div(uiOutput("sim_cards"), card(card_header("Current vs. reduced cost by building"), plotlyOutput("p_sim", height = 300)),
          uiOutput("sim_note")))),

  nav_panel("Recommendations",
    div(class = "note", style = "margin:8px 0;", "Rules are evaluated on the filtered data using the thresholds listed under About. Estimated impacts appear only when they can be calculated; assumptions are stated explicitly."),
    uiOutput("recs")),

  nav_panel("Data Explorer",
    layout_columns(col_widths = c(12), card(card_header("Filtered records"),
      layout_columns(col_widths = c(3, 3, 6), downloadButton("dl_data2", "Download filtered data (CSV)", class = "btn-sm"), downloadButton("dl_anom2", "Download anomalies (CSV)", class = "btn-sm"), div()),
      DT::DTOutput("data_table"))),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Descriptive statistics"), DT::DTOutput("desc_table")),
      card(card_header("Correlation matrix (Pearson)"), plotlyOutput("p_corr", height = 330), div(class = "note", "Correlation does not imply causation."))),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Data quality report"), DT::DTOutput("quality_table"), uiOutput("quality_msgs")),
      card(card_header("Missing values (before imputation)"), DT::DTOutput("missing_table")))),

  nav_panel("Reports",
    card(card_header("Download reports (copies are also saved to the reports/ folder)"),
      layout_columns(col_widths = c(4, 4, 4), downloadButton("dl_data", "Filtered consumption data (CSV)"), downloadButton("dl_anom", "Anomaly report (CSV)"),
                     downloadButton("dl_fc", "Forecast report (CSV)")),
      br(), layout_columns(col_widths = c(4, 4, 4), downloadButton("dl_bld", "Building comparison (CSV)"), downloadButton("dl_sum", "Executive summary (TXT)"), div())),
    card(card_header("Executive summary preview"), verbatimTextOutput("sum_preview"))),

  nav_panel("About",
    card(card_header("Methodology"), HTML("
<p><b>Data.</b> A reproducible simulated dataset (seed 42; 6 buildings, 120 days, 3-hourly readings) is generated on first run. It is simulated and does not represent real campus measurements. Uploaded CSVs are validated, mapped to the internal schema and cleaned (invalid rows removed, duplicates dropped, missing optional values imputed and reported).</p>
<p><b>Anomaly detection.</b> A robust baseline (median and MAD per building x hour x weekday/weekend) gives the expected kWh. When the <code>isotree</code> package is available an Isolation Forest scores each reading using kWh, hour, occupancy, temperature, weekend flag and the ratio to baseline; otherwise the robust z-score is used. A reading is flagged if its score is in the top <i>sensitivity</i> percent, it is at least 2 robust SDs and at least 20% above baseline. Detection is unsupervised and run on the full dataset, so no labels are used. Flags are potential issues, not proven waste.</p>
<p><b>Forecasting.</b> Daily totals form a weekly-seasonal series (frequency 7). ETS, auto-ARIMA and a seasonal-naive baseline are trained on the earlier part of the series and scored (MAE, RMSE) on the last days (chronological holdout, no leakage). The lowest-RMSE candidate is refit on all data and forecasts 7/14/30 days with 80% and 95% prediction intervals. Series shorter than 28 days are rejected.</p>
<p><b>Savings.</b> Cost = kWh x tariff; potential savings = cost x assumed reduction. Annualised values are straight-line extrapolations. All are simulations, not verified savings.</p>
<p><b>Limitations.</b> No floor-area data; the synthetic data has injected anomalies by construction; the forecasting ignores holidays and weather forecasts; recommendation thresholds are heuristics and are configurable in the <code>THRESH</code> list at the top of the analytics section.</p>"))))

## ---- 6. Server -----------------------------------------------------------------
server <- function(input, output, session) {
  init <- load_sample()
  rv <- reactiveValues(data = init$data, meta = init$meta, quality = init$quality, missing = init$missing, report = init$report, source = SIM_LABEL)
  set_dataset <- function(res, label) { rv$data <- res$data; rv$meta <- res$meta; rv$quality <- res$quality; rv$missing <- res$missing; rv$report <- res$report; rv$source <- label }
  money <- function(x, d = 0) paste0(input$currency %||% "\u20b9", fmt_num(x, d))
  pl <- function(p, hover = "x unified") p %>% plotly::layout(font = list(family = "Segoe UI, Arial, sans-serif", size = 12), margin = list(l = 50, r = 10, t = 10, b = 40),
                                                                legend = list(orientation = "h", y = -0.25), hovermode = hover) %>% plotly::config(displaylogo = FALSE)

  reset_filters <- function() {
    d <- rv$data; b <- sort(unique(d$building_name))
    updateDateRangeInput(session, "dates", start = min(d$date), end = max(d$date), min = min(d$date), max = max(d$date))
    updateSelectizeInput(session, "buildings", choices = b, selected = b)
    updateSelectizeInput(session, "sim_bld", choices = b, selected = b)
    updateSliderInput(session, "sens", value = 3); updateNumericInput(session, "tariff_override", value = NA)
  }
  observeEvent(rv$data, reset_filters())
  observeEvent(input$reset, { reset_filters(); showNotification("Filters reset.", type = "message", duration = 3) })
  observe({ b <- input$buildings; cur <- isolate(input$anom_bld); updateSelectInput(session, "anom_bld", choices = b, selected = if (!is.null(cur) && cur %in% b) cur else b[1]) })

  observeEvent(input$use_sample, { set_dataset(load_sample(), SIM_LABEL); showNotification("Simulated sample dataset loaded.", type = "message") })
  observeEvent(input$upload, {
    f <- input$upload
    if (!grepl("\\.csv$", f$name, ignore.case = TRUE)) return(showNotification("Please upload a .csv file.", type = "error", duration = 8))
    if (f$size > 50 * 1024^2) return(showNotification("File is larger than 50 MB. Please upload a smaller file.", type = "error", duration = 8))
    res <- withProgress(message = "Validating upload...", value = 0.5, read_and_validate(f$datapath))
    if (!is.null(res$error)) return(showNotification(paste("Upload rejected:", res$error), type = "error", duration = NULL))
    set_dataset(res, paste0("Uploaded: ", f$name))
    showNotification(HTML(paste0("<b>Upload accepted</b> (", fmt_num(nrow(res$data)), " rows).", if (length(res$report)) paste0("<br>", paste(res$report, collapse = "<br>")) else "")),
                     type = if (length(res$report)) "warning" else "message", duration = 12)
  })
  output$dl_sample <- downloadHandler("sample_energy_data.csv", function(file) { load_sample(); file.copy(SAMPLE_PATH, file, overwrite = TRUE) })
  output$src_badge <- renderUI({
    sim <- grepl("^Simulated", rv$source)
    div(class = paste("badge-src", if (sim) "badge-sim" else "badge-up"), tags$b(if (sim) "SIMULATED DATA" else "UPLOADED DATA"), br(),
        if (sim) "Generated with a fixed random seed. Not real campus measurements." else rv$source)
  })

  ## ---- reactive data layer
  filters <- debounce(reactive(list(dates = input$dates, b = input$buildings)), 500)
  sens_d  <- debounce(reactive(input$sens), 600)
  fdata <- reactive({
    f <- filters(); req(f$dates)
    validate(need(length(f$b) > 0, "Select at least one building."))
    d <- rv$data; d[d$date >= f$dates[1] & d$date <= f$dates[2] & d$building_name %in% f$b, , drop = FALSE]
  })
  use_override <- reactive({ o <- input$tariff_override; is.numeric(o) && length(o) == 1 && !is.na(o) && o > 0 })
  with_cost <- function(df) { df$rate <- if (use_override()) input$tariff_override else df$electricity_tariff; df$cost <- df$electricity_kwh * df$rate; df }
  fdata_c <- reactive(with_cost(fdata()))
  need_df <- function() { df <- fdata_c(); validate(need(nrow(df) > 0, "No records match the current date range and building filters. Adjust the filters or press Reset filters.")); df }
  scored <- reactive({ req(sens_d()); withProgress(message = "Detecting anomalies...", value = 0.5, detect_anomalies(rv$data, sens_d() / 100)) })
  fscored <- reactive({ f <- filters(); req(f$dates); s <- scored()$data; s[s$date >= f$dates[1] & s$date <= f$dates[2] & s$building_name %in% f$b, , drop = FALSE] })
  fanom <- reactive({ s <- fscored(); s <- s[s$is_anomaly, , drop = FALSE]; if (nrow(s)) with_cost(s) else s })
  fc_res <- reactive({
    df <- fdata(); validate(need(nrow(df) > 0, "No records match the current filters."))
    withProgress(message = "Fitting forecasting models...", value = 0.4, run_forecast(daily_totals(df), 30))
  })
  cost_rate <- reactive({ d <- need_df(); sum(d$cost) / sum(d$electricity_kwh) })

  ## ---- Overview
  output$kpis <- renderUI({
    df <- need_df(); an <- fanom(); fc <- fc_res()
    tot <- sum(df$electricity_kwh); cost <- sum(df$cost); nd <- n_distinct(df$date)
    exc <- if (nrow(an)) sum(an$electricity_kwh - an$baseline) else 0; exc_cost <- if (nrow(an)) sum((an$electricity_kwh - an$baseline) * an$rate) else 0
    fk <- if (isTRUE(fc$ok)) sum(fc$fc$forecast) else NA
    kpi <- function(l, v, s = NULL, col = PAL$teal) div(class = "kpi", style = paste0("border-left-color:", col), div(class = "kpi-label", l), div(class = "kpi-value", v), if (!is.null(s)) div(class = "kpi-sub", s))
    div(class = "kpi-grid",
      kpi("Total consumption", paste(fmt_num(tot), "kWh"), paste(nd, "days in range")),
      kpi("Estimated expenditure", money(cost), if (use_override()) "using tariff override" else "kWh x data tariff", PAL$blue),
      kpi("Average daily use", paste(fmt_num(tot / nd), "kWh/day"), NULL, PAL$slate),
      kpi("Detected anomalies", fmt_num(nrow(an)), paste0(fmt_num(100 * nrow(an) / nrow(df), 1), "% of readings"), PAL$red),
      kpi("Potential savings (upper bound)", paste(fmt_num(exc), "kWh"), paste0(money(exc_cost), " if flagged excess were eliminated; needs investigation"), PAL$amber),
      kpi("Forecast: next 30 days", if (is.na(fk)) "n/a" else paste(fmt_num(fk), "kWh"), if (is.na(fk)) "insufficient data" else paste0("approx. ", money(fk * cost_rate())), PAL$teal))
  })
  output$p_trend <- renderPlotly({
    d <- need_df() %>% group_by(date) %>% summarise(kwh = sum(electricity_kwh), .groups = "drop") %>% arrange(date)
    d$ma7 <- if (nrow(d) >= 7) as.numeric(stats::filter(d$kwh, rep(1 / 7, 7), sides = 1)) else NA_real_
    plot_ly(d, x = ~date) %>% add_lines(y = ~kwh, name = "Daily kWh", line = list(color = PAL$grey, width = 1.5)) %>%
      add_lines(y = ~ma7, name = "7-day average", line = list(color = PAL$teal, width = 3)) %>% layout(yaxis = list(title = "kWh per day"), xaxis = list(title = "")) %>% pl()
  })
  bld_tot <- reactive({ d <- need_df() %>% group_by(building_name) %>% summarise(kwh = sum(electricity_kwh), .groups = "drop"); d$building_name <- factor(d$building_name, levels = d$building_name[order(d$kwh)]); d })
  output$p_share <- renderPlotly(plot_ly(bld_tot(), labels = ~building_name, values = ~kwh, type = "pie", hole = 0.5, textinfo = "percent",
        marker = list(colors = c("#0f766e", "#14b8a6", "#d97706", "#334155", "#2563eb", "#94a3b8"))) %>% pl("closest"))
  output$p_bld <- renderPlotly(plot_ly(bld_tot(), x = ~kwh, y = ~building_name, type = "bar", orientation = "h", marker = list(color = PAL$teal)) %>%
        layout(xaxis = list(title = "kWh"), yaxis = list(title = "")) %>% pl("closest"))
  output$p_peak <- renderPlotly({
    d <- need_df() %>% group_by(date, hour) %>% summarise(k = sum(electricity_kwh), .groups = "drop") %>% group_by(hour) %>% summarise(avg = mean(k), .groups = "drop")
    top <- d$hour[order(-d$avg)][1:min(3, nrow(d))]
    plot_ly(d, x = ~hour, y = ~avg, type = "bar", marker = list(color = ifelse(d$hour %in% top, PAL$amber, PAL$teal)), hovertemplate = "%{x}:00 - %{y:.0f} kWh<extra></extra>") %>%
      layout(xaxis = list(title = "Hour of day (top 3 highlighted)", dtick = 3), yaxis = list(title = "kWh")) %>% pl("closest")
  })
  dow_data <- reactive({
    d <- need_df() %>% group_by(date) %>% summarise(k = sum(electricity_kwh), .groups = "drop")
    d$wd <- lubridate::wday(d$date, week_start = 1); d$wk <- d$wd >= 6; d
  })
  output$p_dow <- renderPlotly({
    d <- dow_data() %>% group_by(wd) %>% summarise(avg = mean(k), .groups = "drop"); d$lab <- factor(WD[d$wd], levels = WD)
    plot_ly(d, x = ~lab, y = ~avg, type = "bar", marker = list(color = ifelse(d$wd >= 6, PAL$amber, PAL$teal))) %>% layout(xaxis = list(title = ""), yaxis = list(title = "kWh/day")) %>% pl("closest")
  })
  output$wk_text <- renderText({
    d <- dow_data(); if (!any(d$wk) || all(d$wk)) return("Weekend vs weekday comparison needs both types of day in the range.")
    a <- mean(d$k[d$wk]); b <- mean(d$k[!d$wk]); sprintf("Weekend days average %s kWh = %.0f%% of the weekday average (%s kWh).", fmt_num(a), 100 * a / b, fmt_num(b))
  })

  ## ---- Anomalies
  output$method_note <- renderUI({ s <- scored(); div(class = "note", style = "margin-bottom:8px;", tags$b("Method: "), s$method, sprintf(" (score cut-off %.2f)", s$cutoff)) })
  output$p_anom <- renderPlotly({
    req(input$anom_bld); d <- fscored(); d <- d[d$building_name == input$anom_bld, ]; validate(need(nrow(d) > 0, "No records for this building in the selected range."))
    a <- d[d$is_anomaly & d$severity %in% input$sev, ]
    plot_ly(d, x = ~timestamp) %>% add_lines(y = ~electricity_kwh, name = "Actual kWh", line = list(color = PAL$grey, width = 1)) %>%
      add_lines(y = ~baseline, name = "Expected baseline", line = list(color = PAL$teal, dash = "dot", width = 2)) %>%
      add_markers(data = a, y = ~electricity_kwh, name = "Potential anomaly", marker = list(color = PAL$red, size = 7),
                  text = ~paste0(severity, " severity: ", round(deviation_pct), "% above baseline"), hoverinfo = "text+x") %>%
      layout(yaxis = list(title = "kWh per reading"), xaxis = list(title = "")) %>% pl()
  })
  anom_tbl <- reactive({ a <- fanom(); a <- a[a$severity %in% input$sev, ]; a[order(-a$deviation_pct), ] })
  output$anom_table <- DT::renderDT({
    a <- anom_tbl(); validate(need(nrow(a) > 0, "No anomalies match the current filters and severity selection."))
    t <- data.frame(Timestamp = format(a$timestamp, "%Y-%m-%d %H:%M"), Building = a$building_name, `Actual kWh` = round(a$electricity_kwh, 1), `Expected kWh` = round(a$baseline, 1),
                    `Deviation %` = round(a$deviation_pct, 0), Severity = as.character(a$severity), Occupancy = round(a$occupancy), `Temp C` = round(a$temperature_c, 1), Score = round(a$score, 3), check.names = FALSE)
    DT::datatable(t, selection = "single", rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, order = list(list(4, "desc")))) %>%
      DT::formatStyle("Severity", backgroundColor = DT::styleEqual(c("Low", "Medium", "High"), c("#fef3c7", "#fed7aa", "#fecaca")))
  })
  output$anom_explain <- renderUI({
    i <- input$anom_table_rows_selected; a <- anom_tbl()
    if (is.null(i) || !nrow(a)) return(div(class = "note", "Select a row in the table to see why that reading was flagged."))
    card(card_header("Why was this reading flagged?"), p(a$reason[i]))
  })

  ## ---- Forecasting
  fc_slice <- reactive({ r <- fc_res(); validate(need(isTRUE(r$ok), r$msg %||% "Forecast unavailable.")); r })
  output$p_fc <- renderPlotly({
    r <- fc_slice(); h <- as.integer(input$horizon); f <- r$fc[seq_len(h), ]; hist <- tail(r$hist, 60)
    plot_ly() %>% add_lines(data = hist, x = ~date, y = ~kwh, name = "Observed", line = list(color = PAL$slate, width = 2)) %>%
      add_ribbons(data = f, x = ~date, ymin = ~lo95, ymax = ~hi95, name = "95% interval", fillcolor = "rgba(15,118,110,0.12)", line = list(color = "transparent")) %>%
      add_ribbons(data = f, x = ~date, ymin = ~lo80, ymax = ~hi80, name = "80% interval", fillcolor = "rgba(15,118,110,0.25)", line = list(color = "transparent")) %>%
      add_lines(data = f, x = ~date, y = ~forecast, name = paste("Forecast (", r$model, ")"), line = list(color = PAL$teal, width = 3, dash = "dash")) %>%
      layout(yaxis = list(title = "kWh per day"), xaxis = list(title = "")) %>% pl()
  })
  output$fc_summary <- renderUI({
    r <- fc_res(); if (!isTRUE(r$ok)) return(div(class = "note", style = "color:#b91c1c;", r$msg))
    h <- as.integer(input$horizon); f <- r$fc[seq_len(h), ]; tot <- sum(f$forecast); prev <- sum(tail(r$hist$kwh, h))
    div(p(tags$b("Model: "), r$model), p(tags$b("Forecast total: "), fmt_num(tot), " kWh"), p(tags$b("Estimated cost: "), money(tot * cost_rate())),
        p(tags$b("vs. previous ", h, " days: "), sprintf("%+.1f%%", 100 * (tot - prev) / prev)),
        div(class = "note", "Cost uses the average effective tariff of the filtered data."))
  })
  output$fc_metrics <- DT::renderDT({
    r <- fc_slice(); m <- r$metrics; m$MAE <- round(m$MAE, 1); m$RMSE <- round(m$RMSE, 1)
    DT::datatable(m[, c("Model", "MAE", "RMSE", "Role")], rownames = FALSE, options = list(dom = "t"))
  })
  output$fc_notes <- renderUI({ r <- fc_res(); if (!isTRUE(r$ok)) return(NULL); div(class = "note", style = "margin-top:8px;", tags$ul(lapply(c(r$notes, "MAE/RMSE are in kWh per day on the holdout window."), tags$li))) })
  output$p_decomp <- renderPlotly({
    r <- fc_slice(); validate(need(!is.null(r$decomp), "Decomposition needs at least two full weeks of data."))
    r$decomp$component <- factor(r$decomp$component, levels = c("Observed", "Trend", "Weekly seasonal", "Remainder"))
    ggplotly(ggplot(r$decomp, aes(date, value)) + geom_line(colour = PAL$teal) + facet_wrap(~component, ncol = 1, scales = "free_y") + labs(x = NULL, y = "kWh") + theme_minimal())
  })

  ## ---- Buildings
  bld_summary <- reactive({
    d <- need_df()
    s <- d %>% group_by(Building = building_name, Type = building_type) %>% summarise(total_kwh = sum(electricity_kwh), avg_kwh_per_reading = mean(electricity_kwh), cost = sum(cost),
           op_hours = sum(operating_hours), occ = sum(occupancy), .groups = "drop") %>%
      mutate(kwh_per_op_hour = ifelse(op_hours > 0, total_kwh / op_hours, NA_real_), kwh_per_occupant = ifelse(occ > 0, total_kwh / occ, NA_real_))
    pk <- d %>% group_by(Building = building_name, hour) %>% summarise(m = mean(electricity_kwh), .groups = "drop") %>% group_by(Building) %>% slice_max(m, n = 1, with_ties = FALSE) %>% ungroup()
    s$peak_hour <- sprintf("%02d:00", pk$hour[match(s$Building, pk$Building)]); s
  })
  output$p_rank <- renderPlotly({
    s <- bld_summary(); m <- input$rank_metric; s$v <- s[[m]]; s <- s[!is.na(s$v), ]; validate(need(nrow(s) > 0, "This metric is not available for the current data."))
    s$Building <- factor(s$Building, levels = s$Building[order(s$v)])
    plot_ly(s, x = ~v, y = ~Building, type = "bar", orientation = "h", marker = list(color = PAL$teal), hovertemplate = "%{y}: %{x:,.2f}<extra></extra>") %>% layout(xaxis = list(title = names(which(c("kWh per operating hour" = "kwh_per_op_hour", "kWh per occupant" = "kwh_per_occupant", "Total kWh" = "total_kwh", "Average kWh per reading" = "avg_kwh_per_reading", "Estimated cost" = "cost") == m))), yaxis = list(title = "")) %>% pl("closest")
  })
  output$p_wkly <- renderPlotly({
    d <- need_df(); d$wk <- lubridate::floor_date(d$date, "week", week_start = 1)
    full <- d %>% group_by(wk) %>% summarise(nd = n_distinct(date), .groups = "drop") %>% filter(nd == 7)
    w <- d %>% filter(wk %in% full$wk) %>% group_by(wk, building_name) %>% summarise(kwh = sum(electricity_kwh), .groups = "drop")
    validate(need(nrow(w) > 0, "At least one full week (7 days) of data is needed for the weekly trend."))
    plot_ly(w, x = ~wk, y = ~kwh, color = ~building_name, type = "scatter", mode = "lines+markers") %>% layout(yaxis = list(title = "kWh per week"), xaxis = list(title = "")) %>% pl()
  })
  output$bld_table <- DT::renderDT({
    s <- bld_summary(); t <- data.frame(Building = s$Building, Type = s$Type, `Total kWh` = round(s$total_kwh), `Avg kWh/reading` = round(s$avg_kwh_per_reading, 1), `kWh per operating hour` = round(s$kwh_per_op_hour, 2),
      `kWh per occupant` = round(s$kwh_per_occupant, 3), `Estimated cost` = round(s$cost), `Peak hour` = s$peak_hour, check.names = FALSE)
    DT::datatable(t, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  ## ---- Simulator
  sim_res <- reactive({
    r <- input$sim_red; ef <- input$sim_ef
    validate(need(is.numeric(r) && !is.na(r) && r >= 0 && r <= 100, "Reduction must be between 0 and 100%."), need(is.numeric(ef) && !is.na(ef) && ef >= 0, "Emissions factor must be a non-negative number."),
             need(length(input$sim_bld) > 0, "Select at least one building."))
    p <- input$sim_price; if (!is.null(p) && !is.na(p)) validate(need(p > 0, "Price per kWh must be positive."))
    d <- rv$data; d <- d[d$building_name %in% input$sim_bld, ]
    d <- switch(input$sim_period, range = d[d$date >= input$dates[1] & d$date <= input$dates[2], ], d[d$date > max(rv$data$date) - as.integer(input$sim_period), ])
    validate(need(nrow(d) > 0, "No records in the chosen period and buildings."))
    d$rate <- if (!is.null(p) && !is.na(p) && p > 0) p else d$electricity_tariff; d$cost <- d$electricity_kwh * d$rate
    days <- n_distinct(d$date); cur <- sum(d$cost); sav <- cur * r / 100; kwh <- sum(d$electricity_kwh)
    list(d = d, days = days, kwh = kwh, cur = cur, after = cur - sav, sav = sav, ann = sav / days * 365, co2 = kwh * r / 100 * ef, co2_ann = kwh * r / 100 * ef / days * 365, r = r, ef = ef)
  })
  output$sim_cards <- renderUI({
    s <- sim_res(); kpi <- function(l, v, sub = NULL, col = PAL$teal) div(class = "kpi", style = paste0("border-left-color:", col), div(class = "kpi-label", l), div(class = "kpi-value", v), if (!is.null(sub)) div(class = "kpi-sub", sub))
    div(class = "kpi-grid", kpi("Current estimated cost", money(s$cur), paste(s$days, "days;", fmt_num(s$kwh), "kWh"), PAL$blue), kpi("Cost after reduction", money(s$after), paste0("at -", s$r, "% consumption"), PAL$slate),
        kpi("Potential savings (simulated)", money(s$sav), "current cost x reduction %", PAL$amber), kpi("Annualised savings (extrapolation)", money(s$ann), "straight-line: period savings x 365 / days", PAL$amber),
        kpi("Avoided emissions (period)", paste(fmt_num(s$co2), "kg CO2"), paste0("annualised ~", fmt_num(s$co2_ann / 1000, 1), " t CO2"), PAL$teal))
  })
  output$p_sim <- renderPlotly({
    s <- sim_res(); d <- s$d %>% group_by(building_name) %>% summarise(cur = sum(cost), .groups = "drop"); d$after <- d$cur * (1 - s$r / 100)
    plot_ly(d, x = ~building_name, y = ~cur, type = "bar", name = "Current", marker = list(color = PAL$slate)) %>% add_trace(y = ~after, name = "After reduction", marker = list(color = PAL$teal)) %>%
      layout(barmode = "group", yaxis = list(title = paste("Cost (", input$currency, ")")), xaxis = list(title = "")) %>% pl("closest")
  })
  output$sim_note <- renderUI(div(class = "note", style = "margin-top:8px;", "These figures are scenario simulations based on the assumptions shown, not verified savings. Annualised values extrapolate the chosen period and ignore seasonality."))

  ## ---- Recommendations
  recs <- reactive({ df <- need_df(); an <- fanom(); build_recommendations(df, an, rv$meta$has_occ, rv$meta$has_temp) })
  output$recs <- renderUI({
    r <- recs()
    tagList(lapply(seq_len(nrow(r)), function(i) div(class = "rec",
      h5(span(class = paste("pri", paste0("pri-", r$Priority[i])), r$Priority[i]), r$Recommendation[i], span(class = "note", paste0("  |  ", r$Building[i]))),
      p(r$Why[i]), p(tags$b("Evidence: "), r$Evidence[i]), p(tags$b("Suggested action: "), r$Action[i]), p(tags$b("Estimated impact: "), r$Impact[i]))))
  })

  ## ---- Data explorer
  export_data <- reactive({ d <- need_df(); d$timestamp <- format(d$timestamp, "%Y-%m-%d %H:%M:%S"); d[, c("timestamp", "building_id", "building_name", "building_type", "electricity_kwh", "temperature_c", "occupancy", "operating_hours", "electricity_tariff", "is_weekend", "rate", "cost")] })
  anom_export <- reactive({ a <- fanom(); validate(need(nrow(a) > 0, "No anomalies in the current selection.")); data.frame(timestamp = format(a$timestamp, "%Y-%m-%d %H:%M:%S"), building = a$building_name, actual_kwh = round(a$electricity_kwh, 2), expected_kwh = round(a$baseline, 2),
        deviation_pct = round(a$deviation_pct, 1), severity = as.character(a$severity), occupancy = a$occupancy, temperature_c = a$temperature_c, explanation = a$reason) })
  output$data_table <- DT::renderDT(DT::datatable(export_data(), rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE)))
  output$desc_table <- DT::renderDT({
    d <- need_df(); cols <- c("electricity_kwh", "temperature_c", "occupancy", "operating_hours", "electricity_tariff", "cost")
    t <- do.call(rbind, lapply(cols, function(cn) { x <- d[[cn]]; data.frame(Variable = cn, N = sum(!is.na(x)), Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Q1 = unname(quantile(x, .25, na.rm = TRUE)),
        Median = median(x, na.rm = TRUE), Q3 = unname(quantile(x, .75, na.rm = TRUE)), Max = max(x, na.rm = TRUE)) }))
    t[, -1] <- round(t[, -1], 2); DT::datatable(t, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  output$p_corr <- renderPlotly({
    d <- need_df(); m <- d[, c("electricity_kwh", "temperature_c", "occupancy", "operating_hours", "electricity_tariff", "hour")]
    m <- m[, vapply(m, function(x) isTRUE(sd(x, na.rm = TRUE) > 0), logical(1)), drop = FALSE]; validate(need(ncol(m) >= 2, "Not enough varying numeric columns for correlation."))
    cm <- round(cor(m, use = "pairwise.complete.obs"), 2)
    plot_ly(x = colnames(cm), y = rownames(cm), z = cm, type = "heatmap", colorscale = "RdBu", zmin = -1, zmax = 1) %>% pl("closest")
  })
  output$quality_table <- DT::renderDT(DT::datatable(rv$quality, rownames = FALSE, options = list(dom = "t")))
  output$quality_msgs <- renderUI({ if (length(rv$report)) div(class = "note", tags$ul(lapply(rv$report, tags$li))) else div(class = "note", "No data-quality issues were detected during validation.") })
  output$missing_table <- DT::renderDT(DT::datatable(rv$missing, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)))

  ## ---- Reports
  bld_export <- reactive({ s <- bld_summary(); data.frame(building = s$Building, type = s$Type, total_kwh = round(s$total_kwh, 1), avg_kwh_per_reading = round(s$avg_kwh_per_reading, 2), kwh_per_operating_hour = round(s$kwh_per_op_hour, 3),
        kwh_per_occupant = round(s$kwh_per_occupant, 4), estimated_cost = round(s$cost, 2), peak_hour = s$peak_hour) })
  fc_export <- reactive({ r <- fc_slice(); f <- r$fc; f$date <- as.character(f$date); f$model <- r$model; m <- r$metrics[r$metrics$Model == r$model, ]; f$holdout_mae <- round(m$MAE, 2); f$holdout_rmse <- round(m$RMSE, 2); f[, -1] <- f[, -1]; f })
  summary_txt <- reactive({
    d <- need_df(); an <- fanom(); fc <- fc_res(); s <- bld_summary(); top <- s[which.max(s$total_kwh), ]
    c("SMART CAMPUS ENERGY INTELLIGENCE - EXECUTIVE SUMMARY", paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")), paste("Data source:", rv$source, if (grepl("^Simulated", rv$source)) "(simulated; not real measurements)" else ""),
      paste("Period:", min(d$date), "to", max(d$date), "| Buildings:", paste(sort(unique(d$building_name)), collapse = ", ")), "",
      paste0("Total consumption: ", fmt_num(sum(d$electricity_kwh)), " kWh | Estimated cost: ", money(sum(d$cost))), paste0("Average daily consumption: ", fmt_num(sum(d$electricity_kwh) / n_distinct(d$date)), " kWh"),
      paste0("Largest consumer: ", top$Building, " (", fmt_num(100 * top$total_kwh / sum(s$total_kwh), 1), "% of total)"),
      paste0("Potential issues flagged: ", nrow(an), " (High ", sum(an$severity == "High"), ", Medium ", sum(an$severity == "Medium"), ", Low ", sum(an$severity == "Low"), "); these require investigation and do not prove wastage."),
      if (isTRUE(fc$ok)) paste0("30-day forecast: ", fmt_num(sum(fc$fc$forecast)), " kWh using ", fc$model, " (holdout RMSE ", fmt_num(fc$metrics$RMSE[fc$metrics$Model == fc$model], 1), " kWh/day).") else paste("Forecast unavailable:", fc$msg),
      paste0("Scenario: a ", input$sim_red, "% reduction would save about ", money(sum(d$cost) * input$sim_red / 100), " over this period (simulation, not verified savings)."),
      "", paste("Top recommendations:"), paste0(" - [", head(recs()$Priority, 3), "] ", head(recs()$Building, 3), ": ", head(recs()$Recommendation, 3)))
  })
  output$sum_preview <- renderText(paste(summary_txt(), collapse = "\n"))
  dl <- function(name, fn, ext = "csv") downloadHandler(
    filename = function() paste0(name, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".", ext),
    content = function(file) {
      x <- fn(); if (ext == "csv") readr::write_csv(x, file) else writeLines(x, file)
      try(file.copy(file, file.path(PROJECT_DIR, "reports", paste0(name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)), overwrite = TRUE), silent = TRUE)
    })
  output$dl_data <- dl("filtered_energy_data", function() export_data()); output$dl_data2 <- dl("filtered_energy_data", function() export_data())
  output$dl_anom <- dl("anomaly_report", function() anom_export()); output$dl_anom2 <- dl("anomaly_report", function() anom_export())
  output$dl_fc <- dl("forecast_report", function() fc_export()); output$dl_bld <- dl("building_comparison", function() bld_export())
  output$dl_sum <- dl("executive_summary", function() summary_txt(), "txt")
}

## ---- 7. Launch ---------------------------------------------------------------------
app <- shinyApp(ui, server)
if (!interactive()) {
  message("Starting dashboard... it will open in your browser. Press Ctrl+C in this terminal to stop.")
  shiny::runApp(app, host = "127.0.0.1", launch.browser = TRUE)
} else app
