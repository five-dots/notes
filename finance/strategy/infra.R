### Libraries ###
suppressMessages(library(GGally))
suppressMessages(library(Metrics))
suppressMessages(library(PerformanceAnalytics))
suppressMessages(library(R.utils))
suppressMessages(library(RcppRoll))
suppressMessages(library(RiskPortfolios))
suppressMessages(library(backtester))
suppressMessages(library(data.table))
suppressMessages(library(forecast))
suppressMessages(library(formattable))
suppressMessages(library(furrr))
suppressMessages(library(fs))
suppressMessages(library(glue))
suppressMessages(library(lubridate))
suppressMessages(library(market.data))
suppressMessages(library(rmgarch))
suppressMessages(library(rutils))
suppressMessages(library(tidyverse))
suppressMessages(library(tidyquant))
suppressMessages(library(timetk))
suppressMessages(library(urca))

e <- new.env(parent = emptyenv())

### Functions ### (. = private, .. = obsoleted)

..get_combo_ret <- function(data, c2o_var, o2c_var) {
  c2o_var <- enquo(c2o_var)
  o2c_var <- enquo(o2c_var)

  d <- data %>%
    filter_data() %>%
    add_ret()

  bind_rows(
    d %>%
    mutate(date_time = date + make_duration("09:30:00")) %>%
    select(symbol, date_time, !!c2o_var) %>%
    rename(ret = !!c2o_var),
    d %>%
    mutate(date_time = date + make_duration("16:00:00")) %>%
    select(symbol, date_time, !!o2c_var) %>%
    rename(ret = !!o2c_var)
  ) %>%
    arrange(symbol, date_time) %>%
    spread(symbol, ret)
}

.transpose_date_df <- function(df) {
  purrr::transpose(df) %>%
    ## transpose すると Date => numeric になるため、再度変換
    map(~ list(first_date = as.Date(.$first_date, origin = "1970-01-01"),
               last_date = as.Date(.$last_date, origin = "1970-01-01")))
}

.filter_full_symbols <- function(universe, data) {
  ## xts データの欠損削除後の列数をシンボル毎にカウント
  counts <- map_dfr(universe, function(symbol) {
    data.frame(symbol = symbol,
               nrow = length(na.omit(data[, symbol, drop = TRUE])),
               stringsAsFactors = FALSE)
  })
  ## 欠損値のないシンボルのみを抜き出す
  universe[which(counts$nrow == max(counts$nrow))]
}

.get_sp500_symbols <- function() {
  market.data::sp500_constituents()[[1]]$symbol %>%
    str_replace("\\.", "_")
}

.get_all_cs_symbols <- function() {
  sp500 <- .get_sp500_symbols()
  r3k <- suppressMessages(tq_index("RUSSELL3000")) %>%
    pull(symbol) %>%
    str_replace("\\.", "_") %>%
    sort()
  sort(unique(c(sp500, r3k)))
}

.get_data <- function(symbols, types, data_type = "xts") {
  data <- map(types, function(type) {
    quotemedia_daily_ret(type, e$first_date, e$last_date,
                         symbols, data_type)
  }) %>% set_names(types)
  prepend(data, list(universe = symbols))
}

.get_full_data <- function(symbols, types, data_type = "xts") {
  data <- map(types, function(type) {
    quotemedia_daily_ret(type, e$first_date, e$last_date,
                         symbols, data_type)
  }) %>% set_names(types)

  univ <- map(data, ~ .filter_full_symbols(symbols, .x))
  universe <- purrr::reduce(univ, intersect)

  map(data, ~ .x[, universe]) %>%
    prepend(list(universe = universe))
}

.get_earnings_flags <- function() {
  data <- zacks_earnings_flags()
  colnames(data) <- str_replace(colnames(data), "\\.", "_")

  ## Universe に含まれていないシンボルを補完
  diff_symbols <- setdiff(e$universe, colnames(data))
  for (symbol in diff_symbols) {
    d <- xts(x = rep(0, nrow(data)), order.by = index(data))
    colnames(d) <- symbol
    data <- merge(data, d)
  }

  data[glue("{e$first_date}::{e$last_date}"), e$universe]
}


get_ret <- function(types, symbols = NULL, full = TRUE) {
  d <- quotemedia_daily_ret(types[1], e$first_date, e$last_date, symbols)

  if (is.null(symbols)) {
    ## "TRUE" を除外
    symbols <- colnames(select(d, -date)) %>% .[. != "TRUE"]
  }

  if (full) {
    .get_full_data(symbols, types, "xts")
  } else {
    .get_data(symbols, types, "xts")
  }
}

get_monthly_ranges <- function(daily_ranges) {
  ranges <- daily_ranges %>%
    ## Year + Month でグループ化し、月末を抽出
    group_by(year(last_date), month(last_date)) %>%
    slice(n()) %>%
    ungroup() %>%
    select(first_date, last_date) %>%
    filter(first_date >= e$first_date) %>%
    arrange(first_date)

  ## 最終日が月末でない場合は削除
  last_date <- tail(ranges$last_date, 1)
  month_end <- ceiling_date(last_date, unit = "month") - 1
  if (last_date != month_end) {
    ranges <- slice(ranges, -n())
  }

  .transpose_date_df(ranges)
}

get_weekly_ranges <- function(daily_ranges) {
  ranges <- daily_ranges %>%
    ## Year + ISO Week (52W) でグループ化し、週末を抽出
    group_by(year(last_date), isoweek(last_date)) %>%
    slice(n()) %>%
    ungroup() %>%
    select(first_date, last_date) %>%
    filter(first_date >= e$first_date) %>%
    arrange(first_date)

  ## 最終日が金曜日でない場合は削除
  if (wday(tail(ranges$last_date, 1)) != 6) {
    ranges <- slice(ranges, -n())
  }

  .transpose_date_df(ranges)
}

has_row <- function(data, filtering_row) {
  if (nrow(data) == 0) return(FALSE)

  by <- names(filtering_row)
  rows <- semi_join(data, filtering_row, by = by)

  if (nrow(rows) > 0) return(TRUE)
  FALSE
}

has_row2 <- function(data, filtering_row) {
  if (nrow(data) == 0) return(FALSE)

  col_names <- colnames(filtering_row)
  row <- dplyr::intersect(data[, col_names], filtering_row)

  if (nrow(row) == 1) return(TRUE)
  FALSE
}

combine_results <- function(obj_name) {
  old <- get(obj_name, envir = e)
  new <- get(obj_name, envir = .GlobalEnv)

  col_names <- unique(c(names(new), names(old)))
  if (all(c("symbol", "date") %in% col_names)) {
    bind_rows(old, new) %>%
      arrange(symbol, date)
  } else {
    bind_rows(old, new) %>%
      arrange(date)
  }
}

perf_summary <- function(r_xts) {
  map_dfr(names(r_xts), function(symbol) {
    r <- r_xts[, symbol]
    data.frame(
      Symbol  = symbol,
      Sharpe  = round(as.numeric(SharpeRatio(r, annualize = TRUE,
                                             FUN = "StdDev")), 2),
      Avg_Ret = as.numeric(Return.annualized(r)),
      Cum_Ret = as.numeric(Return.cumulative(r)),
      StdDev  = as.numeric(StdDev.annualized(r)),
      MaxDD   = as.numeric(maxDrawdown(r)),
      stringsAsFactors = FALSE
    )
  }) %>%
    mutate_at(vars(-Symbol, -Sharpe), percent)
}


### Data ###

## Base variables
e$data_dir   <- glue("{.dropbox}/strat_dev/Research")
e$first_date <- ymd("2005-07-31")
e$last_date  <- today()

## Lookback Ranges
.ranges_dir <- glue("{.mkt_data}/Yahoo/Trading_Days")
.ranges <- read_last_file(.ranges_dir)
e$ranges <- .ranges



## Unit-Root Tests
.ur_regex <- "Unit_Root_Tests_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$"
.ur_tests <- tryCatch({
  read_last_file(glue("{e$data_dir}/Unit_Root_Tests"), .ur_regex) %>%
    mutate(date = ymd(date))
}, error = function(e) data.frame())
e$ur_tests <- .ur_tests

## Ljung-Box Tests
## .box_tests <- tryCatch({
##   read_last_file(e$data_dir, "Box_Tests_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$") %>%
##     mutate(date = ymd(date))
## }, error = function(e) data.frame())
## e$box_tests <- .box_tests

## Autocorrelations
## .ac <- tryCatch({
##   read_last_file(e$data_dir, "AC_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$") %>%
##     mutate(date = ymd(date))
## }, error = function(e) data.frame())
## e$ac <- .ac

## Distribution Fits
## .dist_fits <- tryCatch({
##   read_last_file(e$data_dir, "Dist_Fits_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$") %>%
##     mutate(date = ymd(date))
## }, error = function(e) data.frame())
## e$dist_fits <- .dist_fits

## ARMA Identifications
## .arma_ids <- tryCatch({
##   read_last_file(e$data_dir, "ARMA_IDs_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$") %>%
##     mutate(date = ymd(date))
## }, error = function(e) data.frame())
## e$arma_ids <- .arma_ids

## ARMA GARCH Fits
## .arma_garch_fits <- tryCatch({
##   read_last_file(e$data_dir, "ARMA_GARCH_Fits_[0-9]{4}-[0-9]{2}-[0-9]{2}.rds$") %>%
##     mutate(date = ymd(date))
## }, error = function(e) data.frame())
## e$arma_garch_fits <- .arma_garch_fits

## ARMA GARCH Forecast
## .arma_garch_fcsts <- tryCatch({
##   read_last_file(e$data_dir, "ARMA_GARCH_Fcsts_[0-9]{4}-[0-9]{2}-[0-9]{2}.rds$") %>%
##     mutate(date = ymd(date))
## }, error = function(e) data.frame())
## e$arma_garch_fcsts <- .arma_garch_fcsts

## GARCH Fits
## .garch_fits <- tryCatch({
##   read_last_file(e$data_dir, "/GARCH_Fits_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$") %>%
##     mutate(date = ymd(date))
## }, error = function(e) data.frame())
## e$garch_fits <- .garch_fits
