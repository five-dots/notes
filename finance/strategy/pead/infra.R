### Libraries ###
.base_infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(.base_infra_path)

suppressMessages(library(mgcv))


### Functions ### (. = private, .. = obsoleted)
.get_earnings_symbols <- function() {
  zacks_earnings() %>%
    pull(symbol) %>%
    unique() %>%
    str_replace("\\.", "_") %>%
    .[. != "TRUE"] %>%
    sort()
}

.extract_earnings_data <- function() {
  eod <- quotemedia_daily()
  all_ranges <- e$ranges[["200"]]

  list_results <- map(e$universe, function(symbol) {
    print(symbol)
    flags <- e$earnings_flags[, symbol]
    edates <- index(flags[flags[, 1] == 1, ])
    if (length(edates) == 0) return(NULL)

    lag_edates <- map_dbl(edates, ~ e$tdays[tail(which(e$tdays < .x), 1)]) %>%
      as.Date(origin = "1970-01-01")

    ## Extract open, close and volume
    mkt_data <- eod %>%
      filter(symbol == !!symbol, date %in% edates) %>%
      mutate(dvol = round((high + low) / 2 * volume), 0) %>%
      select(date, open, close, volume, dvol)

    ## Calc 200-days moving dollar volume
    ranges <- filter(all_ranges, last_date %in% lag_edates) %>%
      .transpose_date_df()
    dvols <- map2_dfr(ranges, edates, function(range, edate) {
      eod %>%
        filter(symbol == !!symbol,
               between(date, range$first_date, range$last_date)) %>%
        summarise(date = !!edate,
                  dvol_200 = round(median((high + low) / 2 * volume), 0))
    })

    edata <- data.frame(
      date   = edates,
      symbol = symbol,
      y      = as.numeric(e$y[edates, symbol]),
      x1     = as.numeric(e$x1[edates, symbol]),
      x2     = as.numeric(e$y[lag_edates, symbol]),
      stringsAsFactors = FALSE
    )

    left_join(edata, mkt_data, by = "date") %>%
      left_join(dvols, by = "date")
  })

  results %>%
    filter(!is.na(y), !is.na(x1), !is.na(x2)) %>%
    arrange(date, symbol)
}

get_ranges <- function(daily_ranges) {
  ranges <- daily_ranges %>%
    filter(first_date >= e$first_date) %>%
    arrange(first_date)
  ranges <- .transpose_date_df(ranges)
}

get_symbol_ranges <- function(daily_ranges, symbol) {
  ranges <- daily_ranges %>%
    filter(first_date >= e$first_date) %>%
    arrange(first_date)
  ranges <- .transpose_date_df(ranges)

  edates <- e$edata %>%
    filter(symbol == !!symbol) %>%
    pull(date) %>%
    as.list()

  lag_dates <- map(edates, ~ e$tdays[tail(which(.x > e$tdays), 1)])
  calc_ranges <- keep(ranges, ~ .x$last_date %in% lag_dates)
  map2(calc_ranges, edates, ~ {
    list(first_date = .x$first_date,
         last_date  = .x$last_date,
         this_date  = .y[1])
  })
}

get_edate_ranges <- function(lookback) {
  edates <- sort(unique(e$edata$date))
  all_ranges <- get_ranges(e$ranges[[as.character(lookback)]])
  ranges <- keep(all_ranges, ~ {
    .x$first_date >= ymd("2009-04-21") & .x$last_date %in% edates
  })
}


### Data ###

## Base Variables
## IQFeed Minute は 2007/4/27 - 2019/8/2 まで
## Zacks Earnings は、2009/4/21 から
.zacks_first_date <- ymd("2009-04-21")
e$strat_dir  <- glue("{.dropbox}/strat_dev/PEAD")
e$types      <- c("o2c", "c2o")
e$lookbacks  <- c(90)
e$tdays      <- e$ranges[[as.character(min(e$lookbacks))]]$last_date
e$first_date <- tail(e$tdays[e$tdays < ymd(.zacks_first_date)], max(e$lookbacks))[1]
e$last_date  <- ymd("2019-07-31")

## Data
## .symbols <- .get_earnings_symbols()
## .data <- get_ret(e$types, symbols = .symbols, full = FALSE)
## .data <- get_ret(e$types)

## e$universe <- .data$universe
## e$y  <- .data$o2c
## e$x1 <- .data$c2o
## e$earnings_flags <- .get_earnings_flags()
## e$edata <- .extract_earnings_data()
## 117,378

## 決算情報があるシンボルのみで再設定
## e$universe <- sort(unique(e$edata$symbol))
## e$y  <- e$y[, e$universe]
## e$x1 <- e$x1[, e$universe]
## e$earnings_flags <- e$earnings_flags[, e$universe]

e$edata <- read_last_file(glue("{e$strat_dir}/EData")) %>%
  mutate(date = ymd(date),
         dvol = as.numeric(dvol),
         dvol_200 = as.numeric(dvol_200))

e$universe <- sort(unique(e$edata$symbol))

## Linear Model Fits
.lm_fits_regex <- "LM_Fits_[0-9]{4}-[0-9]{2}-[0-9]{2}.rds$"
.lm_fits <- tryCatch({
  read_last_file(glue("{e$strat_dir}/LM_Fits"), .lm_fits_regex) %>%
    mutate(date = ymd(date))
}, error = function(e) data.frame())
e$lm_fits <- .lm_fits

## Linear Model Forecast
.lm_fcsts_regex <- "LM_Fcsts_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$"
.lm_fcsts <- tryCatch({
  read_last_file(glue("{e$strat_dir}/LM_Fcsts"), .lm_fcsts_regex) %>%
    mutate(date = ymd(date))
}, error = function(e) data.frame())
e$lm_fcsts <- .lm_fcsts

## GAM Forecast
.gam_fcsts_regex <- "GAM_Fcsts_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$"
.gam_fcsts <- tryCatch({
  read_last_file(glue("{e$strat_dir}/GAM_Fcsts"), .gam_fcsts_regex) %>%
    mutate(date = ymd(date))
}, error = function(e) data.frame())
e$gam_fcsts <- .gam_fcsts
