#!/usr/bin/env Rscript

## Functions
test_box <- function(data, lag = 1) {
  box_rslt <- Box.test(data, lag = lag, type = "Ljung-Box")
  data.frame(pval = box_rslt$p.value)
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(infra_path)


## Variables
types     <- c("c2c_log", "o2c_log")
ret       <- e$r$xts
symbols   <- e$universe
lookbacks <- 500
lags      <- 1:10


## Loop
prev_plan <- plan(multiprocess(workers = detectCores() -1))
box_tests <- future_map_dfr(symbols, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
    map_dfr(ranges, function(range) {
      map_dfr(types, function(type) {
        map_dfr(lags, function(lag) {
          data <- ret[[type]][glue("{range$first_date}::{range$last_date}"), symbol]
          base_row <- data.frame(symbol = symbol, date = range$last_date,
                                 type = type, lookback = lookback, lag = lag,
                                 stringsAsFactors = FALSE)
          if (has_row(e$box_tests, base_row)) return(NULL)
          bind_cols(base_row, test_box(data, lag))
        })
      })
    })
  })
}, .progress = TRUE)
plan(prev_plan)


results <- combine_results("box_tests")
path <- glue("{.mkt_data}/Time_Series/Box_Tests_{date}.csv",
             date = max(results$date))
write_csv(results, path)
