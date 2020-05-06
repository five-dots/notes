#!/usr/bin/env Rscript

## Functions and Infra Data
test_ur <- function(data, pct = "5pct") {
  adf_rslt <- ur.df(data, type = "drift", lags = 10, selectlags = "AIC")
  cval <- adf_rslt@cval["tau2", pct]
  stat <- adf_rslt@teststat[, "tau2"]
  data.frame(has_ur = stat > cval)
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(infra_path)


## Variables
types     <- c("c2c_log", "o2c_log", "c2o_log")
lookbacks <- c(500)
all_data  <- get_ret(types, full = TRUE)
universe  <- all_data[[1]]
ret       <- all_data[2:length(data)]

## symbol   <- "AAPL"
## type     <- types[1]
## lookback <- lookbacks[1]
## ranges   <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
## range    <- ranges[[1]]
## universe <- "AAPL"


## Loop
prev_plan <- plan(multiprocess(workers = detectCores() - 1))
ur_tests <- future_map_dfr(universe, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
    map_dfr(ranges, function(range) {
      map_dfr(types, function(type) {
        data <- ret[[type]][glue("{range$first_date}::{range$last_date}"), symbol]
        base_row <- data.frame(symbol = symbol, date = range$last_date,
                               type = type, lookback = lookback,
                               stringsAsFactors = FALSE)
        if (has_row2(e$ur_tests, base_row)) return(NULL)
        bind_cols(base_row, test_ur(data))
      })
    })
  })
}, .progress = TRUE)
plan(prev_plan)


results <- combine_results("ur_tests")
path <- glue("{e$data_dir}/Unit_Root_Tests/Unit_Root_Tests_{date}.csv",
             date = max(results$date))
write_csv(results, path)
