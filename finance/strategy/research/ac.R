#!/usr/bin/env Rscript

## Functions
calc_ac <- function(data) {
  acf_rslt <- acf(data, plot = FALSE)$acf[,,1][2:21]
  names(acf_rslt) <- str_c("lag_", 1:length(acf_rslt))
  data.frame(as.list(acf_rslt))
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(infra_path)


## Variables
types     <- c("c2c_log", "o2c_log")
ret       <- e$r$xts
symbols   <- e$universe
lookbacks <- c(250, 500)

## symbol <- symbols[1]
## type <- types[1]
## lookback <- lookbacks[1]
## range <- ranges[[1]]


## Loop
prev_plan <- plan(multiprocess(workers = detectCores() - 1))
ac <- future_map_dfr(symbols, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
    map_dfr(ranges, function(range) {
      map_dfr(types, function(type) {
        data <- ret[[type]][glue("{range$first_date}::{range$last_date}"), symbol]
        base_row <- data.frame(symbol = symbol, date = range$last_date,
                               type = type, lookback = lookback,
                               stringsAsFactors = FALSE)
        if (has_row(e$ac, base_row)) return(NULL)
        bind_cols(base_row, calc_ac(data))
      })
    })
  })
}, .progress = TRUE)
plan(prev_plan)


results <- combine_results("ac")
path <- glue("{.mkt_data}/Time_Series/AC_{date}.csv",
             date = max(results$date))
write_csv(results, path)
