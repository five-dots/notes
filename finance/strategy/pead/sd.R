#!/usr/bin/env Rscript

## Functions and Infra Data
infra_path <- glue::glue("{.dropbox}/memo/strategy/pead/infra.R")
source(infra_path)


## Variables
lookbacks <- e$lookbacks
universe  <- e$universe
types     <- c("y", "x1")

## symbol   <- "AAPL"
## lookback <- lookbacks[1]
## type     <- types[1]
## ranges   <- get_symbol_ranges(e$ranges[[as.character(lookback)]], symbol)
## range    <- ranges[[1]]
## universe <- "AAPL"


## Loop
prev_plan <- plan(multiprocess(workers = detectCores() - 1))
sd <- future_map_dfr(universe, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_symbol_ranges(e$ranges[[as.character(lookback)]], symbol)
    map_dfr(ranges, function(range) {
      map_dfr(types, function(type) {
        data <- e[[type]][glue("{range$first_date}::{range$last_date}"), symbol]
        base_row <- data.frame(symbol = symbol, date = range$this_date,
                               lookback = lookback, type = type,
                               stringsAsFactors = FALSE)
        if (has_row2(e$sd, base_row)) return(NULL)
        bind_cols(base_row, data.frame(sd = sd(data, na.rm = TRUE)))
      })
    })
  })
}, .progress = TRUE)
plan(prev_plan)


results <- combine_results("sd")
path <- glue("{e$strat_dir}/SD/SD_{date}.csv", date = max(results$date))
write_csv(results, path)
