#!/usr/bin/env Rscript

## Functions
fit_dist <- function(data, dist) {
  fit  <- suppressWarnings(fitdist(dist, data))
  llk  <- -tail(fit$values, 1)
  aic  <- -2 * (llk - length(fit$pars))
  pars <- c(mu = NA, sigma = NA, skew = NA, shape = NA, lambda = NA)
  pars[names(fit$pars)] <- fit$pars
  bind_cols(
    data.frame(llk = llk, aic = round(aic), stringsAsFactors = FALSE),
    data.frame(as.list(pars))
  )
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(infra_path)


## Variables
types     <- c("c2c_log", "o2c_log")
ret       <- e$r$xts
symbols   <- e$universe
lookbacks <- c(500)
dists     <- c("std", "sstd", "ged", "sged", "nig")


## Loop
prev_plan <- plan(multiprocess(workers = detectCores() - 1))
dist_fits <- future_map_dfr(symbols, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
    map_dfr(ranges, function(range) {
      map_dfr(types, function(type) {
        map_dfr(dists, function(dist) {
          data <- ret[[type]][glue("{range$first_date}::{range$last_date}"), symbol]
          base_row <- data.frame(symbol = symbol, date = range$last_date,
                                 type = type, lookback = lookback, dist = dist,
                                 stringsAsFactors = FALSE)
          if (has_row(e$dist_fits, base_row)) return(NULL)
          bind_cols(base_row, fit_dist(data, dist))
        })
      })
    })
  })
}, .progress = TRUE)
plan(prev_plan)


results <- combine_results("dist_fits")
path <- glue("{.mkt_data}/Time_Series/Dist_Fits_{date}.csv",
             date = max(results$date))
write_csv(results, path, na = "")
