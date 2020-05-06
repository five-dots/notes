#!/usr/bin/env Rscript

## Functions
fit_garch <- function(data, model, alpha, beta, dist, timeout = 60) {

  spec <- ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    variance.model = list(model = model, garchOrder = c(alpha, beta)),
    distribution.model = dist)

  fit <- tryCatch({
    withTimeout({
      ugarchfit(spec, data)
    }, timeout = timeout)
  }, condition = function(c) NULL)

  if (!is.null(fit) && convergence(fit) == 0) {
    llk  <- likelihood(fit)
    aic  <- -2 * (llk - length(coef(fit)))
    data.frame(llk = llk, aic = aic)
  } else {
    data.frame(llk = NA, aic = NA)
  }
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(infra_path)


## Variables
types     <- c("c2c_log")
ret       <- e$r$xts
symbols   <- e$universe
lookbacks <- c(500)
orders    <- cross(list(alpha = 1:2, beta = 1:2))
models    <- c("sGARCH", "eGARCH")
dists     <- c("norm", "std")

## symbol <- "SPY"
## type <- types[1]
## lookback <- 500
## model <- models[1]
## order <- orders[[1]]
## ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
## range <- ranges[[1]]
## date <- range$last_date
## alpha <- 1
## beta <- 1
## dist <- "std"
## timeout <- 60


## Loop
prev_plan <- plan(multiprocess)
garch_fits <- future_map_dfr(symbols, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
    map_dfr(ranges, function(range) {
      map_dfr(types, function(type) {
        map_dfr(models, function(model) {
          map_dfr(orders, function(order) {
            map_dfr(dists, function(dist) {
              data <- ret[[type]][glue("{range$first_date}::{range$last_date}"), symbol]
              base_row <- data.frame(symbol = symbol, date = range$last_date,
                                     type = type, lookback = lookback,
                                     model = model, alpha = order$alpha,
                                     beta = order$beta, dist = dist,
                                     stringsAsFactors = FALSE)
              if (has_row(e$garch_fits, base_row)) return(NULL)
              bind_cols(base_row, fit_garch(data, model, order$alpha, order$beta, dist))
            })
          })
        })
      })
    })
  })
}, .progress = TRUE)
plan(prev_plan)


results <- combine_results("garch_fits")
path <- glue("{.mkt_data}/Time_Series/GARCH_Fits_{date}.csv",
             date = max(results$date))
write_csv(results, path)
