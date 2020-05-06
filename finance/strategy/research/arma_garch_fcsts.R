#!/usr/bin/env Rscript

## arma_garch_fits.R で同定した ARMA+GARCH の次数を使って
## Conditional Mean と Conditional Var の Rolling Forecast を行う.

## Functions
fcst_arma_garch <- function(data, end_date, lookback, params, timeout = 60) {

  spec <- ugarchspec(
    mean.model = list(armaOrder = c(params$ar, params$ma),
                      include.mean = as.logical(params$mu)),
    variance.model = list(model = params$model,
                          garchOrder = c(params$alpha, params$beta)),
    distribution.model = params$dist)

  fcst_len <- nrow(data) - lookback
  roll <- tryCatch({
    withTimeout({
      ugarchroll(spec, data, n.ahead = 1, forecast.length = fcst_len,
                 refit.every = fcst_len - 1, refit.window = "moving",
                 window.size = lookback)
    }, timeout = timeout)
  }, condition = function(c) NULL)

  if (!is.null(roll) && convergence(roll) == 0) {
    fcst <- roll@forecast$density %>%
      mutate(date = rownames(.)) %>%
      filter(date <= end_date) %>%
      select(date, Mu, Sigma) %>%
      set_names(c("date", "mu", "sigma"))

    data.frame(fcst = I(list(fcst)))
  } else {
    data.frame(fcst = NA)
  }
}

get_best_arma_garch_params <- function(data, symbol, date, type, lookback) {
  data %>%
    filter(symbol == !!symbol, date == !!date,
           type == !!type, lookback == !!lookback) %>%
    slice(which.min(aic)) %>%
    select(model, mu, ar, ma, alpha, beta, dist, coef) %>%
    as.list()
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(infra_path)


## Variables
types     <- c("c2c_log")
ret       <- e$r$xts
symbols   <- e$universe
lookbacks <- c(500)

## symbol <- "SPY"
## type <- types[1]
## lookback <- 500
## ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
## date <- range$last_date
## mu <- 1
## ar <- 1
## ma <- 1
## alpha <- 1
## beta <- 1
## dist <- "std"
## timeout <- 60
## idx <- 50


## Loop (5mim/symbol)
prev_plan <- plan(multiprocess)
arma_garch_fcsts <- future_map_dfr(symbols, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
    map_dfr(1:(length(ranges) - 1), function(idx) {
      r1 <- ranges[[idx]]
      r2 <- ranges[[idx + 1]]
      map_dfr(types, function(type) {
        rows <- nrow(ret[[type]][glue("{r1$first_date}::{r2$last_date}"), symbol])
        data <- ret[[type]][glue("{r1$first_date}::"), symbol] %>% head(rows + 2)
        params <- get_best_arma_garch_params(e$arma_garch_fits, symbol,
                                             r1$last_date, type, lookback)
        base_row <- data.frame(symbol = symbol, date = r1$last_date,
                               type = type, lookback = lookback,
                               model = params$model, mu = params$mu,
                               ar = params$ar, ma = params$ma,
                               alpha = params$alpha, beta = params$beta,
                               dist = params$dist,
                               stringsAsFactors = FALSE)
        if (has_row(e$arma_garch_fcsts, base_row)) return(NULL)
        bind_cols(base_row, fcst_arma_garch(data, r2$last_date, lookback, params))
      })
    })
  })
}, .progress = TRUE)
plan(prev_plan)


results <- combine_results("arma_garch_fcsts")
path <- glue("{.mkt_data}/Time_Series/ARMA_GARCH_Fcsts_{date}.rds",
             date = max(results$date))
saveRDS(results, path)
