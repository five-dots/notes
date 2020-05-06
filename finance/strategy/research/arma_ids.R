#!/usr/bin/env Rscript

## Functions
calc_aic <- function(data, mu, ar, ma, dist, timeout = 60) {
  spec <- arfimaspec(
    mean.model = list(armaOrder = c(ar, ma),
                      include.mean = as.logical(mu)),
    distribution.model = dist)

  ## Solver を順番に試して、計算が完了したら関数を抜ける
  ## solvers <- c("solnp", "gosolnp", "nlminb", "nloptr")
  ## for (solver in solvers) {
  ##   fit <- tryCatch({
  ##     arfimafit(spec, data, solver = "gosolnp")
  ##   }, condition = function(c) NULL)
  ##   if (!is.null(fit) && convergence(fit) == 0) {
  ##     llk <- likelihood(fit)
  ##     if (llk > 0) {
  ##       aic <- -2 * (llk - length(coef(fit)))
  ##       return(data.frame(llk = llk, aic = aic, solver = solver))
  ##     }
  ##   }
  ## }
  ## data.frame(llk = NA, aic = NA, solver = NA)

  ## fit <- arfimafit(spec, data, solver = "hybrid")
  ## fit <- arfimafit(spec, data, solver = "solnp")
  ## fit <- arfimafit(spec, data, solver = "nlminb")
  ## fit <- arfimafit(spec, data, solver = "nloptr")

  fit <- tryCatch({
    ## 稀に長時間かかる場合があるので、タイマーを設定する
    withTimeout({
      arfimafit(spec, data, solver = "solnp")
    }, timeout = timeout)
  }, condition = function(c) NULL)

  if (!is.null(fit) && convergence(fit) == 0) {
    llk <- likelihood(fit)
    aic <- -2 * (llk - length(coef(fit)))
    data.frame(llk = llk, aic = aic)
  } else {
    data.frame(llk = NA, aic = NA)
  }
}

get_best_dist <- function(data, symbol, date, type, lookback) {
  data %>%
    filter(symbol == !!symbol, date == !!date,
           type == !!type, lookback == !!lookback) %>%
    slice(which.min(aic)) %>%
    pull(dist)
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(infra_path)


## Variables
types     <- c("c2c_log", "o2c_log")
ret       <- e$r$xts
symbols   <- e$universe
lookbacks <- c(500)
orders    <- cross(list(mu = 1, ar = 0:2, ma = 0:2))

## JPM 2017-08-31 500 c2c_log 2:1

## Loop
prev_plan <- plan(multiprocess)
arma_ids <- future_map_dfr(symbols, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
    map_dfr(ranges, function(range) {
      map_dfr(types, function(type) {
        map_dfr(orders, function(order) {
          data <- ret[[type]][glue("{range$first_date}::{range$last_date}"), symbol]
          dist <- get_best_dist(e$dist_fits, symbol, range$last_date, type, lookback)
          base_row <- data.frame(symbol = symbol, date = range$last_date,
                                 type = type, lookback = lookback,
                                 mu = order$mu, ar = order$ar,
                                 ma = order$ma, dist = dist,
                                 stringsAsFactors = FALSE)
          if (has_row(e$arma_ids, base_row)) return(NULL)
          bind_cols(base_row, calc_aic(data, order$mu, order$ar, order$ma, dist))
        })
      })
    })
  })
}, .progress = TRUE)
plan(prev_plan)


results <- combine_results("arma_ids")
path <- glue("{.mkt_data}/Time_Series/ARMA_IDs_{date}.csv",
             date = max(results$date))
write_csv(results, path, na = "")
