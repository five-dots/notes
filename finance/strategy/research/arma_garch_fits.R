#!/usr/bin/env Rscript

## arma_ids.R で推定した ARMA(p,q) と dist_fits.R で推定した最適な分布を利用して
## GARCH モデルの当てはめを行う
## GARCH の次数と最大化対数尤度・AIC・推定された係数の行列を出力する.

## Functions
fit_arma_garch <- function(data, model, mu, ar, ma, alpha, beta, dist, timeout = 60) {

  spec <- ugarchspec(
    mean.model = list(armaOrder = c(ar, ma), include.mean = as.logical(mu)),
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
    coef <- fit@fit$matcoef
    data.frame(llk = llk, aic = aic, coef = I(list(coef)))
  } else {
    data.frame(llk = NA, aic = NA, coef = NA)
  }
}

get_best_arma_id <- function(data, symbol, date, type, lookback) {
  data %>%
    filter(symbol == !!symbol, date == !!date,
           type == !!type, lookback == !!lookback) %>%
    slice(which.min(aic)) %>%
    select(mu, ar, ma, dist) %>%
    as.list()
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(infra_path)


## Variables
types     <- c("c2c_log")
ret       <- e$r$xts
symbols   <- e$universe
lookbacks <- c(500)
orders    <- cross(list(alpha = 1:2, beta = 1:2))
models    <- c("eGARCH")

## symbol <- "SPY"
## type <- types[1]
## lookback <- 500
## model <- models[1]
## order <- orders[[1]]
## ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
## range <- ranges[[1]]
## date <- range$last_date
## mu <- 1
## ar <- 1
## ma <- 1
## alpha <- 1
## beta <- 1
## dist <- "std"
## timeout <- 60


## Loop
prev_plan <- plan(multiprocess)
arma_garch_fits <- future_map_dfr(symbols, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
    map_dfr(ranges, function(range) {
      map_dfr(types, function(type) {
        map_dfr(orders, function(order) {
          map_dfr(models, function(model) {
            data <- ret[[type]][glue("{range$first_date}::{range$last_date}"), symbol]
            id <- get_best_arma_id(e$arma_ids, symbol, range$last_date, type, lookback)
            base_row <- data.frame(symbol = symbol, date = range$last_date,
                                   type = type, lookback = lookback,
                                   model = model, mu = id$mu, ar = id$ar, ma = id$ma,
                                   alpha = order$alpha, beta = order$beta, dist = id$dist,
                                   stringsAsFactors = FALSE)
            if (has_row(e$arma_garch_fits, base_row)) return(NULL)
            bind_cols(base_row, fit_arma_garch(data, model, id$mu, id$ar, id$ma,
                                               order$alpha, order$beta, id$dist))
          })
        })
      })
    })
  })
}, .progress = TRUE)
plan(prev_plan)


results <- combine_results("arma_garch_fits")
path <- glue("{.mkt_data}/Time_Series/ARMA_GARCH_Fits_{date}.rds",
             date = max(results$date))
saveRDS(results, path)
