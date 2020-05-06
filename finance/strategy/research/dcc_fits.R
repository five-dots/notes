#!/usr/bin/env Rscript

## Functions
get_multi <- function(data) {
  spec <- ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    distribution.model = "norm")

  specs <- multispec(replicate(ncol(data), spec))
  fits <- multifit(specs, data)

  list(specs = specs, fits = fits)
}

fit_dcc <- function(spec, data, ..., fit = NULL, timeout = 60) {
  tryCatch({
    withTimeout({
      dccfit(spec, data, solver = "solnp", fit = fit, fit.control = list(...))
    }, timeout = timeout)
  }, error = function(e) NULL)
}

fit_dcc_mvt <- function(data, shape, ...) {
  ugarch_spec <- ugarchspec(
    mean.model = list(armaOrder = c(0, 0)),
    variance.model = list(garchOrder = c(1, 1), model = "eGARCH"),
    distribution.model = "std",
    fixed.pars = list(shape = shape))

  mgarch_spec <- dccspec(
    uspec = multispec(replicate(ncol(data), ugarch_spec)),
    dccOrder = c(1,1), distribution = "mvt")

  fit_dcc(mgarch_spec, data, ...)
}

fcst_dcc_mvt <- function(data, iters = 6) {

  ## Initial DCC fit by multifit object (garch-norm + dcc-mvt)
  multi <- get_multi(data)
  init_spec <- dccspec(multi$specs, dccOrder = c(1, 1), distribution = "mvt")

  dcc_fits <- vector("list", length = iters)
  dcc_fits[[1]] <- fit_dcc(init_spec, data, eval.se = FALSE, fit = multi$fits)

  ## Iterate fits by previous fitted shape (garch-std + dcc-mvt)
  shape <- ifelse(is.null(dcc_fits[[1]]), 10, rshape(dcc_fits[[1]]))
  for (i in 2:iters) {
    dcc_fits[[i]] <- fit_dcc_mvt(data, shape, eval.se = FALSE)
    if (!is.null(dcc_fits[[i]])) shape <- rshape(dcc_fits[[i]])
  }

  ## Extract the best llk fit
  loglik <- map_dbl(dcc_fits, ~ ifelse(is.null(.x), NA_real_, likelihood(.x)))
  if (all(is.na(loglik))) {
    return(data.frame(llk = NA, aic = NA, cov = NA, cor = NA, mean = NA))
  }

  fit  <- dcc_fits[[which.max(loglik)]]
  fcst <- dccforecast(fit)
  llk  <- likelihood(fit)
  aic  <- infocriteria(fit)["Akaike", ]
  cov  <- rcov(fcst)[[1]][,,1]
  cor  <- rcor(fcst)[[1]][,,1]
  mean <- fitted(fcst)[,,1]
  data.frame(llk  = llk,
             aic  = aic,
             cov  = I(list(cov)),
             cor  = I(list(cor)),
             mean = I(list(mean)))
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(infra_path)


## Variables
types     <- c("c2c_log")
ret       <- e$r$xts
universe  <- c("SPY", "QQQ", "DIA", "IWM", "EFA", "EEM", "VNQ",
               "SHY", "IEF", "TLT", "TIP", "LQD", "USO", "GLD")
lookbacks <- c(500)
## models    <- c("DCC", "aDCC", "FDCC") # asymmetric, flexible
## dists     <- c("mvnorm", "mvt", "mvlaplace")

type     <- types[1]
lookback <- 500
ranges   <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
range    <- ranges[[1]]
date     <- range$last_date
iters    <- 6


## Loop
prev_plan <- plan(multiprocess)
dcc_fcsts <- map_dfr(lookbacks, function(lookback) {
  ## ranges <- get_weekly_ranges(e$ranges[[as.character(lookback)]])
  ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
  ## Parallel by ranges
  future_map_dfr(ranges, function(range) {
    map_dfr(types, function(type) {
      data <- ret[[type]][glue("{range$first_date}::{range$last_date}"), universe]
      base_row <- data.frame(date = range$last_date, type = type,
                             lookback = lookback, stringsAsFactors = FALSE)
      print(base_row)
      if (has_row(e$dcc_fcsts, base_row)) return(NULL)
      bind_cols(base_row, fcst_dcc_mvt(data, iters = 6))
    })
  }, .progress = TRUE)
})
plan(prev_plan)


results <- combine_results("dcc_fcsts")
path <- glue("{.mkt_data}/Time_Series/DCC_Fcsts_{date}.rds",
             date = max(results$date))
saveRDS(results, path)
