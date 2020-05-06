#!/usr/bin/env Rscript

## Functions
get_multi <- function(data) {
  spec <- ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    distribution.model = "norm")

  specs <- multispec(replicate(ncol(data), spec))
  fits <- tryCatch({
    multifit(specs, data)
  }, error = function(e) NULL)

  list(specs = specs, fits = fits)
}

fit_dcc <- function(spec, data, ..., fit = NULL, timeout = 60) {
  ##  16: In .egarchfit(spec = spec, data = data, out.sample = out.sample,  ... :
  ##  ugarchfit-->warning: solver failer to converge.
  tryCatch({
    withTimeout({
      dccfit(spec, data, solver = "solnp", fit = fit, fit.control = list(...))
    }, timeout = timeout, onTimeout = "error")
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

fcst_cov <- function(data, iters = 6) {

  ## Initial DCC fit by multifit object (garch-norm + dcc-mvt)
  multi <- get_multi(data)
  if (is.null(multi)) {
    return(data.frame(ccc = NA, dcc = NA, deco = NA))
  }
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
    return(data.frame(ccc = NA, dcc = NA, deco = NA))
  }

  ## Extract forecast from best fit
  fit <- dcc_fits[[which.max(loglik)]]
  fcst <- dccforecast(fit)

  ## Variables
  N <- ncol(data)
  D <- sigma(fcst)[,,1]
  universe <- colnames(data)

  ## CCC
  R_ccc <- cor(data)
  H_ccc <- diag(D) %*% R_ccc %*% diag(D)
  colnames(H_ccc) <- universe
  rownames(H_ccc) <- universe

  ## DCC
  H_dcc <- rcov(fcst)[[1]][,,1]

  ## DECO (Dynamic Equicorrelation)
  one <- matrix(1, N, N)
  iota <- rep(1, N)
  Q_dcc <- rcor(fcst, type="Q")[[1]][,,1]
  rho <- as.numeric((N*(N-1))^(-1) * (t(iota) %*% Q_dcc %*% iota - N))
  R_deco <- (1-rho) * diag(1, N, N) + rho * one
  H_deco <- diag(D) %*% R_deco %*% diag(D)
  colnames(H_deco) <- colnames(data)
  rownames(H_deco) <- colnames(data)

  data.frame(hist = I(list(cov(data))),
             ccc  = I(list(H_ccc)),
             dcc  = I(list(H_dcc)),
             deco = I(list(H_deco)))
}

fit_ar_xreg <- function() {
  spec <- ugarchspec(
    mean.model = list(armaOrder = c(1, 0),
                      include.mean = TRUE),
    variance.model = list(model = "eGARCH",
                          garchOrder = c(1, 1)),
    distribution.model = "std")

  x <- head(xreg, 500)
  spec <- ugarchspec(
    mean.model = list(armaOrder = c(1, 0),
                      include.mean = TRUE,
                      external.regressors = x),
    variance.model = list(model = "eGARCH",
                          garchOrder = c(1, 1),
                          external.regressors = x),
    distribution.model = "std")

  fcst_len <- nrow(data) - lookback
  roll <- tryCatch({
    withTimeout({

      a <- ugarchroll(spec, data, n.ahead = 1, forecast.length = fcst_len,
                      refit.every = fcst_len - 1, refit.window = "moving",
                      window.size = lookback)

      a@forecast$density

    }, timeout = timeout)
  }, condition = function(c) NULL)


}

infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(infra_path)


## Variables
ret       <- e$r$xts
lookbacks <- c(500)
universe <- c("AMZN", "BA", "JNJ", "JPM", "MSFT", "NEE", "PG", "XOM")

symbol   <- "AMZN"
lookback <- 500
ranges   <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
range    <- ranges[[1]]
date     <- range$last_date
idx      <- 1


## Loop
prev_plan <- plan(multiprocess)

ar_xreg_fits <- future_map_dfr(universe, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
    map_dfr(1:(length(ranges) - 1), function(idx) {
      r1 <- ranges[[idx]]
      r2 <- ranges[[idx + 1]]
      print(glue("{range$first_date} {range$last_date}"))

      rows <- nrow(ret[["o2c_log"]][glue("{r1$first_date}::{r2$last_date}"), symbol])
      data <- ret[["o2c_log"]][glue("{r1$first_date}::"), symbol] %>% head(rows + 2)
      xreg <- ret[["c2o_log"]][glue("{r1$first_date}::"), symbol] %>% head(rows + 2)

      ## data <- ret[["o2c_log"]][glue("{range$first_date}::{range$last_date}"), symbol]
      ## xreg <- ret[["c2o_log"]][glue("{range$first_date}::{range$last_date}"), symbol]

      base_row <- data.frame(symbol = symbol, date = range$last_date,
                             lookback = lookback, stringsAsFactors = FALSE)
      if (has_row(e$ar_xreg_fits, base_row)) return(NULL)
      bind_cols(base_row, fcst_cov(data, iters = 6))
    })
  })
})
plan(prev_plan)


results <- combine_results("ar_xreg_fits")
path <- glue("{e$data_dir}/Cov_Fcsts/AR_Xreg_Fits_{date}.rds",
             date = max(results$date))
saveRDS(results, path)
