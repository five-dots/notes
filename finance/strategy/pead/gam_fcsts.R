#!/usr/bin/env Rscript

## Functions and Infra Data
fits_gam <- function(formula, family, data) {
  gam_fit <- switch(
    family,
    "gaussian" = gam(as.formula(formula), family = gaussian, data = data),
    "scat" = gam(as.formula(formula), family = scat, data = data))

  ## gam_fit$R
  ## gam_fit$Vc
  ## gam_fit$Ve
  ## gam_fit$Vp
  ## gam_fit$aic
  ## gam_fit$assign
  ## gam_fit$boundary
  ## gam_fit$call
  ## gam_fit$cmX
  ## gam_fit$coefficients
  ## gam_fit$converged
  ## gam_fit$control
  ## gam_fit$db.drho
  ## gam_fit$deviance
  ## gam_fit$df.null
  ## gam_fit$df.residual
  gam_fit$dw.drho <- NULL
  ## gam_fit$edf
  ## gam_fit$edf1
  ## gam_fit$family
  gam_fit$fitted.values <- NULL
  ## gam_fit$formula
  ## gam_fit$gcv.ubre
  ## gam_fit$gcv.ubre.dev
  gam_fit$hat <- NULL
  ## gam_fit$iter
  gam_fit$linear.predictors <- NULL
  ## gam_fit$method
  ## gam_fit$mgcv.conv
  ## gam_fit$min.edf
  gam_fit$model <- NULL
  ## gam_fit$nsdf
  ## gam_fit$null.deviance
  gam_fit$offset <- NULL
  ## gam_fit$optimizer
  ## gam_fit$pred.formula
  gam_fit$prior.weights <- NULL
  ## gam_fit$pterms
  gam_fit$residuals <- NULL
  ## gam_fit$rV
  ## gam_fit$rank
  ## gam_fit$scale.estimated
  ## gam_fit$sig2
  ## gam_fit$smooth
  ## gam_fit$sp
  ## gam_fit$terms
  ## gam_fit$var.summary
  gam_fit$weights <- NULL
  gam_fit$y <- NULL

  ## summary(gam_fit)
  ## plot(gam_fit)
  ## vis.gam(gam_fit)
  ## gam.check(gam_fit)
  ## predict(gam_fit, newdata = list(x1 = 0, x2 = 0), type = "link", se.fit = TRUE)

  if (gam_fit$converged) {
    data.frame(fit = I(list(gam_fit)))
  } else {
    data.frame(fit = NA)
  }
}

fcst_gam <- function(formula, family, data, new_data, timeout = 300) {
  gam_fit <- tryCatch({
    withTimeout({
      switch(
        family,
        "gaussian" = gam(as.formula(formula), family = gaussian, data = data),
        "scat" = gam(as.formula(formula), family = scat, data = data))
    }, timeout = timeout, onTimeout = "error")
  }, error = function(e) NULL)

  if (!is.null(gam_fit) && gam_fit$converged) {
    fcst <- predict(gam_fit, newdata = new_data, type = "link", se.fit = TRUE)
    bind_cols(new_data, data.frame(fit = fcst$fit, se = fcst$se.fit))
  } else {
    mutate(new_data, fit = NA, se = NA)
  }
}

copy_base_rows <- function(base_row, rows) {
  if (rows == 1) return(base_row)

  for(i in 1:(rows - 1))
    base_row[i + 1,] <- base_row[i, ]
  base_row
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/pead/infra.R")
source(infra_path)


## Variables
lookbacks <- c(500)
formulas  <- c("y ~ s(x1) + s(x2)")
families  <- c("gaussian", "scat")

## lookback <- lookbacks[1]
## ranges   <- get_edate_ranges(lookback)
## range    <- ranges[[2]]
## formula  <- formulas[1]
## family   <- families[1]
## timeout  <- 120


## Loop
prev_plan <- plan(multiprocess(workers = detectCores() - 1))
gam_fcsts <- map_dfr(lookbacks, function(lookback) {
  print(glue("Lookback = {lookback}..."))
  ranges <- get_edate_ranges(lookback)
  future_map_dfr(ranges, function(range) {
    map_dfr(formulas, function(formula) {
      map_dfr(families, function(family) {
        print(glue("{family} {range$first_date} {range$last_date}"))
        data <- filter(e$edata, range$first_date <= date & date < range$last_date)
        new_data <- filter(e$edata, date == range$last_date)
        base_row <- data.frame(date = range$last_date, lookback = lookback,
                               formula = formula, family = family,
                               stringsAsFactors = FALSE)
        if (has_row2(e$gam_fcsts, base_row)) return(NULL)
        base_rows <- copy_base_rows(base_row, nrow(new_data))
        bind_cols(base_rows,
                  fcst_gam(formula, family, data,
                           select(new_data, -date), timeout = 120))
      })
    })
  }, .progress = TRUE)
})
plan(prev_plan)


results <- combine_results("gam_fcsts")
path <- glue("{e$strat_dir}/GAM_Fcsts/GAM_Fcsts_{date}.csv", date = max(results$date))
fwrite(results, path)
