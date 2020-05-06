#!/usr/bin/env Rscript

fit_lm <- function(data) {
  lm_fit <- lm(r ~ x1*x2, data = data)
  g <- broom::glance(lm_fit)
  t <- broom::tidy(lm_fit)

  data.frame(
    r2        = g$r.squared,
    pval      = g$p.value,
    llk       = g$logLik,
    aic       = g$AIC,
    i_coef    = t[1, ]$estimate,
    i_pval    = t[1, ]$p.value,
    x1_coef   = t[2, ]$estimate,
    x1_pval   = t[2, ]$p.value,
    x2_coef   = t[3, ]$estimate,
    x2_pval   = t[3, ]$p.value,
    x1x2_coef = t[4, ]$estimate,
    x1x2_pval = t[4, ]$p.value
  )
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/pre_mkt/infra.R")
source(infra_path)


## Variables
universe <- e$universe
lookbacks <- c(500)

symbol   <- "AAPL"
lookback <- 500
ranges   <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
range    <- ranges[[1]]
universe <- "AAPL"


## Loop
prev_plan <- plan(multiprocess)
lm_fits <- future_map_dfr(universe, function(symbol) {
  map_dfr(lookbacks, function(lookback) {
    ranges <- get_monthly_ranges(e$ranges[[as.character(lookback)]])
    map_dfr(ranges, function(range) {
      rng <- glue("{range$first_date}::{range$last_date}")
      print(glue("{symbol} {rng}"))
      data <- data.frame(
        r  = as.numeric(coredata(e$r[rng, symbol])),
        x1 = as.numeric(coredata(e$x1[rng, symbol])),
        x2 = as.numeric(coredata(e$x2[rng, symbol])))
      base_row <- data.frame(symbol = symbol, date = range$last_date,
                             lookback = lookback, stringsAsFactors = FALSE)
      if (has_row(e$lm_fits, base_row)) return(NULL)
      bind_cols(base_row, fit_lm(data))
    })
  })
})
plan(prev_plan)


results <- combine_results("lm_fits")
path <- glue("{e$data_dir}/LM_Fits_{date}.csv",
             date = max(results$date))
write_csv(results, path, na = "")

