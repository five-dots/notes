#!/usr/bin/env Rscript

## Functions and Infra Data
fits_lm <- function(formula, data) {
  lm_fit <- lm(as.formula(formula), data)
  g <- broom::glance(lm_fit)
  t <- broom::tidy(lm_fit)
  data.frame(
    llk  = g$logLik,
    aic  = g$AIC,
    coef = I(list(t))
  )
}

infra_path <- glue::glue("{.dropbox}/memo/strategy/pead/infra.R")
source(infra_path)


## Variables
lookbacks <- c(500)
formulas  <- c("y ~ 1", "y ~ x1", "y ~ x1 + x2", "y ~ x1 * x2")

## lookback <- lookbacks[1]
## ranges   <- get_edate_ranges(lookback)
## range    <- ranges[[1]]
## formula  <- formulas[1]


## Loop
prev_plan <- plan(multiprocess(workers = detectCores() - 1))
lm_fits <- map_dfr(lookbacks, function(lookback) {
  print(glue("Lookback = {lookback}..."))
  ranges <- get_edate_ranges(lookback)
  future_map_dfr(ranges, function(range) {
    map_dfr(formulas, function(formula) {
      map_dfr(sides, function(side) {
        data <- filter(e$edata, range$first_date <= date & date < range$last_date)
        base_row <- data.frame(date = range$last_date, lookback = lookback,
                               formula = formula, side = side,
                               stringsAsFactors = FALSE)
        if (has_row2(e$lm_fits, base_row)) return(NULL)
        bind_cols(base_row, fits_lm(formula, data))
      })
    })
  }, .progress = TRUE)
})
plan(prev_plan)


results <- combine_results("lm_fits")
path <- glue("{e$strat_dir}/LM_Fits/LM_Fits_{date}.rds", date = max(results$date))
saveRDS(results, path)

e$edata
