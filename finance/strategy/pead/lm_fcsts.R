#!/usr/bin/env Rscript

## Functions and Infra Data
fcst_lm <- function(formula, data, new_data, timeout = 300) {
  lm_fit <- lm(as.formula(formula), data = data)
  fcst <- predict(lm_fit, newdata = new_data, se.fit = TRUE)
  bind_cols(new_data, data.frame(fit = fcst$fit, se = fcst$se.fit))
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
formulas  <- c("y ~ x1 + x2")

## lookback <- lookbacks[1]
## ranges   <- get_edate_ranges(lookback)
## range    <- ranges[[2]]
## formula  <- formulas[1]
## timeout  <- 300


## Loop
prev_plan <- plan(multiprocess(workers = detectCores() - 1))
lm_fcsts <- map_dfr(lookbacks, function(lookback) {
  print(glue("Lookback = {lookback}..."))
  ranges <- get_edate_ranges(lookback)
  future_map_dfr(ranges, function(range) {
    map_dfr(formulas, function(formula) {
      data     <- filter(e$edata, range$first_date <= date & date < range$last_date)
      new_data <- filter(e$edata, date == range$last_date)
      base_row <- data.frame(date = range$last_date, lookback = lookback,
                              formula = formula, stringsAsFactors = FALSE)
      if (has_row2(e$lm_fcsts, base_row)) return(NULL)
      base_rows <- copy_base_rows(base_row, nrow(new_data))
      bind_cols(base_rows, fcst_lm(formula, data, select(new_data, -date)))
    })
  }, .progress = TRUE)
})
plan(prev_plan)


results <- combine_results("lm_fcsts")
path <- glue("{e$strat_dir}/LM_Fcsts/LM_Fcsts_{date}.csv", date = max(results$date))
fwrite(results, path)
