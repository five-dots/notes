### Libraries ###
.base_infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(.base_infra_path)


### Functions ### (. = private, .. = obsoleted)


### Data ###

## Base Variables
## IQFeed Minute は 2007/4/27 - 2019/8/2 まで
## Zacks Earnings は、2009/5/1 から

e$strat_dir  <- glue("{.dropbox}/strat_dev/pre_mkt")
e$first_date <- ymd("2009-07-31")
e$last_date  <- ymd("2019-07-31")

## Data
.symbols <- .get_sp500_symbols()
.types <- c("o2c_log", "c2o_log")
.data <- .get_full_data(.symbols, .types)

e$universe <- .data$universe
e$r  <- .data$o2c_log
e$x1 <- .data$c2o_log
e$x2 <- .get_earnings_flags()

## Linear Model Fits
.lm_fits_old <- tryCatch({
  read_last_file(e$data_dir, "LM_Fits_Old_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$") %>%
    mutate(date = ymd(date))
}, error = function(e) data.frame())
e$lm_fits_old <- .lm_fits_old

.lm_fits <- tryCatch({
  read_last_file(e$data_dir, "LM_Fits_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$") %>%
    mutate(date = ymd(date))
}, error = function(e) data.frame())
e$lm_fits <- .lm_fits
