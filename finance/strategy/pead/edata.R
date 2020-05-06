#!/usr/bin/env Rscript
suppressMessages(library(data.table))
suppressMessages(library(furrr))
suppressMessages(library(glue))
suppressMessages(library(lubridate))
suppressMessages(library(market.data))
suppressMessages(library(tidyverse))
suppressMessages(library(rutils))

.base_infra_path <- glue::glue("{.dropbox}/memo/strategy/infra.R")
source(.base_infra_path)


### Variables ###
all_flags <- zacks_earnings_flags()
invalid_symbols <- c("AM", "JCI", "LSI", "SPB", "TCF", "TRUE")
symbols <- names(all_flags) %>%
  str_replace("\\.", "_") %>% .[!. %in% invalid_symbols] %>% sort()

sd_lookback <- 90
dvol_lookback <- 200
lookbacks <- c(sd_lookback, dvol_lookback)
tdays <- e$ranges[[1]]$last_date

zacks_first_date <- head(index(all_flags), 1)
zacks_last_date <- tail(index(all_flags), 1)
first_date <- tail(tdays[tdays < ymd(zacks_first_date)], max(lookbacks))[1]
last_date <- zacks_last_date


all_data <- quotemedia_daily() %>%
  filter(symbol %in% symbols,
         between(date, first_date, last_date))

prev_plan <- plan(multiprocess(workers = detectCores() - 1))
list_results <- future_map(group_split(all_data, symbol), function(data) {

  ## Extract earnings date
  symbol <- data$symbol[1]
  print(symbol)
  flags  <- all_flags[, str_replace(symbol, "_", "\\.")]
  edates <- index(flags[flags[, 1] == 1, ])
  if (length(edates) == 0) return(NULL)
  lag_edates <- map_dbl(edates, ~ tdays[tail(which(tdays < .x), 1)]) %>%
    as.Date(origin = "1970-01-01")

  ## Minute data
  minutes <- map_dfr(edates, function(edate) {
    m <- iqfeed_minute(symbol, edate, edate, "04:00:00", "09:27:00")
    if (nrow(m) == 0) return(NULL)
    data.frame(date = edate, pre_last = tail(m, 1)$close)
  })
  if (nrow(minutes) == 0) return(NULL)

  ## Calc returns
  d <- left_join(data, minutes, by = "date") %>%
    mutate(adj_ratio = open / adj_open,
           adj_pre_last = pre_last / adj_ratio,
           y = (adj_close - adj_open) / adj_open,
           x1 = (adj_open - lag(adj_close)) / lag(adj_close),
           x1_pre = (adj_pre_last - lag(adj_close)) / lag(adj_close),
           x2 = lag(y)) %>%
    slice(-1)

  ## Extract open, close and volume
  mkt_data <- d %>%
    filter(date %in% edates, !is.na(pre_last)) %>%
    mutate(dvol = round((high + low) / 2 * volume), 0) %>%
    select(date, symbol, y, x1, x1_pre, x2, open, close, volume, dvol)

  ## Calc 90-days moving standard deviations
  sd_ranges <- e$ranges[[as.character(sd_lookback)]] %>%
    filter(last_date %in% lag_edates) %>%
    .transpose_date_df()
  sd <- map2_dfr(sd_ranges, edates, function(range, edate) {
    d %>%
      filter(symbol == !!symbol,
             between(date, range$first_date, range$last_date)) %>%
      summarise(date = !!edate,
                sd_y = sd(y, na.rm = TRUE),
                sd_x1 = sd(x1, na.rm = TRUE))
  }) %>%
    filter(!is.na(sd_y), !is.na(sd_x1),
           ## ゼロ除算を避ける
           sd_y != 0, sd_x1 != 0)

  ## Calc 200-days moving dollar volume
  dvol_ranges <- e$ranges[[as.character(dvol_lookback)]] %>%
    filter(last_date %in% lag_edates) %>%
    .transpose_date_df()
  dvols <- map2_dfr(dvol_ranges, edates, function(range, edate) {
    d %>%
      filter(symbol == !!symbol,
              between(date, range$first_date, range$last_date)) %>%
      summarise(date = !!edate,
                dvol_200 = round(median((high + low) / 2 * volume), 0))
  }) %>%
    filter(!is.na(dvol_200))

  ## Join all data
  inner_join(mkt_data, sd, by = "date") %>%
    inner_join(dvols, by = "date") %>%
    mutate(z1 = x1 / sd_x1, z1_pre = x1_pre / sd_x1, z2 = x2 / sd_y) %>%
    select(date, symbol, y, x1, x1_pre, x2, sd_y, sd_x1, z1, z1_pre, z2,
           open, close, volume, dvol, dvol_200)
}, .progress = TRUE)
plan(prev_plan)

results <- bind_rows(list_results) %>%
  filter(x1_pre != 0, open >= 5, volume != 0, dvol_200 != 0)


path <- glue("{.dropbox}/strat_dev/PEAD/EData/EData_{date}.csv",
             date = max(results$date))
fwrite(results, path)

