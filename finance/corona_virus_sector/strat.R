
library(tidyquant)
library(tidyverse)

sectors <- tribble(
  ~ symbol, ~ name,
  "XLB",    "Materials",
  "XLC",    "Communication Service",
  "XLE",    "Energy",
  "XLF",    "Finance",
  "XLI",    "Industrial",
  "XLK",    "Technology",
  "XLP",    "Consumer Staples",
  "XLRE",   "Real Estate",
  "XLU",    "Utilities",
  "XLV",    "Health Care",
  "XLY",    "Consumer Discretionary",
)

from <- date("2020-02-23")

raw_data <- tq_get(sectors$symbol, from = from) %>%
  dplyr::left_join(sectors, by = "symbol") %>%
  select(symbol, name, everything())

data <- raw_data %>%
  group_by(symbol) %>%
  ## mutate(ret = (adjusted - lag(adjusted)) / lag(adjusted)) %>%
  mutate(ret = (close - lag(open)) / lag(open)) %>%
  slice(-1) %>%
  ungroup()

strat <- data %>%
  group_by(symbol, name) %>%
  summarise(
    last = adjusted[length(adjusted)],
    avg = mean(ret),
    sd = sd(ret)
  )
strat$rel_ret <- strat$avg - min(strat$avg)
strat$rel_sharpe <- strat$rel_ret / strat$sd
strat$position <- strat$rel_sharpe - mean(strat$rel_sharpe)

total_dollar <- 50000
strat$value <- strat$position * total_dollar
strat$qty <- round(strat$value / strat$last)
strat$expected <- strat$value * strat$avg

strat <- strat %>% arrange(desc(rel_sharpe))
strat


library(RiskPortfolios)

mat <- data %>%
  select(symbol, date, ret) %>%
  pivot_wider(names_from = symbol, values_from = ret) %>%
  select(-date) %>%
  as.matrix()

cov <- cov(mat)
ret  <- apply(mat, 2, mean)
