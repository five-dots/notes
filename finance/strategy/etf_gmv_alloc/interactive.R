
source("R/packages.R")
source("R/functions.R")
suppressMessages(library(PerformanceAnalytics))

r_outdated()
r_make()
drake_history()
## clean()

## Data
loadd(data)
data
readd(data_dim_check)
readd(download_path)

## Cache
hash <- drake_history() %>%
  filter(target == "data_dim_check") %>%
  pull(hash) %>% head(1)
cache <- drake_cache()
cache$get_value(hash)


## Plot
r_vis_drake_graph()
loadd(plot_perf_sum)
readd(plot_perf_sum)
plot(plot_perf_sum)

data %>%
  group_by(symbol) %>%
  mutate(cum_ret = Return.) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = cum_ret)) + geom_line(aes(color = symbol))

data %>%
  select(date, symbol, ret) %>%
  pivot_wider(names_from = "symbol", values_from = "ret") %>%
  tk_xts(-date, date) %>%
  PerformanceAnalytics::charts.PerformanceSummary(main = "Performance Summary", geometric = FALSE)
