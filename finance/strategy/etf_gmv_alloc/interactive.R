
source("R/packages.R")
source("R/functions.R")

r_outdated()
r_make()
drake_history()
## clean()


## Cache
hash <- drake_history() %>%
  filter(target == "data_dim_check") %>%
  pull(hash) %>% head(1)
cache <- drake_cache()
cache$get_value(hash)


## Data
loadd(data)
data
readd(data_dim_check)
readd(download_path)


## Plot
r_vis_drake_graph()
readd(perf_summary)

rolling_origin(data, initial = 250, assess = 30, skip = 30, cumulative = FALSE)
