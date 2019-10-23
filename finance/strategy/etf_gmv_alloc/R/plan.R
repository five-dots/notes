plan <- drake_plan(

  ### Global variables
  universe = c("QQQ", "VNQ", "TLT", "GLD"),
  from = ymd("2005-07-29"),
  dir = "~/Dropbox/repos/github/five-dots/notes/finance/strategy/etf_gmv_alloc",
  path = glue("{dir}/data/data.csv"),

  ## Data (recipes, rsample, assertr)
  download_path = target(download(universe, from, path),
                         trigger = trigger(condition = need_updated(path, universe))),

  raw_data = target(fread(download_path, data.table = FALSE),
                    trigger = trigger(change = md5(download_path))),

  data = raw_data %>%
    mutate(date = ymd(date)) %>%
    group_by(symbol) %>%
    mutate(ret = (adjusted - lag(adjusted)) / lag(adjusted),
           ret_log = log(adjusted) - lag(log(adjusted))) %>%
    slice(-1) %>%
    ungroup(),

  data_dim_check = data %>%
    group_by(symbol) %>%
    summarise(count = n(), start = min(date), end = max(date)),

  ### Plots

  ### Model fits (parsnip + tune)

  ### Performance (infer, yardstick)

  ### Report
)
