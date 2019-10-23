
need_updated <- function(path, universe) {
  if (!file_exists(path)) return(TRUE)

  data <- fread(path, data.table = FALSE)
  end_date <- max(ymd(data$date))
  symbols <- unique(data$symbol)

  nyc_time <- now(tz = "America/New_York")
  last_date <- if_else(hour(nyc_time) >= 17, date(nyc_time), date(nyc_time) - 1)

  not_latest <- end_date < last_date
  not_all_sym <- !setequal(universe, symbols)

  not_latest || not_all_sym
}

download <- function(universe, from, path) {
  data <- map_dfr(universe, function(symbol) {
    tq_get(symbol, from = from) %>%
      mutate(symbol = !!symbol) %>%
      select_at(vars(symbol, everything()))
  })

  fwrite(data, path)
  invisible(path)
}
