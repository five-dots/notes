
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

plot_perf_summary <- function(data) {
  d <- suppressWarnings(
    data %>%
    group_by(symbol) %>%
    mutate(cum_ret = cumprod(1 + ret) - 1,
           drawdown = Drawdowns(ret),
           label = if_else(date == max(date), symbol, NA_character_)) %>%
    ungroup())

  range <- c(min(data$date), max(data$date) + days(150))

  perf <- ggplot(d, aes(x = date, y = cum_ret, color = symbol)) +
    geom_line() +
    geom_label_repel(aes(label = label), na.rm = TRUE, xlim = c(max(data$date), NA), size = 3) +
    scale_x_date(limits = range) +
    theme(axis.title = element_blank(), legend.position = "none") +
    ggtitle("Cumulative Returns")

  ddown <- ggplot(d, aes(x = date, y = drawdown, color = symbol)) +
    geom_line() +
    geom_label_repel(aes(label = label), na.rm = TRUE, xlim = c(max(data$date), NA), size = 3) +
    scale_x_date(limits = range) +
    theme(axis.title = element_blank(), legend.position = "none") +
    ggtitle("Drawdowns")

  perf + ddown + plot_layout(ncol = 1, heights = c(2, 1))
}
