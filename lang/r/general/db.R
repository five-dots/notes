
## MSSQL by RODBC ----

channel <- RODBC::odbcDriverConnect(settings$strategy.db)

sql  <- "SELECT * FROM open_gap"

data <- data.frame(
  id = 1,
  p_range = "2009-01-01::2009-12-31",
  p_sma_len = 0,
  p_sd_len = 50,
  p_ato_len = 200,
  p_ogc_len = 10,
  p_side = "Long",
  p_sd_l_thres = 0.01,
  p_sd_h_thres = 0.05,
  p_ato_l_thres = 10000000,
  p_ato_h_thres = 100000000,
  p_ogc_thres = 0,
  p_stop_thres = 0.3,
  p_min_thres = 10,
  p_slippage = 0.001,
  p_num_trades = 10,
  p_lot = 10000,
  r_ttl_win = 100000,
  r_ttl_lose = 50000,
  r_drawdown = -20000,
  r_win_days = 150,
  r_lose_days = 200
)

data <- data.frame(
  id = 1,
  p_range = "2009-01-01::2009-12-31",
  p_sma_len = 10
)

ColumnsOfTable       <- sqlColumns(channel, "test")
varTypes             <- as.character(ColumnsOfTable$TYPE_NAME) 
names(varTypes)      <- as.character(ColumnsOfTable$COLUMN_NAME) 
colnames(data) <- as.character(ColumnsOfTable$COLUMN_NAME)

typeInfo <- c("Integer", "character", "double")
typeInfo <- c("numeric", "character", "numeric")
names(typeInfo) <- colnames(data)

RODBC::sqlSave(channel, data, tablename = "test", rownames = FALSE,
               typeInfo = typeInfo, append = TRUE, verbose = TRUE)

RODBC::sqlUpdate(channel, data, tablename = "test")
RODBC::sql

RODBC::sqlQuery(channel, sql, stringsAsFactors = FALSE)

odbcClose(channel)

## SQLite by RODBC ----

library(RODBC)
library(RSQLite)

channel <- odbcDriverConnect(settings$sharadar.db)
channel <- odbcDriverConnect(settings$quotemedia.db)
channel <- odbcDriverConnect(settings$trade.db)
channel <- odbcDriverConnect(settings$strat.result.db)

sql  <- "SELECT [Ticker] FROM DailyOHLCVs"
sql  <- "SELECT * FROM IBInstruments"
sql  <- "SELECT * FROM tickers"

sql  <- "SELECT * FROM DailyOHLCVs"
sql  <- "SELECT [Range],[SDLength],[AvgToverLength],
                [Side],[ToverThres],[StopThres],[MinEntryThres]
        FROM GapStrategy01"

result <- sqlQuery(channel, sql, stringsAsFactors = FALSE) %>% data.table()

odbcClose(channel)

insts[Symbol == ""]
insts[Exchange == ""]
insts[CompanyName == ""]
insts[is.na(RefreshedAt)]
insts[is.na(FromDate)]
insts[is.na(ToDate)]

insts <- sqlQuery(channel, "SELECT * FROM Instruments") %>% data.table()
insts <- insts[, .(Id, Symbol)]

items <- sqlQuery(channel, "SELECT * FROM Items")
items <- data.table(items)[, .(Id, Name)]

result$Ticker %>% unique() %>% str_length()

write.csv(result, "./hoge.csv", row.names = FALSE)

packageVersion("devtools")
