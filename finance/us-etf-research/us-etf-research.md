米国の ETF について、各資産クラスの代表的な銘柄をリストアップし、主要なパフォーマンス指標をまとめてみたい。データの取得・作図・パフォーマンス指標の計算は、全て R を使って行う。

# ライブラリの読み込み

まずは、今回の記事に必要なライブラリを読み込みから。

```R
library(tidyverse)

## 株価の取得
library(tidyquant)

## data.frame から xts 含む時系列データへの変換 (またはその逆)
library(timetk)

## パフォーマンス指標の計算
library(PerformanceAnalytics)

## ggplot2 関連
library(GGally)
library(ggrepel)
library(patchwork)

## データの表示の体裁を整えるパッケージ
library(formattable)
```

# データ

この記事で利用するデータや関数は全て以下の `RData` ファイルにまとめらている。

[ ダウンロード `us-etf-research.RData` ](https://dl.dropboxusercontent.com/s/croimqstksosbmp/us-etf-research.RData)

以降の記事で、全ての関数やデータのダウンロード方法をコード付きで解説するが、このファイルダウンロードし読み込めば、手元で環境を再現することが可能だ。

```R
load("us-etf-research.RData")
```

# ETF リスト

分析対象の ETF を `tibble` 形式でリストアップする。[ETFdb](https://etfdb.com/) を参考に、主要なインデックスから、資産規模 (AUM) が大きく、上場からの期間が長いものを選択している。信託報酬などのコストの多寡は考慮していない。

これは、もちろん網羅的なリストではなく、それを目指したものでもないが、今後順次増やしていく予定だ。

```R
etfs <- tribble(
  ~class,        ~symbol,  ~index,
  "Equity",      "SPY",    "S&P 500",
  "Equity",      "IJH",    "S&P MidCap 400",
  "Equity",      "IJR",    "S&P SmallCap 600",
  "Equity",      "IWB",    "Russel 1000",
  "Equity",      "IWM",    "Russel 2000",
  "Equity",      "QQQ",    "NASDAQ 100",
  "Equity",      "DIA",    "Dow 30",
  "US Sector",   "XLE",    "Energy",
  "US Sector",   "XLF",    "Financials",
  "US Sector",   "XLU",    "Utilities",
  "US Sector",   "XLI",    "Industrial",
  "US Sector",   "XLK",    "Technology",
  "US Sector",   "XLV",    "Health",
  "US Sector",   "XLY",    "Consumer Discretionary",
  "US Sector",   "XLP",    "Consumer Staples",
  "US Sector",   "XLB",    "Materials",
  "Treasury",    "SHV",    "Short Term",
  "Treasury",    "SHY",    "1-3 Year",
  "Treasury",    "IEF",    "7-10 Year",
  "Treasury",    "TLT",    "20+ Year",
  "Treasury",    "TIP",    "Inflation Protected",
  "Corp Bond",   "LQD",    "Investment Grade",
  "Corp Bond",   "HYG",    "High Yield",
  "Corp Bond",   "PFF",    "Preffered Stock",
  "Real Estate", "IYR",    "Dow Jones U.S. Real Estate",
  "Commodity",   "GLD",    "Gold",
  "Commodity",   "SLV",    "Silver",
  "Commodity",   "USO",    "Crude Oil",
  "Forex",       "UUP",    "USD",
  "Forex",       "FXY",    "JPY",
  "Forex",       "FXE",    "EUR",
  "Forex",       "FXB",    "GBP",
  "Forex",       "FXF",    "CHF",
  "Volatility",  "VXX",    "S&P 500 VIX Short-Term Future"
)
```

# データのダウンロード

## Yahoo Finance からダウンロード

`{tidyquant}` を利用して [Yahoo Finance](https://finance.yahoo.com/) からデータを取得する。 `{tidyquant}` は Tidy な `data.frame` として株価をダウンロードしてくれるので便利だ。

R から Yahoo Finance のデータを読み込む際には、一般的に、[ `{quantmod}` ](https://github.com/joshuaulrich/quantmod)が利用されることが多いようだ。しかし `{quantmod}` では、 `xts` でデータが取得されるため `{tidyverse}` のパッケージ群を利用することができない。個人的には、データは `data.frame` (`tibble 含む`) に統一しておき `{dplyr}` や `{ggplot2}` などのパッケージの関数を扱えるようにしておいたほうが、快適にデータ分析を進められると思う。

ただし、一部の時系列分析用のパッケージなどでは、入力に `xts` を求めるものがあるので、その場合は都度変換して対処する方針だ。今回も `{PerformanceAnalytics}` を利用する際に `xts` に変換する処理が必要になる。

それでは、先程用意した ETF リストの銘柄の株価を全て取得してみよう。

```R
rawdata <- tq_get(etfs$symbol, from = "1993-01-01")
```

データは、典型的な四本値 + 分割・配当調整済みの終値 (adjusted) という構成だ。

```R
head(rawdata) %>% mutate_if(is.numeric, round, digits = 2)
```

| symbol | date       | open  | high  | low   | close | volume  | adjusted |
|------ |---------- |----- |----- |----- |----- |------- |-------- |
| SPY    | 1993-01-29 | 43.97 | 43.97 | 43.75 | 43.94 | 1003200 | 26.58    |
| SPY    | 1993-02-01 | 43.97 | 44.25 | 43.97 | 44.25 | 480500  | 26.77    |
| SPY    | 1993-02-02 | 44.22 | 44.38 | 44.12 | 44.34 | 201300  | 26.83    |
| SPY    | 1993-02-03 | 44.41 | 44.84 | 44.38 | 44.81 | 529400  | 27.11    |
| SPY    | 1993-02-04 | 44.97 | 45.09 | 44.47 | 45    | 531500  | 27.23    |
| SPY    | 1993-02-05 | 44.97 | 45.06 | 44.72 | 44.97 | 492100  | 27.21    |

## VXX を結合

VIX 短期先物の ETN として有名な VXX は、2019 年 1 月 30 日に償還を迎えてしまっている。後継となる銘柄が VXXB として登場しているが、まだ 2 年弱しか取引されておらず、分析対象として不十分である。そのため、VXX のデータを別途 csv で用意し、Yahoo からダウンロードした最新の情報と結合する。(ちなみに、VXXB はその後、元々と同じ VXX に名称変更された。上述の ETF リストには、名称変更後の VXX を指定している。この方が、旧 VXX と結合する際にも好都合である。)

[ ダウンロード `VXX.csv` ](https://dl.dropboxusercontent.com/s/v8gykgcb26fzkqr/VXX.csv)

```R
vxx <- read_csv("VXX.csv") %>% mutate(date = ymd(date))
rawdata <- rawdata %>%
  filter(!(symbol == "VXX" & date <= ymd("2019-01-29"))) %>%
  bind_rows(vxx) %>%
  arrange(symbol, date)
```

## 日次リターンを計算

分析対象となる日次リターンを計算する。

```R
data <- rawdata %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, mutate_fun = dailyReturn, col_rename = "ret") %>%
  slice(-1) %>% # リターン計算による先頭 0 を削除
  ungroup() %>%
  left_join(etfs, by = "symbol") # インデックスでもアクセスできるように結合しておく
```

## データの確認

データの前処理が完了したタイミングで、利用可能期間や `NA` の有無などを確認しておく。

```R
data %>%
  group_by(symbol, index) %>%
  summarize(start = min(date), end = max(date), na = sum(is.na(ret)))
```

|    | symbol | index                         | start      | end        | na |
|--- |------ |----------------------------- |---------- |---------- |--- |
| 1  | DIA    | Dow 30                        | 1998-01-21 | 2019-11-12 | 0  |
| 2  | FXB    | GBP                           | 2006-06-27 | 2019-11-12 | 0  |
| 3  | FXE    | EUR                           | 2005-12-13 | 2019-11-12 | 0  |
| 4  | FXF    | CHF                           | 2006-06-27 | 2019-11-12 | 0  |
| 5  | FXY    | JPY                           | 2007-02-14 | 2019-11-12 | 0  |
| 6  | GLD    | Gold                          | 2004-11-19 | 2019-11-12 | 0  |
| 7  | HYG    | High Yield                    | 2007-04-12 | 2019-11-12 | 0  |
| 8  | IEF    | 7-10 Year                     | 2002-07-31 | 2019-11-12 | 0  |
| 9  | IJH    | S&P MidCap 400                | 2000-05-30 | 2019-11-12 | 0  |
| 10 | IJR    | S&P SmallCap 600              | 2000-05-30 | 2019-11-12 | 0  |
| 11 | IWB    | Russel 1000                   | 2000-05-22 | 2019-11-12 | 0  |
| 12 | IWM    | Russel 2000                   | 2000-05-30 | 2019-11-12 | 0  |
| 13 | IYR    | Dow Jones U.S. Real Estate    | 2000-06-20 | 2019-11-12 | 0  |
| 14 | LQD    | Investment Grade              | 2002-07-31 | 2019-11-12 | 0  |
| 15 | PFF    | Preffered Stock               | 2007-04-02 | 2019-11-12 | 0  |
| 16 | QQQ    | NASDAQ 100                    | 1999-03-11 | 2019-11-12 | 0  |
| 17 | SHV    | Short Term                    | 2007-01-12 | 2019-11-12 | 0  |
| 18 | SHY    | 1-3 Year                      | 2002-07-31 | 2019-11-12 | 0  |
| 19 | SLV    | Silver                        | 2006-05-01 | 2019-11-12 | 0  |
| 20 | SPY    | S&P 500                       | 1993-02-01 | 2019-11-12 | 0  |
| 21 | TIP    | Inflation Protected           | 2003-12-08 | 2019-11-12 | 0  |
| 22 | TLT    | 20+ Year                      | 2002-07-31 | 2019-11-12 | 0  |
| 23 | USO    | Crude Oil                     | 2006-04-11 | 2019-11-12 | 0  |
| 24 | UUP    | USD                           | 2007-03-02 | 2019-11-12 | 0  |
| 25 | VXX    | S&P 500 VIX Short-Term Future | 2009-02-02 | 2019-11-12 | 0  |
| 26 | XLB    | Materials                     | 1998-12-23 | 2019-11-12 | 0  |
| 27 | XLE    | Energy                        | 1998-12-23 | 2019-11-12 | 0  |
| 28 | XLF    | Financials                    | 1998-12-23 | 2019-11-12 | 0  |
| 29 | XLI    | Industrial                    | 1998-12-23 | 2019-11-12 | 0  |
| 30 | XLK    | Technology                    | 1998-12-23 | 2019-11-12 | 0  |
| 31 | XLP    | Consumer Staples              | 1998-12-23 | 2019-11-12 | 0  |
| 32 | XLU    | Utilities                     | 1998-12-23 | 2019-11-12 | 0  |
| 33 | XLV    | Health                        | 1998-12-23 | 2019-11-12 | 0  |
| 34 | XLY    | Consumer Discretionary        | 1998-12-23 | 2019-11-12 | 0  |

# 関数リスト

記事内で利用する関数を読み込む。現時点で全てを理解する必要はなく、以降の記事で登場してきたタイミングで見返してもらえればよい。 `{ggplot2}` のテーマである `theme_my()` 以外の関数は全て、引数に前述までに用意した `data` を受け取る関数である。

```R
## 複数の銘柄の中で、最も遅い取引開始日を返す
get_start_date <- function(data) {
  data %>%
    group_by(symbol) %>%
    summarise(start_date =  min(date)) %>%
    pull(start_date) %>%
    max()
}

## get_start_date() の日付に全ての銘柄の開始日を合わせる
align_start_date <- function(data) {
  start_date <- get_start_date(data)
  filter(data, date >= start_date)
}

## 累積リターンとドローダウンを追加する
add_cumret_dd <- function(data) {
  ## PerformanceAnalytics::Drawdowns() 関数の warning を抑制
  suppressWarnings(
    data %>%
      group_by(symbol) %>%
      mutate(cum_ret = cumprod(1 + ret) - 1,
             drawdown = Drawdowns(ret)) %>%
      ungroup()
  )
}

## data.frame から xts へ変換
convert_to_xts <- function(data, name_col, value_col) {
  name_col <- enquo(name_col)
  value_col <- enquo(value_col)
  data %>%
    select(date, !!name_col, !!value_col) %>%
    pivot_wider(names_from = !!name_col, values_from = !!value_col) %>%
    tk_xts(-date, date)
}

## 凡例なしのシンプルな ggplot2 テーマ
theme_my <- function() {
  theme_light() + theme(
    plot.title = element_text(face = "bold.italic"),
    axis.title = element_blank(),
    legend.position = "none")
}

## 線グラフをプロット (累積リターン・ドローダウン・リターンで共通して利用)
plot_lines <- function(data, y_var, color_var, alpha = 0.6, offset = 0.2, label = TRUE) {
  y_var <- enquo(y_var)
  color_var <- enquo(color_var)

  min_date <- min(data$date)
  max_date <- max(data$date)
  range <- c(min_date, max_date + ((max_date - min_date) * offset))

  p <- data %>%
    group_by(!!color_var) %>%
    mutate(label = if_else(date == max(date), !!color_var, NA_character_)) %>%
    ggplot(aes(x = date, y = !!y_var, color = !!color_var)) +
    geom_line(alpha = alpha) +
    scale_x_date(limits = range) +
    theme_my()

  if (label) {
    p + geom_label_repel(aes(label = label), na.rm = TRUE, xlim = c(max_date, NA), size = 3)
  } else {
    p
  }
}

## {patchwork} を利用して複数の ggplot を 1 つに結合
plot_perf_summary <- function(data, alpha = 0.6, offset = 0.2) {
  p1 <- plot_lines(data, cum_ret, index, alpha, offset) +
    ggtitle("Cumulative Returns")

  p2 <- plot_lines(data, drawdown, index, alpha, offset, label = FALSE) +
    ggtitle("Drawdowns")

  p3 <- plot_lines(data, ret, index, alpha, offset, label = FALSE) +
    ggtitle("Daily Returns")

  p1 + p2 + p3 + plot_layout(ncol = 1, heights = c(2, 1, 1))
}

## {GGally} で散布図行列
plot_ggpairs <- function(data) {
  ## ベンチマークとの相関が高い順に並べる
  cors <- map_dfr(unique(data$symbol), function(symbol) {
    d1 <- filter(data, symbol == !!symbol) %>% pull(ret)
    d2 <- filter(data, symbol == !!benchmark) %>% pull(ret)
    cor <- cor(d1, d2)
    data.frame(symbol = symbol, cor = cor, stringsAsFactors = FALSE)
  })
  symbol_order <- arrange(cors, -cor) %>% pull(symbol)

  data %>%
    select(symbol, date, ret) %>%
    pivot_wider(names_from = symbol, values_from = ret) %>%
    select(-date) %>%
    ggpairs(aes(alpha = 0.1), columns = symbol_order, lower = list(continuous = "smooth")) +
    theme_my()
}

## 銘柄毎のパフォーマンス指標を計算して data.frame で返す
calc_perf_metrices <- function(data, etfs, benchmark = "SPY") {
  xts_data <- convert_to_xts(data, symbol, ret)
  rb <- xts_data[, benchmark]

  map_dfr(names(xts_data), function(symbol) {
    ra <- xts_data[, symbol]
    data.frame(
      Symbol  = symbol,
      Index   = etfs[etfs$symbol == symbol, ]$index,
      Sharpe  = round(as.numeric(SharpeRatio(ra, annualize = TRUE, FUN = "StdDev")), 2),
      Alpha   = round(CAPM.alpha(ra, rb), 5),
      Beta    = round(CAPM.beta(ra, rb), 2),
      Avg_Ret = as.numeric(Return.annualized(ra)),
      Cum_Ret = as.numeric(Return.cumulative(ra)),
      StdDev  = as.numeric(StdDev.annualized(ra)),
      MaxDD   = as.numeric(maxDrawdown(ra)),
      stringsAsFactors = FALSE
    )
  }) %>%
    mutate_at(vars(Avg_Ret, Cum_Ret, StdDev, MaxDD), percent, digits = 1) %>%
    arrange(-Sharpe)
}
```

# 資産クラス間の相関係数

ETF を分析する目的は、様々な資産クラスへの分散投資にるリスク分散だ。まず、株式・米国債・不動産・コモディティ・ボラティリティの 5 つの資産クラスから代表的な銘柄を選んで、相関関係を見てみる。

```R
asset_class <- list()
asset_class$symbols <- c("SPY", "IYR", "GLD", "TLT", "VXX")
asset_class$data <- data %>%
  filter(symbol %in% asset_class$symbols) %>%
  align_start_date()

asset_class$cor_plot <- plot_ggpairs(asset_class$data)
asset_class$cor_plot
```

![img](https://dl.dropboxusercontent.com/s/68v4nk09fcm3ywc/us-etf-research_asset-class-cor.png)

株式 (SPY) との関係でみると、

-   不動産 (IYR) は **「強い正の相関」**
-   金 (GLD) は **「無相関」**
-   米国債 (TLT) は **「負の相関」**
-   ボラティリティ (VXX) は **「強い負の相関」**

という、概ね想像通りの結果になった。

なお、資産クラス間の相関関係は、常に一定ではなく、時期によって変動する。特に金融危機時には、多くの資産が正の相関になることが知られている。こうした時変的な相関係数は、DCC モデルなどによってモデル化することができるが、この記事では触れない。

# 各資産クラス毎の分析

以降で、淡々と各資産クラスごとのパフォーマンスを見ていく。パフォーマンスのプロットは `PerformanceAnalytics::charts.PerformanceSummary` を参考に `{ggplot2}` で作成したものである。

パフォーマンス指標の説明は以下の通りだ。これらも順次増やしていきたい。

| 指標      | 説明          |
|--------- |------------- |
| `Sharpe`  | シャープレシオ (年換算) |
| `Alpha`   | CAPM アルファ |
| `Beta`    | CAPM ベータ   |
| `Avg_Ret` | 年率リターン  |
| `Cum_Ret` | 累積リターン  |
| `StdDev`  | 標準偏差 (年換算) |
| `MaxDD`   | 最大ドローダウン |

# エクイティ ETFs

## インデックスの種類

S&P 指数とラッセル指数は、階層構造になっていて、若干覚えにくいため、ここに整理しておく。

-   S&P US Index
    -   S&P Composite 1500
        -   **S&P 500**
        -   **S&P MidCap 400**
        -   **S&P SmallCap 600**
    -   S&P 900 (Upper)
        -   **S&P 500**
        -   **S&P MidCap 400**
    -   S&P 1000 (Lower)
        -   **S&P MidCap 400**
        -   **S&P SmallCap 600**

-   Russel US Index
    -   Russel 3000
        -   **Russel 1000** (Upper)
        -   **Russel 2000** (Lower)

## データの準備

```R
equity <- list()
equity$data <- data %>%
  filter(class == "Equity") %>%
  align_start_date() %>%
  add_cumret_dd()
```

## パフォーマンス (プロット)

```R
equity$perf_plot <- plot_perf_summary(equity$data, offset = 0.15)
equity$perf_plot
```

![img](https://dl.dropboxusercontent.com/s/sl8sk3wy61nbblg/us-etf-research_equity-perf.png)

## パフォーマンス (テーブル)

```R
equity$perf_table <- calc_perf_metrices(equity$data, etfs, benchmark = "SPY")
equity$perf_table
```

    
      Symbol            Index Sharpe    Alpha Beta Avg_Ret Cum_Ret StdDev MaxDD
    1    IJH   S&P MidCap 400   0.45  0.00012 1.01    9.3%  458.9%  20.5% 55.1%
    2    IJR S&P SmallCap 600   0.45  0.00015 1.04   10.0%  532.6%  22.0% 58.2%
    3    DIA           Dow 30   0.43  0.00007 0.92    7.6%  317.0%  17.9% 51.9%
    4    IWB      Russel 1000   0.35  0.00002 0.97    6.4%  234.5%  18.6% 55.4%
    5    IWM      Russel 2000   0.35  0.00007 1.10    8.0%  348.3%  23.1% 58.6%
    6    SPY          S&P 500   0.33  0.00000 1.00    6.2%  223.3%  18.8% 55.2%
    7    QQQ       NASDAQ 100   0.22 -0.00001 1.18    5.7%  195.5%  26.2% 80.5%

## 相関係数

```R
equity$cor_plot <- plot_ggpairs(equity$data)
equity$cor_plot
```

![img](https://dl.dropboxusercontent.com/s/esrpqq7mssgxxrb/us-etf-research_equity-cor.png)

# セクター ETFs

## セクターの分類

セクターの分類方法にはいくつかの種類があるが、ここでは [GICS Sector](https://www.msci.com/gics) の分類に従った [Select Sector SPDR ETFs](http://www.sectorspdr.com/sectorspdr/) シリーズの ETF を利用する。現在 11 のセクターに分類されているが、XLRE と XLC は近年追加されたものであるため、今回は除外する。

| シンボル | セクター               | 主要銘柄        | メモ              |
|---- |---------------------- |--------------- |----------------- |
| XLB  | Materials              | LIN, DD, ECL    |                   |
| XLE  | Energy                 | XOM, CVX, COP   |                   |
| XLF  | Finance                | BRK.B, JPM, BAC |                   |
| XLI  | Industrial             | BA, HON, UNP    |                   |
| XLK  | Technology             | MSFT, AAPL, V   |                   |
| XLP  | Consumer Staples       | PG, KO, PEP     | 生活必需品        |
| XLU  | Utilities              | NEE, DUK, D     |                   |
| XLV  | Health                 | JNJ, PFE, UNH   |                   |
| XLY  | Consumer Discretionary | AMZN, HD, MCD   | 一般消費財        |
| XLRE | Real Estate            | AMT, CCI, PLD   | 2015-10-08 から取引開始 |
| XLC  | Communication Service  | FB, GOOG, DIS   | 2018-11-03 から取引開始 |

## データの準備

```R
us_sector <- list()
us_sector$data <- data %>%
  filter(class == "US Sector" | symbol == "SPY") %>% # ベンチマークとして SPY を含める
  align_start_date() %>%
  add_cumret_dd()
```

## パフォーマンス (プロット)

```R
us_sector$perf_plot <- plot_perf_summary(us_sector$data, offset = 0.15)
us_sector$perf_plot
```

![img](https://dl.dropboxusercontent.com/s/ftf84ex991loua7/us-etf-research_us-secotr-perf.png)

## パフォーマンス (テーブル)

```R
us_sector$perf_table <- calc_perf_metrices(us_sector$data, etfs, benchmark = "SPY")
us_sector$perf_table
```

       Symbol                  Index Sharpe    Alpha Beta Avg_Ret Cum_Ret StdDev MaxDD
    1     XLV                 Health   0.45  0.00014 0.73    8.1%  407.6%  17.8% 39.2%
    2     XLP       Consumer Staples   0.43  0.00012 0.53    6.4%  266.1%  14.9% 35.9%
    3     XLY Consumer Discretionary   0.41  0.00012 0.97    9.1%  509.5%  21.8% 59.0%
    4     XLI             Industrial   0.39  0.00008 0.96    8.1%  410.5%  20.8% 62.3%
    5     XLU              Utilities   0.39  0.00016 0.58    7.3%  330.8%  18.4% 52.3%
    6     SPY                S&P 500   0.34  0.00000 1.00    6.5%  274.6%  19.0% 55.2%
    7     XLB              Materials   0.32  0.00009 0.96    7.6%  363.1%  23.8% 59.8%
    8     XLE                 Energy   0.25  0.00009 0.97    6.7%  288.0%  26.8% 57.4%
    9     XLK             Technology   0.24  0.00000 1.14    6.1%  242.8%  25.4% 82.0%
    10    XLF             Financials   0.18 -0.00002 1.28    5.5%  207.4%  30.2% 82.7%

## 相関係数

```R
us_sector$cor_plot <- plot_ggpairs(us_sector$data)
us_sector$cor_plot
```

![img](https://dl.dropboxusercontent.com/s/rcl500stjigjty5/us-etf-research_us-sector-cor.png)

# 米国債 ETFs

## データの準備

```R
treasury <- list()
treasury$data <- data %>%
  filter(class == "Treasury" | symbol == "SPY") %>%
  align_start_date() %>%
  add_cumret_dd()
```

## パフォーマンス (プロット)

```R
treasury$perf_plot <- plot_perf_summary(treasury$data, offset = 0.12)
treasury$perf_plot
```

![img](https://dl.dropboxusercontent.com/s/97puju5upnibvle/us-etf-research_treasury-perf.png)

## パフォーマンス (テーブル)

```R
treasury$perf_table <- calc_perf_metrices(treasury$data, etfs, benchmark = "SPY")
treasury$perf_table
```

      Symbol               Index Sharpe   Alpha  Beta Avg_Ret Cum_Ret StdDev MaxDD
    1    SHV          Short Term   2.72 0.00004  0.00    1.0%   13.8%   0.4%  0.4%
    2    SHY            1-3 Year   1.42 0.00009 -0.03    1.9%   27.6%   1.4%  2.2%
    3    IEF           7-10 Year   0.75 0.00026 -0.15    5.0%   87.2%   6.7% 10.4%
    4    TIP Inflation Protected   0.66 0.00019 -0.07    4.0%   66.2%   6.1% 14.4%
    5    TLT            20+ Year   0.48 0.00043 -0.32    6.8%  131.6%  14.2% 26.6%
    6    SPY             S&P 500   0.43 0.00000  1.00    8.4%  181.9%  19.4% 55.2%

## 相関係数

```R
treasury$cor_plot <- plot_ggpairs(treasury$data)
treasury$cor_plot
```

![img](https://dl.dropboxusercontent.com/s/iqevu86hgywb8gj/us-etf-research_treasury-cor.png)

# 社債 ETFs

## データの準備

```R
corp_bond <- list()
corp_bond$data <- data %>%
  filter(class == "Corp Bond" | symbol == "SPY") %>%
  align_start_date() %>%
  add_cumret_dd()
```

## パフォーマンス (プロット)

```R
corp_bond$perf_plot <- plot_perf_summary(corp_bond$data, offset = 0.15)
corp_bond$perf_plot
```

![img](https://dl.dropboxusercontent.com/s/ic05jm92t8nz29b/us-etf-research_corp-bond-perf.png)

## パフォーマンス (テーブル)

```R
corp_bond$perf_table <- calc_perf_metrices(corp_bond$data, etfs, benchmark = "SPY")
corp_bond$perf_table
```

      Symbol            Index Sharpe   Alpha Beta Avg_Ret Cum_Ret StdDev MaxDD
    1    LQD Investment Grade   0.69 0.00021 0.04    5.6%   98.5%   8.1% 21.5%
    2    HYG       High Yield   0.46 0.00008 0.38    5.4%   92.7%  11.6% 34.2%
    3    SPY          S&P 500   0.43 0.00000 1.00    8.4%  177.2%  19.5% 55.2%
    4    PFF  Preffered Stock   0.21 0.00002 0.57    4.2%   67.8%  20.2% 65.5%

## 相関係数

```R
corp_bond$cor_plot <- plot_ggpairs(corp_bond$data)
corp_bond$cor_plot
```

![img](https://dl.dropboxusercontent.com/s/hett4zev4ljbg64/us-etf-research_corp-bond-cor.png)

# 不動産 ETFs

## データの準備

```R
real_estate <- list()
real_estate$data <- data %>%
  filter(class == "Real Estate" | symbol == "SPY") %>%
  align_start_date() %>%
  add_cumret_dd()
```

## パフォーマンス (プロット)

```R
real_estate$perf_plot <- plot_perf_summary(real_estate$data, offset = 0.2)
real_estate$perf_plot
```

![img](https://dl.dropboxusercontent.com/s/kj5rrxuywgux5i3/us-etf-research_real-estate-perf.png)

## パフォーマンス (テーブル)

```R
real_estate$perf_table <- calc_perf_metrices(real_estate$data, etfs, benchmark = "SPY")
real_estate$perf_table
```

      Symbol                      Index Sharpe   Alpha Beta Avg_Ret Cum_Ret StdDev MaxDD
    1    IYR Dow Jones U.S. Real Estate   0.36 0.00022 0.98    9.7%  495.9%  26.7% 74.1%
    2    SPY                    S&P 500   0.31 0.00000 1.00    5.8%  199.8%  18.8% 55.2%

## 相関係数

```R
real_estate$cor_plot <- plot_ggpairs(real_estate$data)
real_estate$cor_plot
```

![img](https://dl.dropboxusercontent.com/s/bmtgv6i1wodeno7/us-etf-research_real-estate-cor.png)

# コモディティ ETFs

## データの準備

```R
commodity <- list()
commodity$data <- data %>%
  filter(class == "Commodity" | symbol == "SPY") %>%
  align_start_date() %>%
  add_cumret_dd()
```

## パフォーマンス (プロット)

```R
commodity$perf_plot <- plot_perf_summary(commodity$data, offset = 0.1)
commodity$perf_plot
```

![img](https://dl.dropboxusercontent.com/s/qt9zbzycy4qrabo/us-etf-research_commodity-perf.png)

## パフォーマンス (テーブル)

```R
commodity$perf_table <- calc_perf_metrices(commodity$data, etfs, benchmark = "SPY")
commodity$perf_table
```

      Symbol     Index Sharpe    Alpha Beta Avg_Ret Cum_Ret StdDev MaxDD
    1    SPY   S&P 500   0.46  0.00000 1.00    8.7%  209.2%  19.0% 55.2%
    2    GLD      Gold   0.31  0.00028 0.02    5.7%  111.1%  18.4% 45.6%
    3    SLV    Silver   0.03  0.00010 0.33    1.0%   13.8%  31.2% 72.4%
    4    USO Crude Oil  -0.36 -0.00058 0.72  -12.3%  -83.0%  33.8% 93.2%

## 相関係数

```R
commodity$cor_plot <- plot_ggpairs(commodity$data)
commodity$cor_plot
```

![img](https://dl.dropboxusercontent.com/s/8jt83srq1eqm0ic/us-etf-research_commodity-cor.png)

# ボラティリティ ETFs

## データの準備

```R
volatility <- list()
volatility$data <- data %>%
  filter(class == "Volatility" | symbol == "SPY") %>%
  align_start_date() %>%
  add_cumret_dd()
```

## パフォーマンス (プロット)

```R
volatility$perf_plot <- plot_perf_summary(volatility$data, offset = 0.2)
volatility$perf_plot
```

![img](https://dl.dropboxusercontent.com/s/xixd7j4oxeq6vey/us-etf-research_volatility-perf.png)

## パフォーマンス (テーブル)

```R
volatility$perf_table <- calc_perf_metrices(volatility$data, etfs, benchmark = "SPY")
volatility$perf_table
```

      Symbol                         Index Sharpe    Alpha  Beta Avg_Ret Cum_Ret StdDev  MaxDD
    1    SPY                       S&P 500   0.96  0.00000  1.00   15.3%  363.9%  15.9%  21.8%
    2    VXX S&P 500 VIX Short-Term Future  -0.88 -0.00047 -3.17  -55.3% -100.0%  62.9% 100.0%

## 相関係数

```R
volatility$cor_plot <- plot_ggpairs(volatility$data)
volatility$cor_plot
```

![img](https://dl.dropboxusercontent.com/s/1unftohnm2wv3qw/us-etf-research_volatility-cor.png)

# FX ETFs

## データの準備

```R
forex <- list()
forex$data <- data %>%
  filter(class == "Forex" | symbol == "SPY") %>%
  align_start_date() %>%
  add_cumret_dd()
```

## パフォーマンス (プロット)

```R
forex$perf_plot <- plot_perf_summary(forex$data, offset = 0.1)
forex$perf_plot
```

![img](https://dl.dropboxusercontent.com/s/21w2w55jyeqidri/us-etf-research_forex-perf.png)

## パフォーマンス (テーブル)

```R
forex$perf_table <- calc_perf_metrices(forex$data, etfs, benchmark = "SPY")
forex$perf_table
```

      Symbol   Index Sharpe    Alpha  Beta Avg_Ret Cum_Ret StdDev MaxDD
    1    SPY S&P 500   0.44  0.00000  1.00    8.6%  185.2%  19.4% 55.2%
    2    FXF     CHF   0.10  0.00008 -0.01    1.2%   16.3%  11.4% 33.4%
    3    UUP     USD   0.09  0.00008 -0.09    0.8%   10.6%   8.5% 22.2%
    4    FXY     JPY   0.02  0.00012 -0.22    0.2%    2.3%  10.3% 40.6%
    5    FXE     EUR  -0.14 -0.00009  0.13   -1.3%  -15.5%   9.7% 35.4%
    6    FXB     GBP  -0.29 -0.00016  0.14   -2.9%  -31.2%   9.8% 42.1%

## 相関係数

```R
forex$cor_plot <- plot_ggpairs(forex$data)
forex$cor_plot
```

![img](https://dl.dropboxusercontent.com/s/6o7hqyopqrlw8lz/us-etf-research_forex-cor.png)
