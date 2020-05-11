[GitHub](https://github.com/five-dots/notes/blob/master/lang/r/general/date_time_pitfall/date_time_pitfall.org) | [Blog](https://objective-boyd-9b8f29.netlify.app/2020/05/date_time_pitfall/) | [Qiita](https://qiita.com/five-dots/items/b90c5f4cf31d60d04ed9)

R の `Date` / `POSIXct` 型を利用していて過去にハマったポイントを備忘録として整理しておく。


# `for` loop 内で `Date` が `numeric` になってしまう問題


## 現象

-   `Date` `vector` に対して `for` loop でアクセスすると意図した結果にならない
-   `for` loop 内で class attribute が欠落してしまうことが原因
    -   [For loops in R can lose class information@R-bloggers](https://www.r-bloggers.com/for-loops-in-r-can-lose-class-information/)
    -   `Date` は `numeric` に class attribute を追加したものであるため

```R
dates <- c(as.Date("2020-05-01"), as.Date("2020-05-02"))

for (date in dates) {
  print(date)
}
```

```R
[1] 18383
[1] 18384
```


## 対策 1: `list` に変換してからループする

```R
for (date in as.list(dates)) {
  print(date)
}
```

```R
[1] "2020-05-01"
[1] "2020-05-02"
```


## 対策 2: インデックスでアクセスする

```R
for (i in seq_along(dates)) {
  print(dates[i])
}
```

```R
[1] "2020-05-01"
[1] "2020-05-02"
```


# `POSIXct` から `Date` への変換で日付がずれる問題


## 現象

-   参考: [R: POSIXct -> Date で日付がズレる@Qiita](https://qiita.com/kota9/items/657c8c0ac5092e3ec1ff)

```R
td <- as.POSIXct("2020-05-01")
as.Date(td)
```

```R
[1] "2020-04-30"
```



-   これは `as.Date()` は元の `POSIXct` のタイムゾーンを意識せず、デフォルトで UTC へ変換してしまうことが原因
    -   `as.POSIXct()` で作成した場合、デフォルトでシステムのタイムゾーンを利用する (この場合は、JST)
    -   そのため、JST から 9 時間分の差が発生する
-   以下の例を見れば、違いが良くわかる

```R
as.Date(as.POSIXct("2020-05-01 8:00:00")) # 2020-04-30 23:00 へ変換されてから、時間情報が削除されている
as.Date(as.POSIXct("2020-05-01 9:00:00")) # 2020-05-01 00:00 へ変換されてから、時間情報が削除されている
```

```R
[1] "2020-04-30"
[1] "2020-05-01"
```


## 対策 1: `tz` を指定する

-   変換前と変換後のタイムゾーンを揃えることを意識しておけば良い
    -   タイムゾーンは、&ldquo;Area/Locality&rdquo; の形式で指定すべき
        -   [Understanding timezone strings in R@Stackoverflow](https://stackoverflow.com/questions/37205128/understanding-timezone-strings-in-r)

```R
# UTC に統一して変換
td <- as.POSIXct("2020-05-01", tz = "UTC")
as.Date(td)

# もしくは、JST に統一して変換
## td <- as.POSIXct("2020-05-01")
## as.Date(td, tz = "Asia/Tokyo")
```

```R
[1] "2020-05-01"
```


## 対策 2: `lubridate::as_date()` を利用する

-   `lubridate::as_Date()` は、元の `POSIXct` のタイムゾーンを保持して変換してくれる

```R
td <- as.POSIXct("2020-05-01")
lubridate::as_date(td)
```

```R
[1] "2020-05-01"
```


# ミリ秒の丸め問題


## 現象

-   文字列から `POSIXct` を作成する際に、ミリ秒がずれる (切り捨てられる)
    -   [R issue with rounding milliseconds@Stackoverflow](https://stackoverflow.com/questions/10931972/r-issue-with-rounding-milliseconds)
    -   `format` の `%OS` は `"second.millisecond"` の形式

```R
options(digits.secs = 3)
ms_dt <- as.POSIXct("2020-05-01 00:00:00.123", format = "%Y-%m-%d %H:%M:%OS")
ms_dt
```

```R
[1] "2020-05-01 00:00:00.122 JST"
```


## 対策 1: `lubridate::ymd_hms()` を使う

```R
options(digits.secs = 3)
lubridate::ymd_hms("2020-05-01 00:00:00.123", tz = "Asia/Tokyo")
```

```R
[1] "2020-05-01 00:00:00.123 JST"
```


## [番外] ミリ秒単位の経過時間を `POSIXct` に変換する

-   [R How to convert milliseconds from origin to date and keep the milliseconds@Stackoverflow](https://stackoverflow.com/questions/49828433/r-how-to-convert-milliseconds-from-origin-to-date-and-keep-the-milliseconds)
-   株価のティックデータなど、ミリ秒単位の経過時間で表現されるデータがある
-   1000 で割って秒数に換算する
-   +0.0005 を足すことで丸め誤差を消すことができる

```R
msec <- 1588291200123 # 2020-05-01 00:00:00.123 JST
dt <- as.POSIXct(msec/1000, origin = "1970-01-01", tz = "JST")
format(dt + 0.0005, "%Y-%m-%d %H:%M:%OS")
```

```R
[1] "2020-05-01 00:00:00.123"
```



-   `lubridate::as_datetime()` でも同じようにずれるので、+0.0005 する

```R
lubridate::as_datetime(msec/1000 + 0.0005, tz = "JST")
```

```R
[1] "2020-05-01 00:00:00.123 JST"
```


# セッション情報

```R
sessionInfo()
```

```R
R version 3.6.3 (2020-02-29)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.4 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=C
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=C
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

loaded via a namespace (and not attached):
[1] compiler_3.6.3  generics_0.0.2  tools_3.6.3     Rcpp_1.0.4.6
[5] lubridate_1.7.8
```
