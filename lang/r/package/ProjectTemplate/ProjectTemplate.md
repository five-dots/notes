データ分析には、パッケージのロード・データのインポートと加工・プロジェクト毎の関数の読み込み等、毎回共通して発生する作業がある。その都度、データはどこに保存しようか、この関数はどこに書こうか、などと迷ってしまう。そろそろ本腰を入れて、自分用のテンプレートを作ろうかと思っていた矢先に[ `{ProjectTemplate}` ](https://github.com/KentonWhite/ProjectTemplate)というパッケージと出会った。個人的に、これがかなりしっくりきたので、紹介してみようと思う。

※ [RStudio の Project Template 機能](https://rstudio.github.io/rstudio-extensions/rstudio_project_templates.html)とは別物である。

# R のワークフローツール

本題に入る前に、R でワークフロー管理を行う場合の選択肢を見てみよう。自分に合いそうなツールを選択されると良いと思う。

[List of workflow tools for R projects](https://github.com/jdblischak/r-project-workflows) には、約 50 ほどのワークフロー関連ツールがリストされている。 `{ProjectTemplate}` もこのリストの中で見つけたものだ。この中には、R のパッケージもあれば、R のパッケージ構成をテンプレートとして利用するタイプのものなど様々である。

`{ProjectTemplate}` 以外であれば[ `{drake}` ](https://github.com/ropensci/drake)が有力な選択肢になるだろう。ワークフロー全体を `data.frame` で管理し `data.frame` の 1 行を 1 つのタスクとする方式だ。一度実行した分析結果は、変更がなければキャッシュから再利用してくれるところが便利である。開発も活発であるし、興味のある方は、[この記事](https://blog.hoxo-m.com/entry/2018/09/05/184425)辺りから始めてみるとよいだろう。

また、R のパッケージ構成をデータ分析プロジェクトに流用するというのも有力なアプローチのようだ。パッケージ開発に慣れている方や、分析結果を Vignette として公開するというコンセプトに魅力を感じるのであれば、試してみる価値があるだろう。 [Analyses as Packages](https://rmflight.github.io/posts/2014/07/analyses_as_packages.html) にそのメリットがまとめられている。

私自身は、Github のスターが多かった `{drake}` から試してみた。ワークフローの依存関係をグラフ化してくれたり、非常に精力的なプロジェクトに感じたが、最終的には[ワークフロー内のタスク (target と呼ばれる) から、次のタスクを transform で生成できない](https://stackoverflow.com/questions/56161508/how-to-generate-arguments-to-a-target-transformation-dynamically-in-r-drake)点が致命的に感じてしまい、現在は利用していない。色々と工夫はされているが、痒いところに手が届かなかった印象だ。

`{ProjectTemplate}` はフォルダ構成のテンプレートをベースに、いくつかのヘルパー関数を収録している。直感的にわかりやすいため学習コストが低く、色々やりすぎていないのが好印象だ。

※ RStudio が[ `{tidymodels}` ](https://github.com/tidymodels/tidymodels)パッケージ群の 1 つとして `{workflows}` というものを開発している模様。まだリリース前で[ `{tune}` ](https://github.com/tidymodels/tune)の記述の一部に登場してくるのみだが、リリースされれば本命になるかもしれない。

# {ProjectTemplate} の特徴

前置きが長くなってしまったが `{ProjectTemplate}` について具体的に見ていこう。 **Convention over configuration** (CoC: [設定より規約](https://ja.wikipedia.org/wiki/設定より規約)) のコンセプトをベースにしていて、複雑な設定不要でシンプルに利用できる。

個人的に気に入っているところを挙げると:

-   わかりやすいフォルダ構成で、どこに何を書くべきか・置くべきかで迷うことがなくなる
-   プロジェクトで利用するデータ・外部パッケージ・関数や変数を自動で読み込んでくれる
-   読み込んだデータを自動でキャッシュしてくれる
-   キャッシュに依存関係を設定すれば、依存先に変更があった場合のみ、キャッシュを更新させることができる
-   プロジェクトの構成をカスタマイズすることができる

# プロジェクトの作成

それでは、CRAN もしくは Github からパッケージをインストールし、プロジェクトを作成してみよう。プロジェクトの新規作成は、適当なディレクトリに移動し `create.project("プロジェクト名")` とするだけだ。

```R
library("ProjectTemplate")
setwd("/your/favarite/dir")
create.project("test-project")
```

すると、以下のようなフォルダが作成される。

```shell
tree --dirsfirst test-project
```

    test-project
    ├── cache
    │   └── README.md
    ├── config
    │   ├── global.dcf
    │   └── README.md
    ├── data
    │   └── README.md
    ├── diagnostics
    │   ├── 1.R
    │   └── README.md
    ├── docs
    │   └── README.md
    ├── graphs
    │   └── README.md
    ├── lib
    │   ├── globals.R
    │   ├── helpers.R
    │   └── README.md
    ├── logs
    │   └── README.md
    ├── munge
    │   ├── 01-A.R
    │   └── README.md
    ├── profiling
    │   ├── 1.R
    │   └── README.md
    ├── reports
    │   └── README.md
    ├── src
    │   ├── eda.R
    │   └── README.md
    ├── tests
    │   ├── 1.R
    │   └── README.md
    ├── README.md
    └── TODO
    
    13 directories, 28 files

全てのフォルダの役割は、[Architecture](http://projecttemplate.net/architecture.html) に記載されている。

# プロジェクトの読み込み

それでは、作成したフォルダに移動し、プロジェクトを読み込んでみよう。

```R
setwd("test-project")
load.project()
```

    Project name: test-project
    Loading project configuration
    Autoloading helper functions
     Running helper script: globals.R
     Running helper script: helpers.R
    Autoloading data
    Munging data
     Running preprocessing script: 01-A.R

いくつかのファイルが自動で実行されているのがわかる。 `load.project()` 時にどのファイルが・どの順番で読み込まれるかは、必ず押さえておく必要があるだろう。

デフォルトでは:

1.  `config/global.dcf`
    -   プロジェクト全体の環境設定を行うファイル
2.  `lib/globals.R`
    -   プロジェクト全体で利用するグローバル変数
3.  `lib/helpers.R`
    -   プロジェクト全体で利用する関数
4.  `cache/*`
    -   キャッシュが有効な場合、ここに保存されている `.RData` ファイル
5.  `data/*`
    -   `.csv` や `.rds` などプロジェクトで利用するデータファイル ([Supported File Formats](http://projecttemplate.net/file_formats.html))
6.  `munge/*`
    -   読み込んだデータを分析可能な状態に変換・整形するためのスクリプト
    -   ファイルのソート順に実行されるので `01_hoge.R`, `02_fuga.R` のように命名すると良い

なお、データ分析用のスクリプトを置く `src/` のファイルは自動実行されない。 `run.project()` を実行するか、個別に `source()` で実行することになる。典型的には `src/eda.R` などで、読み込んだデータに対して、探索的に分析を繰り返し、ある程度コードがまとまったタイミングで `lib/` や `munge/` にコードを追加後 `load.project()` を繰り返すという分析フローになるだろう。

# プロジェクトの設定

## `config/global.dcf`

このファイルにプロジェクト全体の環境設定を記載する。デフォルトの設定は以下の通りだ。全項目の解説を記載するので、参考にしてほしい。(原文 [Configuration](http://projecttemplate.net/configuring.html))

```txt
version: 0.9.0
data_loading: TRUE
data_loading_header: TRUE
data_ignore:
cache_loading: TRUE
recursive_loading: FALSE
munging: TRUE
logging: FALSE
logging_level: INFO
load_libraries: FALSE
libraries: reshape2, plyr, tidyverse, stringr, lubridate
as_factors: TRUE
tables_type: tibble
attach_internal_libraries: FALSE
cache_loaded_data:  TRUE
sticky_variables: NONE
```

| 設定項目                    | 説明                                                                                                   |
|--------------------------- |------------------------------------------------------------------------------------------------------ |
| `data_loading`              | `data/`, `cache/` からデータを読み込むかどうか。両方に同じ名前が存在する場合で、かつ `cache_loading` が `TRUE` の場合は `cache/` のみから読み込まれる。 |
| `data_loading_header`       | csv ファイルなどの 1 行目をヘッダーとして扱うかどうか。                                                |
| `data_ignore`               | `data/` フォルダから自動で読み込ませたくないファイルを `"hoge.csv", "fuga.rds"` のように `data/` からの相対パスで指定する。 |
| `cache_loading`             | `cache/` フォルダからデータを読み込むかどうか。                                                        |
| `recursive_loading`         | `data/` フォルダ内のサブフォルダに存在するファイルも読み込むかどうか。                                 |
| `munging`                   | `munge/` ファルダの R スクリプトを自動的に実行するかどうか。                                           |
| `logging`                   | ログを有効にするかどうか。                                                                             |
| `logging_level`             | ログレベルを設定。                                                                                     |
| `load_libraries`            | `libraries` で指定したライブラリを自動で読み込むかどうか。                                             |
| `libraries`                 | 自動的に読み込むライブラリを指定。                                                                     |
| `as_factors`                | `data.frame` 内の character vector を `factor` に変換するかどうか。(`stringsAsFactors` の設定)         |
| `table_type`                | `tibble` (デフォルト), `data_frame`, `data_table` の中から選択する。                                   |
| `attach_internal_libraries` | `TRUE` の場合 `load.project()` 時に warning が表示される。                                             |
| `cache_loaded_data`         | `data/` から読み込まれたデータをキャッシュするかどうか。                                               |
| `sticky_variables`          | `clear()` コマンド時にも環境に残しておきたい、サイズの大きなオブジェクトを指定する。                   |

## `lib/globals.R`

ここにプロジェクト単位のグローバル変数を定義する。個人的は、プロジェクトに git リポジトリを作成した上で[ `{here}` ](https://github.com/r-lib/here)を使って、プロジェクトのディレクトリを設定している。

```R
add.config(
  apply.override = FALSE,
  prj_dir = here::here()
)

add.config(
  apply.override = TRUE
)
```

# データの読み込み

プロジェクトで利用する外部データは `data/` にファイルを置くだけで、自動で読み込まれる。試しに、 `iris` をファイルとして書き出してみる。

```R
iris2 <- iris
write.csv(iris2, "data/iris2.csv")
```

一旦 `clear()` し、再度プロジェクトを読み込む。

```R
clear()
load.project()
```

    Objects to clear from memory: config helper.function iris2 project.info
    Project name: test-project
    Loading project configuration
    Autoloading helper functions
     Running helper script: globals.R
     Running helper script: helpers.R
    Autoloading data
     Loading data set: iris2
     Translating data.frame to tibble: iris2
      Creating cache entry from global environment: iris2
    Munging data
     Running preprocessing script: 01-A.R

自動的に読み込まれたことが確認できる。

```R
head(iris2)
```

| X | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species |
|--- |------------ |----------- |------------ |----------- |------- |
| 1 | 5.1          | 3.5         | 1.4          | 0.2         | setosa  |
| 2 | 4.9          | 3           | 1.4          | 0.2         | setosa  |
| 3 | 4.7          | 3.2         | 1.3          | 0.2         | setosa  |
| 4 | 4.6          | 3.1         | 1.5          | 0.2         | setosa  |
| 5 | 5            | 3.6         | 1.4          | 0.2         | setosa  |
| 6 | 5.4          | 3.9         | 1.7          | 0.4         | setosa  |

`cache_loaded_data` が `TRUE` なので、キャッシュも自動的に作成された。

```shell
tree test-project/cache
```

    test-project/cache
    ├── iris2.hash
    ├── iris2.RData
    └── README.md
    
    0 directories, 3 files

# データの加工とキャッシュ

`data/` や `cache/` から読み込んだデータを加工・整形するためのスクリプトは `munge/` に置く。プロジェクトの読み込み時に自動で実行されるので、すぐに分析に取り掛かることができて便利だ。

例えば `munge/01-A.R` に以下のように記載し、プロジェクトを再度ロードしてみる。

```R
iris_longer <- iris2 %>%
  pivot_longer(-Species, names_to = "key", values_to = "value")
```

```R
load.project()
```

    Project name: test-project
    Loading project configuration
    Autoloading helper functions
     Running helper script: globals.R
     Running helper script: helpers.R
    Autoloading data
    Munging data
     Running preprocessing script: 01-A.R

すると、Global 環境で変換後のデータが利用可能になる。

```R
head(iris_longer)
```

| Species | key          | value |
|------- |------------ |----- |
| setosa  | X            | 1     |
| setosa  | Sepal.Length | 5.1   |
| setosa  | Sepal.Width  | 3.5   |
| setosa  | Petal.Length | 1.4   |
| setosa  | Petal.Width  | 0.2   |
| setosa  | X            | 2     |

先程 `data/` フォルダから読み込まれたファイルのキャッシュが自動的に作成される様子を示したが `munge/` で作成したオブジェクトもキャッシュさせたい場合は `cache()` 関数を利用する。 `munge/01-A.R` を以下のように変更するとキャッシュすることが可能だ。

```R
cache("iris_longer", depends = c("iris2"), CODE = {
  iris2 %>%
    pivot_longer(-Species, names_to = "key", values_to = "value")
})
```

```R
load.project()
```

    Project name: test-project
    Loading project configuration
    Autoloading helper functions
     Running helper script: globals.R
     Running helper script: helpers.R
    Autoloading data
    Munging data
     Running preprocessing script: 01-A.R
    Loading required namespace: formatR
      Creating cache entry from CODE: iris_longer

`cache/` フォルダ内にキャッシュが作成されたことが確認できる。

```shell
tree test-project/cache
```

    test-project/cache
    ├── iris2.hash
    ├── iris2.RData
    ├── iris_longer.hash
    ├── iris_longer.RData
    └── README.md
    
    0 directories, 5 files

一度キャッシュを作成した後は、依存先である `depends` に指定したオブジェクトか `CODE` が変更された場合のみキャッシュが再生成される。

# ユニットテスト

[ `{testthat}` ](https://github.com/r-lib/testthat)を利用してユニットテストを実行する場合は `tests/` に記載する。テスト対象の関数とテストを記載したら、後は `test.project()` を実行するだけだ。

-   `lib/helpers.R`

```R
add <- function(a, b) {
  a + b
}
```

-   `tests/1.R`

```R
library(testthat)
context("Example tests")
expect_equal(add(1, 1), 2)
```

```R
test.project()
```

    Project name: test-project
    Loading project configuration
    Autoloading helper functions
     Running helper script: globals.R
     Running helper script: helpers.R
    Autoloading data
    Munging data
     Running preprocessing script: 01-A.R
    ✔ |  OK F W S | Context
    ✔ |   1       | Example tests
    
    ══ Results ═════════════════════════════════════════════════════════════════════
    OK:       1
    Failed:   0
    Warnings: 0
    Skipped:  0

# その他の機能

ここまでで紹介した機能を使いこなせれば、データ分析もこれまでよりも快適に行えるのではないかと思う。 `{ProjectTemplate}` には他にも機能があるが、以下の機能は私自身利用していないこともあって、今回は割愛する。興味のある方は、各自マニュアルを参照して欲しい。

-   `log/`
-   `diagnostics/`
-   `profiling/`

# テンプレートのカスタマイズ

ここまではパッケージに付属のテンプレートに沿って解説を行ってきたが、これらのファイル構成は、カスタマイズが可能だ。私自身、不要なフォルダを削除したり、デフォルト設定を変更したものを作成して使っている。

-   [five-dots/ProjectTemplateTemplates](https://github.com/five-dots/ProjectTemplateTemplates)

`.Rprofile` に以下の設定を追加しておけば `create.project("project_name", "template_name")` でカスタマイズした構成を利用できる。

```R
options(ProjectTemplate.templatedir = "/path/to/templatedir")
```

# まとめ

以上 `{ProjectTemplate}` の主要な機能を紹介した。これにより、データ分析で必要になる定型的な作業を削減できるのではないだろうか。

それでは、Happy coding !!

# 参考リンク

-   [List of workflow tools for R projects](https://github.com/jdblischak/r-project-workflows)
-   ProjectTemplate 公式
    -   [公式サイト](http://projecttemplate.net/)
    -   [CRAN](https://cran.r-project.org/web/packages/ProjectTemplate/)
    -   [Reference Manual (PDF)](https://cran.r-project.org/web/packages/ProjectTemplate/ProjectTemplate.pdf)
    -   [Github Repo](https://github.com/KentonWhite/ProjectTemplate)
-   Blog
    -   [ProjectTemplate@John Myles White](http://www.johnmyleswhite.com/notebook/2010/08/26/projecttemplate/)
    -   [Packages vs ProjectTemplate@R-bloggers](https://www.r-bloggers.com/packages-vs-projecttemplate/)
    -   [First Impressions: ProjectTemplate For R-Projects@Statistics&R](https://ericeikrem.com/r-blog/first-impressions-projecttemplate-for-r-projects/)
    -   [Project Template@Applied R Code](http://applied-r.com/project-directory-template/)
    -   [Love for ProjectTemplate](https://hilaryparker.com/2012/08/25/love-for-projecttemplate/)
    -   [Customising ProjectTemplate in R@Jeromy Anglim's Blog](http://jeromyanglim.blogspot.com/2014/05/customising-projecttemplate-in-r.html)
        -   [jeromyanglim/AnglimModifiedProjectTemplate@Github](https://github.com/jeromyanglim/AnglimModifiedProjectTemplate)
    -   [Analyses as Packages@Deciphering life: One bit at a time](https://rmflight.github.io/posts/2014/07/analyses_as_packages.html)
    -   [Creating an analysis as a package and vignette@Deciphering life: One bit at a time](https://rmflight.github.io/posts/2014/07/vignetteAnalysis.html)
