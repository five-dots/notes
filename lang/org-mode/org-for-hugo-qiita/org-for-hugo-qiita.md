普段、技術的なメモは Emacs の [org-mode](https://orgmode.org/ja/) で書いたものを [自身の Github リポジトリ](https://github.com/five-dots/notes) に公開している。ただ、これはあくまで個人的なメモなので、それほど他人が見ることを意識したものではない。より見やすい記事として公開するために、[個人のブログ](https://objective-boyd-9b8f29.netlify.com/)を、[Hugo](https://gohugo.io/) + [Netlify](https://www.netlify.com/) で開始したが、力を入れて書いた技術記事は Qiita にも公開して、広く読んでもらえるようにしたい。

そこで、これまで通り、org ファイルは同じリポジトリで管理しつつ、1 つの org ファイルから個人ブログ向けと Qiita 向けの Markdown をエクスポートできないか試してみた。

# Qiita 向けの Markdown Exporter の選択

Hugo 向けの Exporter は[ `ox-hugo` ](https://github.com/kaushalmodi/ox-hugo)一択でよいだろう。Qiita 向けの Markdown の生成には、いつくかの候補を検討した後に[ `ox-gfm` ](https://github.com/larstvei/ox-gfm)を選択した。他の選択肢としては、標準の `ox-md` や、[こちらの記事](https://qiita.com/0x60df/items/3cde67967e3db30d9afe)で紹介されている[ `ox-qmd` ](https://github.com/0x60df/ox-qmd)などがある。特に `ox-qmd` はそのものズバリで、最も org-mode から Qiita への再現性が高いと思われた。しかし、自分の設定が悪いのか、org の table が HTML として出力されてしまい、その結果、文字修飾が有効にならないという問題があったため断念した。他にも[ `ox-pandoc` ](https://github.com/kawabata/ox-pandoc)からも Markdown を出力できるが、今回は試していない。

# 生成されるファイル

個人ブログ向け・ Qiita 向け、それぞれの出力結果がこちらである。尚 `.org` や `.md` を Github 上で閲覧すると [github/markup](https://github.com/github/markup) によって自動的にレンダリングされるので、ソースは「Raw」から見てほしい。

1.  [元となる org ファイル@github](https://github.com/five-dots/notes/blob/master/lang/org-mode/org-for-hugo-qiita/org-for-hugo-qiita.org)
2.  [個人ブログの記事](https://objective-boyd-9b8f29.netlify.com/2019/11/org-for-hugo-qiita/) ([Markdown ファイル@github](https://github.com/five-dots/blog/blob/master/content/post/2019/11/org-for-hugo-qiita.md))
3.  [Qiita の記事](https://qiita.com/five-dots/items/a0183c9f4b46f786a666) ([Markdown ファイル@github](https://github.com/five-dots/notes/blob/master/lang/org-mode/org-for-hugo-qiita/org-for-hugo-qiita.md))

以降の章で、org-mode の記法毎に出力結果を確認していく。両者に共通して利用できるものに絞って記事を作成することで、二重管理を避けていくことが主目的だ。

なお、Hugo テーマは [Even](https://github.com/olOwOlo/hugo-theme-even) を利用している。その他のテーマでは、この記事と異なる結果になる可能性もあるので、その点は注意が必要だ。

# 動作確認バージョン

| ソフトウェア | バージョン      |
|-------- |--------------- |
| OS       | Ubuntu 18.04    |
| Emacs    | 26.3            |
| org-mode | 9.2.6           |
| Hugo     | 0.59.1/extended |

# 見出し

# Hugo=h2 / Qiita=h1

## Hugo=h3 / Qiita=h2

### Hugo=h4 / Qiita=h3

#### Hugo=h5 / Qiita=h4

##### Hugo=h6 / Qiita=h5

## メモ

-   Hugo
    -   Hugo では `*` がタイトルとして扱われるため、 `**` (h2) から見出しとして機能する。そのため Qiita とは 1 階層ずれるが、大きな問題にはならない
    -   org-mode のデフォルトでは、h4 までしか出力されないため、それ以上の深さが必要な場合は `#+OPTIONS: H:6` のように追加で設定が必要
    -   [Even](https://github.com/olOwOlo/hugo-theme-even) theme では h6 が最小

# 表・文字修飾

| 項目   | org-mode          | 結果                                     |
|------ |----------------- |---------------------------------------- |
| 太字   | `*bold*`          | **bold**                                 |
| イタリック | `/italic/`        | *italic*                                 |
| 下線   | `_underline_`     | <span class="underline">underline</span> |
| 取り消し線 | `+strikethrough+` | ~~strikethrough~~                        |
| コード | `~code~`          | `code`                                   |
| 逐語   | `=verbatim=`      | `verbatim`                               |
| 上付き | `hoge^{super}`    | hoge<sup>super</sup>                     |
| 下付き | `hoge_{sub}`      | hoge<sub>sub</sub>                       |
| ギリシャ文字 | `\alpha`          | &alpha;                                  |

## メモ

-   Hugo
    -   下線は `span.underline {text-decoration: underline;}` を `static/css/custom.css` に追加した場合の表示結果
    -   コードは `(setq org-hugo-use-code-for-kbd t)` に設定した場合の表示結果
-   Qiita
    -   下線・コードは有効でない

# リスト

## 順序なしリスト

-   hoge
    -   hoge
    -   fuga
    -   piyo
-   fuga
-   piyo

## チェックボックス

-   [ ] hoge
-   fuga
-   [ ] piyo

## 順序ありリスト

1.  hoge
    1.  hoge
    2.  fuga
    3.  piyo
2.  fuga
3.  piyo

## 定義リスト

-   **リンゴ:** 赤いフルーツ
-   **オレンジ:** 橙色フルーツ

## メモ

-   Hugo
    -   チェックボックスは、チェックをつけてしまうとなぜか表示されなくなってしまう
-   Qiita
    -   チェックボックス自体が有効にならない

# 引用

> Everything should be made as simple as possible, but not any simpler &#x2014;Albert Einstein

## メモ

-   Hugo/Qiita ともに問題なく表示されている

# 数式

## インライン

`$y=f(x)$`

$y=f(x)$

## ブロック

`$$y=f(x)$$`

$$y=f(x)$$

## メモ

-   Qiita
    -   ox-gfm のエクスポート時に Latex Fragment の `$ ... $` が `\( ... \)` に `$$ ... $$` が `\[ ... \]` に変換されてしまう
    -   [Qiita の数式記法](https://qiita.com/PlanetMeron/items/63ac58898541cbe81ada) では、この記法に対応していないので、[公式](https://orgmode.org/manual/Advanced-Export-Configuration.html)や[ここ](https://emacs.stackexchange.com/questions/47733/org-latex-exports-math-as-can-this-be-avoided)や[ここ](http://fjyuu.info/blog/remove-japanese-spaces/)を参考にフィルタを作成して再変換する

```emacs-lisp
(defun my/org-replace-latex-wrap (text backend _info)
  (when (org-export-derived-backend-p backend 'gfm)
    (cond
     ((s-starts-with? "\\(" text)
      (message (format "start with (, content = %s" text))
      (--> text
           (s-chop-prefix "\\(" it)
           (s-chop-suffix "\\)" it)
           (s-wrap it "$")))
     ((s-starts-with? "\\[" text)
      (message (format "start with [, content = %s" text))
      (--> text
           (s-chop-prefix "\\[" it)
           (s-chop-suffix "\\]" it)
           (s-wrap it "$$"))))))
(add-to-list 'org-export-filter-latex-fragment-functions 'my/org-replace-latex-wrap)
```

# 脚注

-   org-mode<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup> `[fn:name]`

## メモ

-   Qiita
    -   Qiita では有効でない

# 水平線

`-----`

---

## メモ

-   Hugo
    -   5 つの `-` で Markdown 側では `---` に変換される

# コードブロック

## Emacs Lisp

```emacs-lisp
(emacs-version)
```

    GNU Emacs 26.3 (build 2, x86_64-pc-linux-gnu, GTK+ Version 3.22.30)
     of 2019-09-17

## R

### コード出力

```R
R.version
```

                   _                           
    platform       x86_64-pc-linux-gnu         
    arch           x86_64                      
    os             linux-gnu                   
    system         x86_64, linux-gnu           
    status                                     
    major          3                           
    minor          6.1                         
    year           2019                        
    month          07                          
    day            05                          
    svn rev        76782                       
    language       R                           
    version.string R version 3.6.1 (2019-07-05)
    nickname       Action of the Toes

### 表

```R
library(tidyverse)
head(iris)
```

| Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species |
|------------ |----------- |------------ |----------- |------- |
| 5.1          | 3.5         | 1.4          | 0.2         | setosa  |
| 4.9          | 3           | 1.4          | 0.2         | setosa  |
| 4.7          | 3.2         | 1.3          | 0.2         | setosa  |
| 4.6          | 3.1         | 1.5          | 0.2         | setosa  |
| 5            | 3.6         | 1.4          | 0.2         | setosa  |
| 5.4          | 3.9         | 1.7          | 0.4         | setosa  |

### プロット

```R
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()
```

![img](https://dl.dropboxusercontent.com/s/4j5jstkg1fsvdiw/iris.png)

### メモ

-   Qiita
    -   Hugo 向けには `ox-hugo` が自動で画像ファイルを `static/ox-hugo/` へ移動してくれるが、Qiita へは手動で画像をアップロードする必要がある
    -   そのため org-babel からプロット画像を出力する先を Dropbox フォルダに設定し、共有リンク機能を利用して画像を公開する
    -   org-babel は `:exports code` に設定することで、ファイルリンク出力を抑制しつつ、手元ではプロットをインライン画像で確認できる
    -   [ここ](http://ijmp320.hatenablog.jp/entry/2015/01/18/171807)の記事を参考に、Dropbox の直リンクに変換し、以下のように HTML として出力する
    -   将来的には Dropbox API + Emacs Lisp で自動化したい

```org
#+attr_html:
[[https://dl.dropboxusercontent.com/s/4j5jstkg1fsvdiw/iris.png]]
```

## Python

```python
import sys
sys.version
```

    3.6.8 (default, Oct  7 2019, 12:59:55) 
    [GCC 8.3.0]

# まとめ

一部、Qiita 側で有効にならない記法が見られたが、無いと致命的、という項目はなかった。また、不足している項目も `(org-export-filter-TYPE-funstions)` を利用すれば、それほど苦労もなく今後カスタマイズできそうなこともわかった。これから快適なブログライフをおくっていきたい。

# 参考

-   Qiita 関連
    -   [Markdown 記法 チートシート](https://qiita.com/Qiita/items/c686397e4a0f4f11683d)
    -   [Qiita の数式チートシート](https://qiita.com/PlanetMeron/items/63ac58898541cbe81ada)
    -   [Org-mode から Qiita 準拠の Markdown を export するパッケージを作ってみました](https://qiita.com/0x60df/items/3cde67967e3db30d9afe)
    -   [emacs の org-mode で書いた記事を qiita に投稿する org-qiita.el](https://qiita.com/dwarfJP/items/594a8d4b0ac6d248d1e4)
-   ox-hugo
    -   [Org-mode で記事を書いて Hugo 向け markdown を ox-hugo で自動生成する話](https://sfus.net/blog/2018/12/org-mode-with-ox-hugo/)
    -   [ox-Hugo Cheat Sheet](https://ladicle.com/post/ox-hugo-cheat/)
-   org-mode filters
    -   [12.17 Advanced Export Configuration](https://orgmode.org/manual/Advanced-Export-Configuration.html)
    -   [Org latex exports $ … $ math as $ … \) $: can this be avoided?](https://emacs.stackexchange.com/questions/47733/org-latex-exports-math-as-can-this-be-avoided)
    -   [org-mode が出力する HTML から余分な改行を削除する](http://fjyuu.info/blog/remove-japanese-spaces/)
-   Dropbox
    -   [【備忘録】Dropbox の画像の URL（直リンク）の取得](http://ijmp320.hatenablog.jp/entry/2015/01/18/171807)
