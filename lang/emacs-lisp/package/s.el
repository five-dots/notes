;;;; s.el


;;; 空白・改行関連

;; 空白を取り除く
(let ((test-str " trim "))
  (s-trim test-str)                      ; => "trim"
  (s-trim-left test-str)                 ; => "trim "
  (s-trim-right test-str))               ; => " trim"

;; 末尾の改行コード \n, \r, \r\n を一つ取り除く
(s-chomp "no newlines\n")     ; => "no newlines"
(s-chomp "no newlines\r\n")   ; => "no newlines"
(s-chomp "some newlines\n\n") ; => "some newlines\n"

;; 間にあるものをスペース 1 つにする
(s-collapse-whitespace "only   one space   please") ; => "only one space please"
(s-collapse-whitespace "collapse \n all \t sorts of \r whitespace") ; => "collapse all sorts of whitespace"

;; 指定文字数で折り返す
(s-word-wrap 10 "This is too long")         ; => "This is\ntoo long"
(s-word-wrap 10 "This is way way too long") ; => "This is\nway way\ntoo long"
(s-word-wrap 10 "It-wraps-words-but-does-not-break-them") ; => "It-wraps-words-but-does-not-break-them"

;; 指定文字数で中央寄せ
(s-center 5 "a")                        ; => "  a  "
(s-center 5 "ab")                       ; => "  ab "
(s-center 1 "abc")                      ; => "abc"

;; 指定文字で padding
(s-pad-left 3 "0" "3")                  ; => "003"
(s-pad-left 3 "0" "23")                 ; => "023"
(s-pad-left 3 "0" "1234")               ; => "1234"
(s-pad-right 3 "." "3")                 ; => "3.."
(s-pad-right 3 "." "23")                ; => "23."
(s-pad-right 3 "." "1234")              ; => "1234"


;;; 文字列を短くする (抽出・削除)

;; ...で省略する
(s-truncate 6 "This is too long")       ; => "Thi..."
(s-truncate 16 "This is also too long") ; => "This is also ..."
(s-truncate 16 "But this is not!")      ; => "But this is not!"

;; 左右から指定文字数を抜き出す
(s-left 3 "lib/file.js")  ;; => "lib"
(s-left 3 "li")           ;; => "li"
(s-right 3 "lib/file.js") ;; => ".js"
(s-right 3 "li")          ;; => "li"

;; prefix/suffix を取り除く
(s-chop-suffix "-test.js" "penguin-test.js") ;; => "penguin"
(s-chop-suffix "\n" "no newlines\n")         ;; => "no newlines"
(s-chop-suffix "\n" "some newlines\n\n")     ;; => "some newlines\n"

(s-chop-suffixes '("_test.js" "-test.js" "Test.js") "penguin-test.js") ;; => "penguin"
(s-chop-suffixes '("\r" "\n") "penguin\r\n") ;; => "penguin"
(s-chop-suffixes '("\n" "\r") "penguin\r\n") ;; => "penguin"

(s-chop-prefix "/tmp" "/tmp/file.js")     ;; => "/file.js"
(s-chop-prefix "/tmp" "/tmp/tmp/file.js") ;; => "/tmp/file.js"

(s-chop-prefixes '("/tmp" "/my") "/tmp/my/file.js") ;; => "/file.js"
(s-chop-prefixes '("/my" "/tmp") "/tmp/my/file.js") ;; => "/my/file.js"

;; 冒頭/末尾の共通部分を抜き出す
(s-shared-start "bar" "baz")    ;; => "ba"
(s-shared-start "foobar" "foo") ;; => "foo"
(s-shared-start "bar" "foo")    ;; => ""

(s-shared-end "bar" "var") ;; => "ar"
(s-shared-end "foo" "foo") ;; => "foo"
(s-shared-end "bar" "foo") ;; => ""


;;; 文字列を長くする

;; 指定回数繰り返し
(s-repeat 10 " ") ;; => "          "
(s-concat (s-repeat 8 "Na") " Batman!") ;; => "NaNaNaNaNaNaNaNa Batman!"

;; 文字列の連結
(s-concat "abc" "def" "ghi") ;; => "abcdefghi"

;; prefix/suffix を追加
(s-prepend "abc" "def") ;; => "abcdef"
(s-append "abc" "def")  ;; => "defabc"


;;; 文字列 <=> list 変換

;; 行 => list に変換
(s-lines "abc\ndef\nghi")     ;; => ("abc" "def" "ghi")
(s-lines "abc\rdef\rghi")     ;; => ("abc" "def" "ghi")
(s-lines "abc\r\ndef\r\nghi") ;; => ("abc" "def" "ghi")

;; 正規表現にマッチしたものをリストで返す
(s-match "^def" "abcdefg") ;; => nil
(s-match "^abc" "abcdefg") ;; => ("abc")
(s-match "^/.*/\\([a-z]+\\)\\.\\([a-z]+\\)" "/some/weird/file.html") ; => ("/some/weird/file.html" "file" "html")

(s-match-strings-all "{\\([^}]+\\)}" "x is {x} and y is {y}") ;; => (("{x}" "x") ("{y}" "y"))
(s-match-strings-all "ab." "abXabY")      ;; => (("abX") ("abY"))
(s-match-strings-all "\\<" "foo bar baz") ;; => (("") ("") (""))

;; 正規表現にマッチした位置を返す
(s-matched-positions-all "l+" "{{Hello}} World, {{Emacs}}!" 0)            ;; => ((4 . 6) (13 . 14))
(s-matched-positions-all "{{\\(.+?\\)}}" "{{Hello}} World, {{Emacs}}!" 0) ;; => ((0 . 9) (17 . 26))
(s-matched-positions-all "{{\\(.+?\\)}}" "{{Hello}} World, {{Emacs}}!" 1) ;; => ((2 . 7) (19 . 24))

;; 正規表現で、文字列を分割 (分割文字含む)
(s-slice-at "-" "abc")               ;; => ("abc")
(s-slice-at "-" "abc-def")           ;; => ("abc" "-def")
(s-slice-at "[.#]" "abc.def.ghi#id") ;; => ("abc" ".def" ".ghi" "#id")

;; 正規表現で、文字列を分割 (分割文字含まない), OMIT-NULLS で NULL を削除
(s-split "|" "a|bc|12|3")   ;; => ("a" "bc" "12" "3")
(s-split ":" "a,c,d")       ;; => ("a,c,d")
(s-split "\n" "z\nefg\n")   ;; => ("z" "efg" "")
(s-split "\n" "z\nefg\n" t) ;; => ("z" "efg")

;; s-split のリスト長指定
(s-split-up-to "\\s-*-\\s-*" "Author - Track-number-one" 1) ;; => ("Author" "Track-number-one")
(s-split-up-to "\\s-*-\\s-*" "Author - Track-number-one" 2) ;; => ("Author" "Track" "number-one")
(s-split-up-to "|" "foo||bar|baz|qux" 3 t)                  ;; => ("foo" "bar" "baz|qux")

;; 区切り文字で連結
(s-join "+" '("abc" "def" "ghi"))  ;; => "abc+def+ghi"
(s-join "\n" '("abc" "def" "ghi")) ;; => "abc\ndef\nghi"


;;; Predicates

;; string-equal wrapper
(s-equals? "abc" "ABC")    ;; => nil
(s-equals? "abc" "abc")    ;; => t
(string-equal "abc" "abc") ;; => t

;; string-lessp wrapper 昇順か？
(s-less? "abc" "abd")      ; => t
(string-lessp "abc" "abd") ; => t
(s-less? "abd" "abc")      ; => nil
(s-less? "abc" "abc")      ; => nil

;; string-match-p wrapper
(s-matches? "^[0-9]+$" "123")  ;; => t
(s-matches? "^[0-9]+$" "a123") ;; => nil
(s-matches? "1" "1a" 1)        ;; => nil

;; nil or empty ?
(s-blank? "")    ;; => t
(s-blank? nil)   ;; => t
(s-blank? " ")   ;; => nil

(s-present? "")  ;; => nil
(s-present? nil) ;; => nil
(s-present? " ") ;; => t

;; 指定文字列で開始・終了？
(s-ends-with? ".md" "readme.md")   ;; => t
(s-ends-with? ".MD" "readme.md")   ;; => nil
(s-ends-with? ".MD" "readme.md" t) ;; => t

(s-starts-with? "lib/" "lib/file.js")   ;; => t
(s-starts-with? "LIB/" "lib/file.js")   ;; => nil
(s-starts-with? "LIB/" "lib/file.js" t) ;; => t

;; 文字列を含むか？
(s-contains? "file" "lib/file.js")      ;; => t
(s-contains? "nope" "lib/file.js")      ;; => nil
(s-contains? "^a" "it's not ^a regexp") ;; => t

;; 小文字のみ？大文字のみ？混在？
(s-lowercase? "file") ;; => t
(s-lowercase? "File") ;; => nil
(s-lowercase? "filä") ;; => t

(s-uppercase? "HULK SMASH")     ;; => t
(s-uppercase? "Bruce no smash") ;; => nil
(s-uppercase? "FöB")            ;; => nil

(s-mixedcase? "HULK SMASH")     ;; => nil
(s-mixedcase? "Bruce no smash") ;; => t
(s-mixedcase? "BRÜCE")          ;; => nil

(s-capitalized? "Capitalized")      ;; => t
(s-capitalized? "I am capitalized") ;; => t
(s-capitalized? "I Am Titleized")   ;; => nil

;; 数値の文字列か？
(s-numeric? "123")         ;; => t
(s-numeric? "onetwothree") ;; => nil
(s-numeric? "7a")          ;; => nil


;;; misc

;; old -> new で置き換える
(s-replace "file" "nope" "lib/file.js")     ;; => "lib/nope.js"
(s-replace "^a" "\\1" "it's not ^a regexp") ;; => "it's not \\1 regexp"

;; 変換ルールをコンスセル(old . new)で指定
(s-replace-all '(("lib" . "test") ("file" . "file_test")) "lib/file.js") ;; => "test/file_test.js"
(s-replace-all '(("lib" . "test") ("test" . "lib")) "lib/test.js")       ;; => "test/lib.js"

;; 指定の書式へ変換
(s-downcase "ABC")       ;; => "abc"
(s-upcase "abc")         ;; => "ABC"
(s-capitalize "abc DEF") ;; => "Abc def"
(s-capitalize "abc.DEF") ;; => "Abc.def"
(s-titleize "abc DEF")   ;; => "Abc Def"
(s-titleize "abc.DEF")   ;; => "Abc.Def"

;; 文字列に FORM を適応 (FORM の末尾に文字列が引数として入る)
(s-with "   hulk smash   " s-trim s-upcase) ;; => "HULK SMASH"
(s-with "My car is a Toyota"
  (s-replace "car" "name")
  (s-replace "a Toyota" "Bond")
  (s-append ", James Bond")) ;; => "My name is Bond, James Bond"
(s-with "abc \ndef  \nghi"
  s-lines
  (mapcar 's-trim)
  (s-join "-")
  s-reverse) ;; => "ihg-fed-cba"

;; 開始のインデックスを得る
(s-index-of "abc" "abcdef")       ;; => 0
(s-index-of "CDE" "abcdef" t)     ;; => 2
(s-index-of "n.t" "not a regexp") ;; => nil

;; 文字列反転
(s-reverse "abc")    ;; => "cba"
(s-reverse "ab xyz") ;; => "zyx ba"
(s-reverse "")       ;; => ""

;; 文字列なら文字列を返す (s-present? でチェック)
(s-presence nil)   ;; => nil
(s-presence "")    ;; => nil
(s-presence "foo") ;; => "foo"

;; format
;; (s-format "${name}" 'gethash hash-table)
;; (s-format "${name}" 'aget alist)
;; (s-format "$0" 'elt sequence)

;; 連想リストから取得
(s-format "help ${name}! I'm ${malady}"
          'aget '(("name" . "nic") ("malady" . "on fire"))) ;; => "help nic! I'm on fire"
;; 関数から値を生成
(s-format "hello ${name}, nice day"
          (lambda (var-name) "nic")) ;; => "hello nic, nice day"
;; リストのインデックスで指定
(s-format "hello $0, nice $1"
          'elt '("nic" "day")) ;; => "hello nic, nice day"

;; let ローカル変数で置き換える
(let ((x 1))
  (s-lex-format "x is ${x}")) ; => "x is 1"
(let ((str1 "this") (str2 "that"))
  (s-lex-format "${str1} and ${str2}")) ;; => "this and that"
(let ((foo "Hello\\nWorld"))
  (s-lex-format "${foo}")) ;; => "Hello\\nWorld"

;; マッチした数をカウント
(s-count-matches "a" "aba")                              ;; => 2
(s-count-matches "a" "aba" 0 2)                          ;; => 1
(s-count-matches "\\w\\{2\\}[0-9]+" "ab1bab2frobinator") ;; => 2

;; 指定文字で囲む
(s-wrap "foo" "\"")    ;; => "\"foo\""
(s-wrap "foo" "(" ")") ;; => "(foo)"
(s-wrap "foo" "bar")   ;; => "barfoobar"

;; 単語のリストにする
(s-split-words "under_score")       ;; => ("under" "score")
(s-split-words "some-dashed-words") ;; => ("some" "dashed" "words")
(s-split-words "evenCamelCase")     ;; => ("even" "Camel" "Case")

;; CamelCase 変換
(s-lower-camel-case "some words")         ;; => "someWords"
(s-lower-camel-case "dashed-words")       ;; => "dashedWords"
(s-lower-camel-case "under_scored_words") ;; => "underScoredWords"

(s-upper-camel-case "some words")         ;; => "SomeWords"
(s-upper-camel-case "dashed-words")       ;; => "DashedWords"
(s-upper-camel-case "under_scored_words") ;; => "UnderScoredWords"

;; snake_case
(s-snake-case "some words")      ;; => "some_words"
(s-snake-case "dashed-words")    ;; => "dashed_words"
(s-snake-case "camelCasedWords") ;; => "camel_cased_words"

;; dashed
(s-dashed-words "some words")         ;; => "some-words"
(s-dashed-words "under_scored_words") ;; => "under-scored-words"
(s-dashed-words "camelCasedWords")    ;; => "camel-cased-words"

;; Capitalized
(s-capitalized-words "some words")         ;; => "Some words"
(s-capitalized-words "under_scored_words") ;; => "Under scored words"
(s-capitalized-words "camelCasedWords")    ;; => "Camel cased words"

;; Initials
(s-word-initials "some words")         ;; => "sw"
(s-word-initials "under_scored_words") ;; => "usw"
(s-word-initials "camelCasedWords")    ;; => "cCW"
