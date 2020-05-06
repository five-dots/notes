;; string=

"hoge"                                  ; => "hoge"

(type-of "hoge")                        ; => string
(stringp "hoge")                        ; => t
(sequencep "hoge")                      ; => t
(listp "hoge")                          ; => nil

;; 文字列の長さ
(length "hoge")                         ; => 4
(length "浅井")                         ; => 2

(string-width "hoge")                   ; => 4
(string-width "浅井")                   ; => 4



;;; 比較

;; `string='/`string-equal'
;; - text property は無視される

(string= "hoge" "hoge")                 ; => t

;; case-sensitive
(string= "Hoge" "hoge")                 ; => nil

;; string= と同じ
(string-equal "hoge" "hoge")            ; => t



;;; 書式

;; `format'
;; `%s' = string
;; `%d' = decimal
;; `$f' = float (桁数指定)

(format "My name is %s %s." "Shun" "Asai") ; => "My name is Shun Asai."
(format "I'm %d years old." 36)            ; => "I'm 36 years old."
(format "Pi is %.2f." 3.141)               ; => "Pi is 3.14."



;;; 文字列の操作

;; `concat' 文字列の結合
(concat "hoge" "fuga")                  ; => "hogefuga"


;; `substring' 文字列の抽出
;; from/to で抽出
(substring "hoge" 2 4)                  ; => "ge"


;; `split-string' 文字列の分割
;; SEPARATOR を省略した場合には、空白文字列 (スペース、タブ、改行など) で分割
(split-string "hoge fuga piyo")         ; => ("hoge" "fuga" "piyo")
(split-string "hoge, fuga, piyo" ", ")  ; => ("hoge" "fuga" "piyo")


;; `downcase'/`upcase'/`capitalize'
;; 大文字・小文字の変換
(downcase "HOGE")                       ; => "hoge"
(upcase "hoge")                         ; => "HOGE"
(capitalize "hoge")                     ; => "Hoge"



;;; 正規表現

;; `string-match'
;; 文字列が正規表現にマッチした場合、マッチしたインデックスを返す
;; マッチしない場合 nil を返す
(string-match ".x" "ax")                ; => 0
(string-match ".a" "ax")                ; => nil

;; 大文字・小文字の区別を一時的に変更する
(let ((case-fold-search t))
  (string-match ".x" "AX"))             ; => 0


;; `match-string'
;; 正規表現にマッチした文字列を抽出する



;;; 文字列を出力する

;; `message'
;; エコーエリアに出力
;; *Message* buffer にも表示される
(message "hoge %s" "fuga")              ; =>


;; `error'
;; エラーメッセージを出力する
;; (error "error %s" "fuga!")


;; `princ'
;; 標準出力に出力する
(princ "Hello world.")                  ; =>

;; 標準出力を文字列にリダイレクトする
;; バッファではなく、文字列にリダイレクトすることで、検証が楽になる
(with-output-to-string
  (princ "hoge.")
  (princ "fuga."))                      ; =>
