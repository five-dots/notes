
;;; Global variable

;; `setq' = global variable
;; いきなり setq で変数を宣言できる
(setq a 100)                            ; => 100
a                                       ; => 100


;; package 内で使う変数などは、明示的に宣言した方が良い
;; - パッケージ変数は prefix-, prefix-- (インターナル) を付ける
(defvar test-var "hoge"
   "Use defvar to define a variable in elisp file.") ; => test-var


;; 定数を定義
(defconst tmk-hoge "tmk-hoge-const")    ; => tmk-hoge


;; まとめて設定できる
(setq hoge "hoge"
      fuga "fuga")

hoge                                    ; => "hoge"
fuga                                    ; => "fuga"



;;; local variable

;; `let' でローカル変数を宣言
;; let 内で同名のグルーバル変数を編集しても影響しない
(setq x 1) ; => 1
(let ((x 3))
  x                                     ; => 3
  )
x                                       ; => 1

;; `let*' を使えば、ローカル変数宣言ブロックの前の変数にアクセスできる
(let* ((foo 10)
       (bar (+ foo 20)))
  bar)                                    ; => 30

(letf)



;;; Evaluation during compilation
;; elisp -> (バイトコンパイル) -> バイトコード (マシン非依存のコード)
;; バイトコードインタプリタによって実行される

;; コンパイル時にのみ評価され、ロード時には評価されない (評価結果は定数になる)
(defvar my-regexp
   (eval-when-compile (regexp-opt '("aaa" "aba" "abb")))) ; => my-regexp


