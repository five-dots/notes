;;; macro

;; マクロ = S式からS式への展開規則

;; `defmacro'
;; ` で S 式内に変数を埋め込み
;; ,変数名  = 式が一つの場合
;; ,@変数名 = 式が複数の場合

(defmacro my-when (condition &rest body)
  ;; インデントを浅くする引数の位置を指定 (0から開始)
  (declare (indent 1))
  `(if ,condition
       (progn ,@body)))


;; Expand macro で展開後の内容を確認できる
(my-when t
  (+ 1 2))                              ; => 3


(when t
  (+ 1 2))                              ; => 3
