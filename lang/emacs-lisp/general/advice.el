;;; advice

;; `defadvice'
;; (defadvice FUNCTION (CLASS NAME [POSITION] [ARGLIST] FLAG ...)
;;    [DOCSTRING] [INTERACTIVE-FORM]
;;    [BODY])

;; CLASS = before | around | after | activation | deactivation.
;; (ad-activate 'hoge) で有効化


;; before advice

;; テスト用の関数
(defun f1 () (princ "f1"))
(with-output-to-string (f1))            ; => "f1"

(defadvice f1 (before before-f1 activate)
  (princ "before/"))

(with-output-to-string (f1))            ; => "before/f1"



;; before advice + function args

;; テスト用の関数
(defun f2 (str) (princ str))
(with-output-to-string (f2 "x"))     ; => "x"

(defadvice f2 (before before-f2 activate)
  ;; 元の関数の引数にアクセスできる
  (princ (format "%s/" (upcase str))))

(with-output-to-string (f2 "hoge"))   ; => "HOGE/hoge"



;; after advice

;; テスト用の関数
(defun f3 () (princ "hoge"))
(with-output-to-string (f3))            ; => "hoge/after"

(defadvice f3 (after after-f3 activate)
  (princ "/after"))

(with-output-to-string (f3))            ; => "hoge/after"



;; after advice + `ad-return-value'

;; テスト用の関数
(defun f4 (a b) (+ a b))
(f4 1 2)                                ; => 4

;; 元の関数の返り値には、ad-return-value でアクセスする
(defadvice f4 (after after-f4 activate)
  (setq ad-return-value (+ ad-return-value 1)))

(f4 1 2)                                ; => 4



;; around advice + `ad-do-it'

;; テスト用の関数 2
(defun f5 () (princ "hoge"))

(defadvice f5 (around around-f5 activate)
  (princ "before/")
  ;; ad-do-it で元の処理を呼び出す
  ad-do-it
  (princ "/after"))

(with-output-to-string (f5))            ; => "before/hoge/after"
