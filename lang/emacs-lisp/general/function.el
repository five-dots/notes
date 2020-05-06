
;;; defun

;; 関数定義
;; `&optional' は省略可能引数。省略時には nil が代入される。
;; `&rest' は可変長引数。リストとして渡される。

(defun func1 (a &optional b &rest c)
  (setq b (or b 20))
  (list a b c))

(func1 1)                               ; => (1 20 nil)
(func1 1 2)                             ; => (1 2 nil)
(func1 1 2 3)                           ; => (1 2 (3))



;;; cl-defun

;; cl-macs に収録されている
;;   - cl-macs は Common Lisp ライクな機能をもたせるためのマクロ集)
;; `&key' キーワード引数を設定できる。キーワードは `:keyword' の形式で指定
;; `defun*' は cl-defun のエイリアス

(cl-defun hello (&key (name "Shun"))
  (interactive)
  (message (format "Hello, %s !" name)))


(hello)                                 ; => "Hello, Shun !"
(hello :name "Shiori")                  ; => "Hello, Shiori !"

;; feature を宣言
(provide 'my-funcs)



;;; lambda

;; 無名関数を宣言できる

(lambda () (message "hoge"))            ; => (lambda nil (message "hoge"))

;; 関数を変数に格納する
(setq func-var (lambda () (message "hoge"))) ; => (lambda nil (message "hoge"))

;; `funcall' で呼び出し
(funcall func-var)                      ; => "hoge"

;; `ARGUMENTS' は `&rest' なので、リストとして渡さずにそのまま渡す
(funcall '+ 1 2 3)                      ; => 6



;;; command

;; `interactive' 指定することで、コマンドとして利用できる
;; コマンド = 実行時に引数を動的指定できるもの

;; tests of interactive code chars
;; `interactive' の後の文字で指定された形式で `arg' が変換される
(defun testint-b (arg) (interactive "bExisting Buffer name: ") (message "%S" arg))
(defun testint-B (arg) (interactive "BBuffer name: ") (message "%S" arg))
(defun testint-D (arg) (interactive "DDirectory name: ") (message "%S" arg))
(defun testint-f (arg) (interactive "fExisting file name: ") (message "%S" arg))
(defun testint-F (arg) (interactive "FFile name: ") (message "%S" arg))
(defun testint-n (arg) (interactive "nNumeric: ") (message "%S" arg))
(defun testint-s (arg) (interactive "sString: ") (message "%S" arg))
