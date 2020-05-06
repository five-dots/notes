;;; debugging

;; `debug-on-error' t に設定しエラー時にデバッグ情報を表示させる方法

(defun f (x) (1+ (g x))) ; f -> g
(defun g (x) (let ((y 4)) (* y (h x)))) ; g -> h
(defun h (x) (/ x 2))


(setq debug-on-error nil)




;; edebug でステップ実行しながらデバッグを行なう

;; n next
;; c continue
;; i step-in
;; o step-out
;; h goto
;; e eval (変数の内容を確認)
;; q quit

(defun test-fun (x)
  (let ((var 1))
    (setq x (+ var x))
    (setq x (1+ x))
    (setq x (1+ x))
    (setq x (1+ x))
    x))

(test-fun 1)
