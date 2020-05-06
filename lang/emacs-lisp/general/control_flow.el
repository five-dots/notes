;;; Control flow

;; `if'
;; TRUE 節に 1 つしかフォームを置くことができない => `progn' を使う
;; インデントが見ずらいのが欠点

(let ((var 10))
  (if (= var 10)
      ;; TRUE
      (message "var is 10.")
    ;; FALSE
    (message "var is not 10.")))        ; => "var is 10."


(let ((var 10))
  (if (= var 10)
      ;; TRUE
      (progn
        (setq var (* var 2))
        (message "var is %d." var))
    ;; FALSE
    (message "var is not 10.")))        ; => "var is 20."



;; `when'/`unless'

;; t or nil の時のみ実行させる

(when (display-graphic-p)
  (message "GUI is enabled."))          ; => "GUI is enabled."

(unless (string= system-type "darwin")
  (message "This is not mac !"))        ; => "This is not mac !"



;; `cond'

;; 所謂 switch 文

(let ((a 10))
  (cond ((= a 10) "a is 10.")
        ((= a 15) "a is 15.")
        (t "a is not 10.")))            ; => "a is 10."



;; `dolist'

;; (dolist (ループ変数 リスト 返り値指定フォーム) 本体)
;; R の for (x in list) {} に近いイメージ

;; リストを逆順にする
(let (result)
  (dolist (x '(1 2 3 4))
    (setq result (cons x result)))
  result)                               ; => (4 3 2 1)


;; [RESULT] の部分で返り値に形式を指定できる
(let (result)
  (dolist (x '(1 2 3) (cons 'finished result))
    (setq result (cons x result)))) ; => (finished 3 2 1)



;; `dotimes'

;; 指定回数を実行
;; R の for (i in 1:10) {} に近いイメージ

(let ((result))
  (dotimes (i 20)
    (add-to-list 'result i))
  result)               ; => (19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)



;; `while'

(let ((i 0)
      result)
  (while (< i 3)
    (setq result (cons i result))
    (setq i (1+ i)))
  result) ; => (2 1 0)
