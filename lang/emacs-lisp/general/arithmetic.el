
;;; 四則演算
(+ 1 2)                                 ; => 3
(- 7 3)                                 ; => 4
(* 2 4)                                 ; => 8
(/ 10 3)                                ; => 3
(/ 10 3.0)                              ; => 3.3333333333333335

;; インクリメント・デクリメント
(1+ 2)                                  ; => 3
(1- 2)                                  ; => 1

;;; 剰余
(% 10 3)                                ; => 1


;;; 累乗
(expt 2 8)                              ; => 256


;;; 数値の型、リテララル
(type-of 1)                             ; => integer
(type-of 1.1)                           ; => float
(type-of 1e-10)                         ; => float

(numberp 1)                             ; => t
(numberp 1.1)                           ; => t
(integerp 1)                            ; => t
(integerp 1.1)                          ; => nil
(floatp 1)                              ; => nil
(floatp 1.1)                            ; => t

;;; 関数
(max 10 2 100)                          ; => 100
(min 10 -1 -5)                          ; => -5

(round 1.5)                             ; => 2
(ceiling 1.5)                           ; => 2
(floor 1.5)                             ; => 1

(log 10)                                ; => 2.302585092994046
(log10 100)                             ; => 2.0

;; e
(exp 1)                                 ; => 2.718281828459045

;; 数字を合計するための sum 関数は存在しない
(+ 1 2 3)                               ; => 6
(apply '+ '(1 2 3))                     ; => 6
(reduce '+ '(1 2 3))                    ; => 6


;; second from 1970/01/01
(float-time)                            ; => 1587480393.0919778

;;; 最大値・最小値
most-positive-fixnum                    ; => 2305843009213693951
most-negative-fixnum                    ; => -2305843009213693952
most-positive-float                     ; => nil
most-negative-float                     ; => nil
