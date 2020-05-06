
;;; 基本的な比較
(= 10 10)                               ; => t
(/= 10 9)                               ; => t
;; 10 > 9
(> 10 9)                                ; => t
;; 9 < 10
(< 9 10)                                ; => t



;;; 論理演算

;; t = TRUE, `nil' = FALSE を表す
;; nil 以外の評価結果は t として扱われる

(not nil)                               ; => t
(not t)                                 ; => nil
(not 0)                                 ; => nil
(not 1)                                 ; => nil
(not -1)                                ; => nil


(and t t)                               ; => t
(and t nil)                             ; => nil

;; `or' = 最初のゼロでない値を返す

(or t nil)                              ; => t
(or nil nil)                            ; => nil
(or nil 1)                              ; => 1



;;; 同値

;; `eq' = 同一性
;; - 同じものかどうか
;; - 整数・シンボルは常に唯一の存在
;; - `eq' はより強い比較のため `equal' を含む

(eq 1 1)                                ; => t
(eq 'foo 'foo)                          ; => t
(eq "foo" "foo")                        ; => nil

(setq obj1 '(1 2))
(eq obj1 obj1)                          ; => t

(setq obj2 obj1)
(eq obj1 obj2)                          ; => t

;; `equal' = 同値性:
;; - 比較対象が同じかどうか
;; - 文字列や実数の比較につかう
;; - list の場合は、全ての要素を比較し、全て t なら t を返す

(equal 1.1 1.1)                         ; => t
(equal "hoge" "hoge")                   ; => t
(equal 1 1)                             ; => t
(equal '(1 2 3) '(1 2 3))               ; => t
(equal '(1 2 3) '(1 2 4))               ; => nil
