
;;; vector

;; `array' の一種
;; リストよりも n 番目の要素へのアクセスが楽
;; ただし、リストとは違い固定長

[1 2 3]                               ; => [1 2 3]
["hoge" "fuga" "piyo"]                ; => ["hoge" "fuga" "piyo"]
['foo 'bar]                           ; => [(quote foo) (quote bar)]

[1 2 3 [4 5 6]]                         ; => [1 2 3 [4 5 6]]

(vector 1 2 3)                          ; => [1 2 3]
(setq vector-var [1 2 3])               ; => [1 2 3]

(vectorp vector-var)                    ; => t
(arrayp vector-var)                     ; => t
(sequencep vector-var)                  ; => t
(listp vector-var)                      ; => nil
(consp vector-var)                      ; => nil


;; Access by index

;; `elt' はシーケンス全般に使える
(elt vector-var 1)                      ; => 2

(aref vector-var 1)                     ; => 2
(last vector-var)                       ; => [1 2 3]


;; 以下のリスト用関数は使えない
;; (nth 1 vector-var)
;; (car vector-var)
;; (first vector-var)


;; `aset'
;; 値を変更 (破壊的)
(let ((ary [1 2 3]))
  (aset ary 0 5)
  ary)                                   ; => [5 2 3]
