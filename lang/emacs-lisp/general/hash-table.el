;;; hash-table

;; `make-hash-table' でハッシュテーブルを作成
;; `:test' で比較の関数を指定 (eq, eq or equal)

(make-hash-table :test 'eq)             ; => #s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data ())


;; 空のハッシュテーブル
(setq hash-var (make-hash-table :test 'equal)) ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ())


;; 作成時にデータも投入する書き方
(setq hash-var
      #s(hash-table test equal data("one" 1 "two" 2 "three" 3))) ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("one" 1 "two" 2 "three" 3))


;; `puthash' で要素を追加する
(puthash "one" 1 hash-var)              ; => 1
hash-var                                ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("one" 1 "two" 2 "three" 3))


;; `gethash' でキーから要素を取得する
(gethash "one" hash-var)                ; => 1


;; `remhash' で要素を削除する
(remhash "one" hash-var)                ; => nil
hash-var                                ; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ( "two" 2 "three" 3))



;;; predicate

(hash-table-p hash-var)                 ; => t
(listp hash-var)                        ; => nil
(sequencep hash-var)                    ; => nil
