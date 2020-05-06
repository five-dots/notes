;;; コンスセル (cons cell)

;; 2 つの値のペア
;; (cons car cdr) と '(car . cdr) は同じ

(cons 1 "one")                          ; => (1 . "one")
'(1 . "one")                            ; => (1 . "one")

;; getters
(car (cons 1 "one"))                    ; => 1
(cdr (cons 1 "one"))                    ; => "one"

;; car -> car
(caar '((1 . 2) (3 . 4)))               ; => 1

;; car -> cdr
(cadr '((1 . 2) (3 . 4)))               ; => (3 . 4)
(cdr '((1 . 2) (3 . 4)))                ; => ((3 . 4))

;; car -> cdr
(cdar '((1 . 2) (3 . 4)))               ; => 2


;;; リスト

;; (list foo bar) と '(foo bar) は同じ

(list 1 2 3)                            ; => (1 2 3)
'(1 2 3)                                ; => (1 2 3)
'("foo" "bar")                          ; => ("foo" "bar")

(car '(1 2 3))                          ; => 1
(cdr '(1 2 3))                          ; => (2 3)
(last '(1 2 3))                         ; => (3)


;; quote されていれば、list = data として扱われる
'(+ 1 2)                                ; => (+ 1 2)

;; 評価される場合、フォームと呼ばれる
(+ 1 2)                                 ; => 3

;; list を評価することも可能
(eval '(+ 1 2))                         ; => 3


;; `add-to-list' list の先頭に追加する
(setq list-var '(1 2 3 4))
(add-to-list 'list-var 5)               ; => (5 1 2 3 4)

;; 既に存在する場合には、追加されない
(add-to-list 'list-var 4)               ; => (5 1 2 3 4)



;; 型の確認

(consp '(1 . 2))                        ; => t
(listp '(1 . 2))                        ; => t
(listp '(1 2 3))                        ; => t
(sequencep '(1 2 3))                    ; => t

(type-of '(1 . 2))                      ; => cons
(type-of '(1 2 3))                      ; => cons


;;; リスト要素の抽出

(setq list-var '(1 2 3 4))

(car list-var)                          ; => 1
;; car と同じ
(first list-var)                        ; => 1
(last list-var)                         ; => (4)

(nth 0 list-var)                        ; => 1
(nth 1 list-var)                        ; => 2
(nth 9 list-var)                        ; => nil

(elt list-var 0)                        ; => 1
(elt list-var 1)                        ; => 2
(elt list-var 9)                        ; => nil



;;; リスト要素の追加

(setq list-var1 '(1 2 3 4))             ; => (1 2 3 4)

;; `add-to-list'
;; 先頭に追加
(add-to-list 'list-var1 5)               ; => (5 1 2 3 4)

;; 重複は追加されない
(add-to-list 'list-var1 5)               ; => (5 1 2 3 4)

;; `append'
;; 末尾に追加する
(append list-var '(1))                  ; => (1 2 3 4 1)

;; 入れ子も表現できる
(append list-var '((1 . 2)))            ; => (1 2 3 4 (1 . 2))



;;; リスト要素の削除

;; `delq' or `delete'
;; 元のリストを破壊的に変更してしまうので、元リストのコピーを用意しておくとよい

(setq list-var1 '(1 2 3 4))              ; => (1 2 3 4)

;; `delq' は `eq' で比較を行なうので、数値やシンボル対して使う
(delq 1 list-var1)                       ; => (2 3 4)


(setq list-var2 '("foo" "bar" "bar"))   ; => ("foo" "bar" "bar")

;; `delete' は `equal' で比較を行なうので、文字列に対して使う
;; 複数マッチする場合は、全て削除
(delete "bar" list-var2)                ; => ("foo")



;;; リストが要素を含んでいるか

;; `memq' は eq で比較する
;; - ELT が LIST に含まれる場合 ELT が car のリストを返す

(memq 3 '(1 2 3))                       ; => (3)
(memq 3 '(1 2 3 4 5))                   ; => (3 4 5)

(memq major-mode '(emacs-lisp-mode lisp-interaction-mode)) ; => (emacs-lisp-mode lisp-interaction-mode)

;; eq での比較なので、文字列ではマッチしない
(memq "foo" '("foo" "bar"))             ; => nil


;; `member' は equal で比較する

(member "foo" '("foo" "bar"))           ; => ("foo" "bar")
(member 1.1 '(1.1 2.2 3.3))             ; => (1.1 2.2 3.3)



;;; alist (Association List)

;; - 最初の要素がキーになっている
;; - リストが入れ子になっているもの
;; - 子要素は cons cell でなくても普通のリストでもよい


;; シンボルがキーになる場合
(setq alist1 '((foo . 1) (bar . 2) (piyo . 3)))

;; 文字列がキーになる場合
(setq alist2 '(("foo" . 1) ("bar" . 2) ("piyo" . 3)))


;; キーからペアを取得する
;; `assq' は `eq' で取得
(assq 'foo alist1)                      ; => (foo . 1)

;; `assoc' は `equal' で取得
(assoc "foo" alist2)                    ; => ("foo" . 1)

;; キーから値を得る (シンボル・文字列両対応)
(assoc-default 'foo alist1)             ; => 1
(assoc-default "foo" alist2)            ; => 1

;; 値からペアを得る
(rassq 1 alist1)                        ; => (foo . 1)
(rassoc 1 alist2)                       ; => ("foo" . 1)


;;; plist (Property List)

;; シンボルに属性情報を付与することができる
;; `put', `get' で簡単に操作
;; plist = (KEY1 VALUE1 KEY2 VALUE2) の形式になっているリスト

;; sym に属性を付与
(put 'me 'weight 90)                    ; => 90
(put 'me 'height 185)                   ; => 185

;; 属性を取り出す
(get 'me 'weight)                       ; => 90
(symbol-plist 'me)                      ; => (weight 90 height 185)


(setq plist1 '(foo 1 bar 2))            ; => (foo 1 bar 2)
;; 典型的にはこんな形
(setq plist2 '(:foo 1 :bar 2))          ; => (:foo 1 :bar 2)


;; car が PROP になるように返す
(plist-member plist1 'foo)              ; => (foo 1 bar 2)
(plist-member plist2 ':foo)             ; => (:foo 1 :bar 2)


(plist-get plist1 'foo)                 ; => 1
(plist-get plist2 ':foo)                ; => 1


(plist-put plist2 ':ba 3)               ; => (:foo 1 :bar 2 :ba 3)

(plist-delete! plist1 'foo)             ; => (bar 2)


;;; nil in list

;; 空のリストは nil
(listp nil)                             ; => t
(listp ())                              ; => t


'(1 . nil)                              ; => (1)
(cdr '(1 . nil))                        ; => nil


;; `null' は nil の type predicate
(null nil)                              ; => t
(null 1)                                ; => nil
