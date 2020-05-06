;;; window

;; `selected-window'
;; 現在の window object を取得する

(setq cur-win (selected-window))        ; => #<window 76 on window.el>

(type-of cur-win)                       ; => window
(windowp cur-win)                       ; => t



;; ウィンドウの分割

;; `split-window-vertically'/`split-window-horizontally'
;; デフォルトでは等分する
;; サイズは現在のウィンドウのサイズを整数で指定
;; マイナスのサイズを指定すれば、分割された側のサイズを指定できる

;; (split-window-vertically -10) ; 下にサイズ 10
;; (split-window-horizontally)


;; `one-window-p'
;; 分割されているか確認する
(one-window-p)                          ; => nil



;; ウィンドウの削除

;; `delete-window'
;; 引数を省略すると自分自身を削除
;; (delete-window)


;; `delete-other-windows'
;; 自分以外のウィンドウを削除
;; (delete-other-windows)



;; ウィンドウの選択

;; `select-window'
;; 指定したウィンドウオブジェクトを選択する
;; split と組合せると分割したウィンドウへ飛ぶことができる
;; (select-window (split-window-vertically))

;; `save-selected-window'
;; 元に選択していたウィンドウへ戻ってくる場合に利用する
;; 下に分割したウィンドウに文字を表示し、元のウィンドウに戻ってくる例
;; (save-selected-window
;;   (select-window (split-window-vertically -10))
;;   (switch-to-buffer "*scratch*")
;;   (insert "hoge"))



;; ウィンドウとバッファ

;; `window-buffer'
;; ウィンドウが表示しているバッファを得る
(window-buffer)                         ; => #<buffer window.el>


;; `get-buffer-window'
;; バッファを表示しているウィンドウを得る
(get-buffer-window)                     ; => #<window 76 on window.el>



;; バッファを表示する

;; `switch-to-buffer-other-window'
;; 表示方法のカスタマイズはできない
;; (switch-to-buffer-other-window "*scratch*")


;; ウィンドウのサイズ・とサイズ変更

(window-height)                         ; => 67
(window-width)                          ; => 138

;; ウィンドウサイズの拡大・縮小
(enlarge-window 1)
(shrink-window 1)

(enlarge-window-horizontally 1)
(shrink-window-horizontally 1)


;; 自動的にバッファサイズに合わせる
(shrink-window-if-larger-than-buffer)


