;;; buffer


;;; バッファ取得

;; `buffer-list'
;; 現在開いているバッファをリストとして取得する

(setq buf-list (buffer-list))           ; => (#<buffer buffer.el> #<buffer  *Minibuf-1*> #<buffer *scratch*> #<buffer window.el> #<buffer config.el> #<buffer  *Treemacs-Scoped-Buffer-#<frame config.el – Doom Emacs 0x1434c30>*> #<buffer *doom*> #<buffer index.org> #<buffer  *Minibuf-0*> #<buffer *Messages*> #<buffer  *code-conversion-work*> #<buffer  *Echo Area 0*> #<buffer  *Echo Area 1*> #<buffer  *code-converting-work*> #<buffer  *ivy-posframe-buffer*> #<buffer  *pfuture stderr dummy*> #<buffer  *counsel*> #<buffer  *lispxmp tmp*> #<buffer *flycheck-posframe-buffer*> #<buffer  *skk-jisyo*> #<buffer  *SKK-JISYO.L*> #<buffer  *skkserv*> #<buffer  *which-key*>), (#<buffer buf> #<buffer  *Minibuf-1*> #<buffer *scratch*> #<buffer window.el> #<buffer config.el> #<buffer  *Treemacs-Scoped-Buffer-#<frame config.el – Doom Emacs 0x1434c30>*> #<buffer *doom*> #<buffer index.org> #<buffer  *Minibuf-0*> #<buffer *Messages*> #<buffer  *code-conversion-work*> #<buffer  *Echo Area 0*> #<buffer  *Echo Area 1*> #<buffer  *code-converting-work*> #<buffer  *ivy-posframe-buffer*> #<buffer  *pfuture stderr dummy*> #<buffer  *counsel*> #<buffer  *lispxmp tmp*> #<buffer *flycheck-posframe-buffer*> #<buffer  *skk-jisyo*> #<buffer  *SKK-JISYO.L*> #<buffer  *skkserv*> #<buffer  *which-key*>)
(type-of buf-list)                      ; => cons, cons
(car buf-list)                          ; => #<buffer buffer.el>, #<buffer buf>


;; `get-buffer'
;; バッファ名もしくはバッファオブジェクトで指定
;; 存在しない場合は nil
(setq msg-buf (get-buffer "*Messages*")) ; => #<buffer *Messages*>, #<buffer *Messages*>
(get-buffer "not exists")                ; => nil, nil


;; `current-buffer'
(setq cur-buf (current-buffer))         ; => #<buffer buffer.el>, #<buffer buf>
(bufferp cur-buf)                       ; => t, t


;; バッファ情報
(type-of msg-buf)                       ; => buffer, buffer
(bufferp msg-buf)                       ; => t, t
(buffer-live-p msg-buf)                 ; => t, t
(buffer-name msg-buf)                   ; => "*Messages*", "*Messages*"
(buffer-file-name)                      ; => "/home/shun/Dropbox/repos/github/five-dots/notes/lang/emacs-lisp/general/buffer.el", "/home/shun/Dropbox/repos/github/five-dots/notes/lang/emacs-lisp/general/buffer.el"
buffer-file-name                        ; => "/home/shun/Dropbox/repos/github/five-dots/notes/lang/emacs-lisp/general/buffer.el", "/home/shun/Dropbox/repos/github/five-dots/notes/lang/emacs-lisp/general/buffer.el"
(rename-buffer "buf")                   ; => "buf", "buf"
major-mode                              ; => emacs-lisp-mode, emacs-lisp-mode


;; 接尾語がついたバッファ名を自動作成
;; (rename-uniquely)                       ; => nil



;;; バッファ切り替え

;; `switch-to-buffer'
;; 副作用だけを目的とした関数
;; バッファ名もしくは、バッファオブジェクトで切り替え
;; バッファを表示する目的で使う。表示しなくてよい場合は、`with-current-bufer' を使う

;; (switch-to-buffer "*scratch*")


;;; バッファ作成

;; `get-buffer-create'
;; create buffer if not exists
;; すでに存在する場合は、そのバッファを返す

;; (setq temp-buf (get-buffer-create "hoge"))
;; (bufferp temp-buf)
;; (switch-to-buffer temp-buf)
;; (buffer-name temp-buf)

;; `generate-new-buffer'
;; create buffer
;; 新しくバッファを作成。複数ある場合は、<2> などの suffix がつく

;; (generate-new-buffer "hoge")


;; `find-file'
;; create from file
;; ファイルが存在しない場合、空のバッファを作成する

;; (find-file "~/Dropbox/repos/github/five-dots/notes/lang/emacs-lisp/general/list.el")
;; (find-file "~/not_exist")



;;; バッファの削除

;; `kill-buffer'
;; バッファ名を省略すれば、現在のバッファを削除する

;; (kill-buffer)
;; (kill-buffer "*scratch*")
;; (kill-buffer "test.el")



;;; バッファへの表示

;; 標準出力(デフォルトでエコーエリア)に出力する
(princ "hoge")                          ; => "hoge", "hoge"


;; `with-output-to-temp-buffer'
;; Show string to temp buffer

;; (with-output-to-temp-buffer "*Hello*"
;;   (princ "Hello, world."))


;; `temp-buffer-show-function' で temp buffer がどのように表示されるかを制御

;; (let ((temp-buffer-show-function 'switch-to-buffer))
;;   (with-output-to-temp-buffer "*Hello*"
;;     (princ "Hello, world.")))


;; `with-current-buffer'
;; カレントバッファを一時的に変更して関数を実行する

;; カレントバッファに対して作用する関数が多いため
;; (with-current-buffer "temp.R" major-mode)


;; `with-temp-buffer'
;; Change the current buffer to temp buffer temporariry

;; (with-temp-buffer major-mode)
;; (with-temp-buffer (buffer-name))


;; `insert'
;; 現在のカーソル位置に書き込む
(insert "hoge")hogehoge

;; (insert-buffer)
;; (insert-buffer-substring)



;;; バッファローカル変数

;; バッファ単位で保持している値が異なる変数

;; `make-variable-buffer-local'
;; 変数を buffer local に設定する
(defvar local-var1 'global)
(defvar local-var2 nil)
(make-variable-buffer-local 'local-var2)

;; `setq-default'
;; buffer local 変数の初期値を設定
(setq-default local-var2 'automatically-local)
(with-temp-buffer
  ;; 変数をバッファローカルに設定し、初期値も設定する
  (set (make-local-variable 'local-var1) 'local1)
  local-var1
  ;; バッファローカルになるまえのデフォルト値
  (default-value 'local-var1)
  local-var2
  (setq local-var2 1)
  local-var2
  (setq-default local-var2 'automatically-local-modifiled)
  local-var2
  (default-value 'local-var2)
  )

local-var1
local-var2



;;; バッファ内の位置を得る

;; Point in buffer

;; 現在位置
(point)                                 ; => 4019
;; バッファの先頭
(point-max)                             ; => 5391
;; バッファの末尾
(point-min)                             ; => 1
;; beggining of line
(point-at-bol)                          ; => 4283
;; end of line
(point-at-eol)                          ; => 4487

;; Predicates

;; begning of the buffer
(bobp)                                  ; => nil
;; end of the buffer
(eobp)                                  ; => nil
;; begining of the line
(bolp)                                  ; => nil
;; end of the lien
(eolp)                                  ; => nil



;;; 移動

;; `goto-char'
;; point もしくは marker で指定する
(goto-char (point-min))
(goto-char (point-max))
(goto-char 1000)
(goto-line 1)


;; `forward-char'
(forward-char 2)
(forward-char -2)
(forward-line -2)
(forward-word -2)
(forward-sexp -2)


;; `save-excursion'
;; 現在の状態を保存. バッファ・ポイント・マーカーを保存
;; 移動の処理をした後に、元の場所へ戻ってきたい場合に使う

;; (save-excursion)



;;; 削除

;; (erase-buffer)
;; (delete-region)
;; (delete-char -3)
