;;; custom

;; defvar と比較してのメリット
;; - getter, setter
;; - type check
;; - customize の UI に統合される

;; defcustomで定義された変数はsetqではなくcustom-set-variablesで設定すべき理由
;; http://kawamuray.hatenablog.com/entry/2013/11/03/180543


(defcustom my-custom-var nil "my first custom var."
  ;; `:type'
  ;; VALUE should be a widget type for editing the symbol’s value.
  ;; Every ‘defcustom’ should specify a value for this keyword.

  ;; `:options'
  ;; VALUE should be a list of valid members of the widget type.

  ;; `:initialize'
  ;; VALUE should be a function used to initialize the
  ;; variable.  It takes two arguments, the symbol and value
  ;; given in the ‘defcustom’ call.  The default is
  ;; ‘custom-initialize-reset’.

  ;; `:set'
  ;; VALUE should be a function to set the value of the symbol
  ;; when using the Customize user interface.  It takes two arguments,
  ;; the symbol to set and the value to give it.  The function should
  ;; not modify its value argument destructively.  The default choice
  ;; of function is ‘set-default’.

  ;; `:get'
  ;; VALUE should be a function to extract the value of symbol.
  ;; The function takes one argument, a symbol, and should return
  ;; the current value for that symbol.  The default choice of function
  ;; is ‘default-value’.

  ;; `:require'
  ;; VALUE should be a feature symbol.  If you save a value
  ;; for this option, then when your init file loads the value,
  ;; it does (require VALUE) first.

  ;; `:set-after' VARIABLES
  ;; Specifies that SYMBOL should be set after the list of variables
  ;; VARIABLES when both have been customized.

  ;; `:risky'
  ;; Set SYMBOL’s ‘risky-local-variable’ property to VALUE.

  ;; `:safe'
  ;; Set SYMBOL’s ‘safe-local-variable’ property to VALUE.
  ;; See Info node ‘(elisp) File Local Variables’.
  )
my-custom-var                           ; => 20

;; これまで通り、`setq' で値の変更ができる
(setq my-custom-var 10)                 ; => 10

;; custom を使って変更しても同じ
(custom-set-variables '(my-custom-var 20))
my-custom-var                           ; => 20



;;; setter を使う場合の例

;; variable, value を受け取る関数を渡す
;; lambda 式を渡しても良いし、別途関数を定義しても良い


;; 値を 2倍にして設定する setter 関数
(defun my-double-setter (variable value)
  (set-default variable (* value 2)))

(defcustom my-setter-var 0 "my first setter variable"
  :set 'my-double-setter)


(custom-set-variables '(my-setter-var 20))
my-setter-var                           ; => 40


;; setq では setter が経由されない
(setq my-setter-var 30)                 ; => 30


;; custom 変数の場合は non-nil を返す
(custom-variable-p 'my-setter-var)      ; => (0)
