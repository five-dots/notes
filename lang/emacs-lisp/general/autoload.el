;;; autoload

;; 遅延ロードを実現する仕組み
;; 関数を実行したときに、そのファイルがロードされる仕組み
;; 起動時にはロードしないで済むため、起動が早い
;; autoload を設定することで、ロードしていなくても関数名が見えるようになる
;; package.el によって、autoload に関する設定が自動生成される

;;; autoload cookie
;; 関数の前に前置する

;;;###autoload
(defun hoge-autoload () (message "hoge"))

;;; eval-after-load
;; 遅延ロードされるパッケージの変数などに対して設定をする場合
;; progn の quote がないとその場で評価されてしまう
(eval-after-load "mode-name"
  '(progn ("hoge")))
