;;; misc-init.el

;; ----------------------------------------------------------------------
;; @ session.el
;;   kill-ringやミニバッファで過去に開いたファイルなどの履歴を保存する

(setq load-path
      (cons (expand-file-name "~/.emacs.d/elisp/session/lisp/") load-path))
(when (require 'session nil t)
  (setq session-save-file "~/.emacs.d/var/session/.session")
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  ; ミニバッファ履歴リストの最大長：tなら無限
  (setq history-length t)
  (add-hook 'after-init-hook 'session-initialize)
  ;; 前回閉じたときの位置にカーソルを復帰
  (setq session-undo-check -1)
  )


;; ----------------------------------------------------------------------
;; @ info
;;   infoファイルを置くディレクトリを設定する

(setq Info-directory-list
      (append
    Info-default-directory-list
       (list
     (expand-file-name "~/.emacs.d/share/info")
        (expand-file-name "~/.emacs.d/share/info/python2.5")
     (expand-file-name "~/.emacs.d/share/info/EmacsFAQ")
        )
    ))

;; ----------------------------------------------------------------------
;; @ dmacro
;;   繰り返し操作をおこなうことができる

(defconst *dmacro-key* [f7] "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

;; ----------------------------------------------------------------------
;; @ sr-speedbar
(when (autoload-if-found 'sr-speedbar "sr" "" t)
  (eval-after-load "sr-speedbar"
    '(progn
       ;;(setq sr-speedbar-right-side nil);;左側に表示
       (global-set-key (kbd "<f11>") 'sr-speedbar-toggle)
       )))



