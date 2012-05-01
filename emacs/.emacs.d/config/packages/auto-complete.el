;;; auto-complete-init.el

(when (autoload-if-found 'auto-complete-config "auto-complete" nil t)
  (eval-after-load "auto-complete-config"
    '(progn
       (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/dict")
       (ac-config-default)

       ;; 補完推測機能のデータを永続化するファイル名を指定
       (setq ac-comphist-file "~/.emacs.d/var/auto-complete/ac-comphist.dat")
       )))
