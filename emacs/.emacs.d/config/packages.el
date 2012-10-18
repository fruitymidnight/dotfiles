;; el-get.el の設定
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; レシピ置き場
(add-to-list 'el-get-recipe-path
             (expand-file-name
              (concat user-emacs-directory "config/el-get/recipes")))
;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path
             (expand-file-name
              (concat user-emacs-directory "config/el-get/local-recipes")))

(el-get 'sync)


; インストールしたelispの設定
(load "config/packages/init-anything")
(load "config/packages/init-autocomplete")
(load "config/packages/init-elscreen")
(load "config/packages/init-magit")
(load "config/packages/init-c")
(load "config/packages/init-lua")
(load "config/packages/init-markdown")
(load "config/packages/init-org")
(load "config/packages/init-calfw")
(load "config/packages/init-session")
(load "config/packages/init-srspeedbar")
(load "config/packages/init-dmacro")
