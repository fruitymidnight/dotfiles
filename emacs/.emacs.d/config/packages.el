;; el-get.el の設定
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)


; インストールしたelispの設定
;(load "config/packages/init-autoinstall")
(load "config/packages/init-anything")
(load "config/packages/init-autocomplete")
(load "config/packages/init-elscreen")
(load "config/packages/magit")
;(load "config/packages/init-tramp")
;(load "config/packages/init-ruby")
;(load "config/packages/init-vc")
;(load "config/packages/init-c")
;(load "config/packages/init-lua")
;(load "config/packages/init-org")
;(load "config/packages/init-misc")
;(load "config/packages/init-migemo")
;(load "config/packages/init-calfw")
