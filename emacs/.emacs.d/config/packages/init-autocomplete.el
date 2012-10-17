;;; init-autocomplete.el
(unless (locate-library "auto-complete")
  (el-get 'sync 'auto-complete))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
(ac-config-default)

;; 補完推測機能のデータを永続化するファイル名を指定
(setq ac-comphist-file "~/.emacs.d/var/auto-complete/ac-comphist.dat")
