;====================================
;; auto-complete
;====================================

; del: auto-complete の設定の変更（2010/08/30）
;'(when (require 'auto-complete nil t)
;   (global-auto-complete-mode t))
(add-to-list 'load-path "~/.emacs.d/elisp/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/dict")
(ac-config-default)

; 補完推測機能のデータを永続化するファイル名を指定
(setq ac-comphist-file "~/.emacs.d/var/auto-complete/ac-comphist.dat")
