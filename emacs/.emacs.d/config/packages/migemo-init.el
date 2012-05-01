;====================================
; migemoの設定
;====================================
;;(setq migemo-command "~/.emacs.d/bin/cmigemo/cmigemo")
;;(setq migemo-command "cmigemo")
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))
;(setq migemo-dictionary (expand-file-name "~/.emacs.d/bin/cmigemo/dict/utf-8/migemo-dict"))
;;(setq migemo-dictionary "~/.emacs.d/bin/cmigemo/dict/utf-8/migemo-dict")
;;(setq migemo-dictionary "c:/home/DEMO_client/.emacs.d/bin/cmigemo/dict/utf-8/migemo-dict")

;; windows
;;(setq migemo-dictionary "C:\\home\\DEMO_client\\.emacs.d\\bin\\cmigemo\\dict\\utf-8\\migemo-dict")

;; mac
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1024)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)
