;====================================
; Tramp
;====================================
;; for Windows (putty に付属の plink を使用する)
;; (require 'tramp)
;; (setq tramp-default-method "plink")
;; (setq tramp-completion-without-shell-p t)
;; (setq tramp-shell-prompt-pattern "^[ $]+")
;; (setq tramp-debug-buffer t)
;; (nconc (cadr (assq 'tramp-login-args (assoc "ssh" tramp-methods))) '("/bin/sh" "-i"))
;; (setcdr (assq 'tramp-remote-sh (assoc "ssh" tramp-methods)) '("/bin/sh -i"))
;; (modify-coding-system-alist 'process "plink" 'utf-8)
