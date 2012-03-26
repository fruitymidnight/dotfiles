;====================================
; install-elisp (obsolute)
;====================================
;(require 'install-elisp)
; elisp をインストールするディレクトリの指定
;(setq install-elisp-repository-directory "~/.emacs.d/elisp")


;====================================
; auto-install
;====================================
;(add-to-list 'load-path "~/.emacs.d/elisp/auto-install/")
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
;(setq url-proxy-services '(("http" . "133.254.7.25:8080")))
;(setq url-proxy-services '(("https" . "133.254.7.25:8080")))
; (5) I recommend you add below to your ~/.emacs for install-elisp users:
;(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)


;; autoload を使用したかったけど失敗
;; mode-compile
;; (when (autoload 'auto-install "auto-install" nil t)
;;   (eval-after-load "auto-install"
;;     '(progn
;;        (setq auto-install-directory "~/.emacs.d/elisp/")
;;        (auto-install-compatibility-setup))))

