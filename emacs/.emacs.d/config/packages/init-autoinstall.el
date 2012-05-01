;;; auto-install-init.el 

(when (autoload-if-found 'auto-install "auto-install" nil t)
  ;; autoload は成功した場合のみ non-nil を返すので、
  ;; when の条件部に置くことで、依存関係にある設定項目を自然に表現できます。
  (eval-after-load "auto-install"
    '(progn
       ;;(setq url-proxy-services '(("http" . "133.254.7.25:8080")))
       ;;(setq url-proxy-services '(("https" . "133.254.7.25:8080")))
       ;;(auto-install-update-emacswiki-package-name t)
       (auto-install-compatibility-setup)
       )))

