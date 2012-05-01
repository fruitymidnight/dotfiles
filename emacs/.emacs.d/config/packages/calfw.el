;;; calfw-init.el


(when (require 'calfw nil t) ; 初回一度だけ
  ;;(cfw:open-calendar-buffer) ; カレンダー表示
  (require 'calfw-org)
  ;; エラーがでるのでコメントアウト
  ;;(cfw:install-org-schedules) 

  ;; cfw-org 連携
  (define-key global-map [f9] 'cfw:open-org-calendar)

  ;; face 設定
  (custom-set-faces
   '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
   '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
   '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
   '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
   '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
   '(cfw:face-default-content ((t :foreground "#bfebbf")))
   '(cfw:face-regions ((t :foreground "#366060")))
   '(cfw:face-day-title ((t :background "grey10")))
   '(cfw:face-periods ((t :foreground "#8cd0d3")))
   '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
   '(cfw:face-today ((t :background: "grey10" :weight bold)))
   '(cfw:face-select ((t :background "#2f2f2f"))))
)

;; ----------------------------------------------------------------------
;; @ 日本の祝日
;;
;;   http://d.hatena.ne.jp/kiwanami/20110723/1311434175

(add-hook 'calendar-load-hook
          (lambda ()
            ;; 祝日設定
            (when (require 'japanese-holidays nil t)
              (setq calendar-holidays
                    (append japanese-holidays local-holidays other-holidays))
              ;; (setq calendar-weekend-marker 'diary)
              ;; (add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
              ;; (add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)
              )))



