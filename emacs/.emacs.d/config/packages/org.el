;;; org-init.el

(when (autoload-if-found 'org-install "org-mode" nil t)
  (eval-after-load "org-install"
    (setq org-startup-truncated nil)
    (setq org-return-follows-link t)

    ;; DONE にしたときの日時を記録する
    (setq org-log-done t)
    (add-to-list 'auto-mode-alist '(".org$" . org-mode))
    (org-remember-insinuate)
    (setq org-directory "~/Work/gitrepos/docs/memo/")
    (setq org-default-notes-file (concat org-directory "agenda.org"))

    ;; agenda 関連の設定
    ;; http://d.hatena.ne.jp/tamura70/20100208/org

    ;; アジェンダ表示の対象ファイル
    (setq org-agenda-files (list org-directory))
    ;; アジェンダ表示で下線を用いる
    (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
    (setq hl-line-face 'underline)
    ;; 標準の祝日を利用しない
    (setq calendar-holidays nil)

    ;; GTD 
    ;; http://hpcgi1.nifty.com/spen/index.cgi?OrgMode%2FOrg-mode%A4%C7GTD%BC%C2%C1%A9%A1%CA%CB%DD%CC%F5%A1%CB

    (setq org-agenda-custom-commands
          '(
            ("P" "Projects"   
             ((tags "PROJECT")))

            ("H" "Office and Home Lists"
             ((agenda)
              (tags-todo "OFFICE")
              (tags-todo "HOME")
              (tags-todo "COMPUTER")
              (tags-todo "DVD")
              (tags-todo "READING")))

            ("D" "Daily Action List"
             (
              (agenda "" ((org-agenda-ndays 1)
                          (org-agenda-sorting-strategy
                           (quote ((agenda time-up priority-down tag-up) )))
                          (org-deadline-warning-days 0)
                          ))))
            )
          )

    ;; remember-mode
    (define-key global-map [f8] 'org-remember)
    (setq remember-annotation-functions '(org-remember-annotation))
    (setq remember-handler-functions '(org-remember-handler))
    (add-hook 'remember-mode-hook 'org-remember-apply-template)
    (setq org-remember-templates
          '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "未整理")
            ("Note" ?n "** %? :NOTE:\n %t" nil "未整理")
            ("box" ?b "** %? \n" nil "メール")
            ("Meeting" ?m "** %? %t" nil "打合せ")
            ))

    ;; キー設定
    (add-hook 'org-mode-hook
              '(lambda ()
                 (local-set-key "\C-cl" 'org-store-link)
                 (local-set-key "\C-ca" 'org-agenda)
                 (local-set-key "\C-cb" 'org-iswitchb)
                 ))
    ))
