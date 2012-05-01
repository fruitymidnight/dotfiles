;;; org-init.el

(when (require 'org-install nil t)
  (add-to-list 'auto-mode-alist '(".org$" . org-mode))
  ;; キー設定
  (add-hook 'org-mode-hook
            '(lambda ()
               (local-set-key "\C-cl" 'org-store-link)
               (local-set-key "\C-ca" 'org-agenda)
               (local-set-key "\C-cb" 'org-iswitchb)
               ))

  ;; 基本ディレクトリ
  (setq org-directory "~/org/gtd")
  (setq org-default-notes-file (concat org-directory "agenda.org"))

  ;; 打ち切らない.
  (setq org-startup-truncated nil)
  ;; link を return で追う
  (setq org-retrun-follows-link t)

  ;; DONE にしたときの日時を記録する
  (setq org-log-done t)
  (org-remember-insinuate)


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

  ;; 大文字で補完(github)
  ;;http://www.gfd-dennou.org/member/uwabami/cc-env/Emacs/init-org.html
  (add-to-list 'org-structure-template-alist
               '("s" "#+BEGIN_SRC ?\n\n#+END_SRC"
                 "<src lang=\"?\">\n\n</src>"))


  ;; 公開設定

  ;; default の style sheet は使わない
  (setq org-export-html-style-include-default nil)

  ;; HTMLエクスポートでコードハイライト用 CSS を分離
  ;; CSS は M-x org-export-htmlize-generate-css で作成可能
  (setq org-export-htmlize-output-type 'css)

  ;; cache の置き場所を ~/.emacs.d/tmp/org-timestamps/ に変える
  (setq org-publish-timestamp-directory
        (convert-standard-filename (concat user-emacs-directory "var/org-timestamps/")))

  ;; project の設定
  (setq org-publish-project-alist
        '(("memo"
           :base-directory "~/org/memo/"
           :publishing-directory "~/org/public_html"
           :section-numbers nil
           :table-of-contents nil
           :language ja
           :author-info nil
           :email-info nil
           :creator-info nil
           :auto-sitemap t
           :sitemap-filename "sitemap.org"
           :sitemap-title "sitemap"
           :style "<link rel=\"stylesheet\" href=\"./css/common.css\" type=\"text/css\"/>
           <link rel=\"stylesheet\" href=\"./css/face.css\" type=\"text/css\"/>"
           )))


  )
