;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; init-org.el --- 

;;; Commentary:

;; Org-mode の設定

;;; Code:


(use-package org
  :mode (("\\.org$" . org-mode)
         ("\\.org.txt$"   . org-mode)
         )
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c m" . org-capture)
         )
  :init
  (progn

    )
  :config
  (progn
    ;; ディレクトリの設定
    (setq org-directory "~/org/gtd")
    (setq org-default-notes-file (concat org-directory "agenda.org"))

    ;; pdf に変換するときのコマンドを定義
    (setq org-latex-pdf-process '("/usr/texbin/ptex2pdf -u -l -ot '-synctex=1' %f"))
    ;; latex に変換するときのデフォルト値
    (setq org-latex-default-class "jsarticle")
    (setq org-latex-classes '(("jsarticle"
                               "\\documentclass[uplatex]{jsarticle}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{url}
\\usepackage{atbegshi}
\\AtBeginShipoutFirst{\\special{pdf:tounicode EUC-UCS2}}
\\usepackage[dvipdfmx,setpagesize=false]{hyperref}
\\usepackage{ulem}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
                               ("\\section{%s}" . "\\section*{%s}")
                               ("\\subsection{%s}" . "\\subsection*{%s}")
                               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                               ("\\paragraph{%s}" . "\\paragraph*{%s}")
                               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                              ("jsbook"
                               "\\documentclass[uplatex]{jsbook}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{url}
\\usepackage{atbegshi}
\\AtBeginShipoutFirst{\\special{pdf:tounicode EUC-UCS2}}
\\usepackage[dvipdfmx,setpagesize=false]{hyperref}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
                               ("\\chapter{%s}" . "\\chapter*{%s}")
                               ("\\section{%s}" . "\\section*{%s}")
                               ("\\subsection{%s}" . "\\subsection*{%s}")
                               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                               ("\\paragraph{%s}" . "\\paragraph*{%s}")
                               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                              ))
    
    


  ))
