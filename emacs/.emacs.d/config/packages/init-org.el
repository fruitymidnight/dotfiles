;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; init-org.el --- Org-mode の設定ファイル

;;; Commentary:

;;; Tex 環境のインストール
;; Export に latex を利用する場合はTex環境のインストールが必要
;; - Mac OS の場合
;;   + MacTex を利用してインストール
;;      MacTex は TexLive の MacOSX 用インストールパッケージ。
;;      TexLive では Tex 環境だけでなく基本的なパッケージもインストールされるため
;;      標準的な利用では特に追加設定は不要
;;

;;; Tex の設定
;; 奥村先生の作成された jsarticle ドキュメントクラスを利用する
;; jsarticle ドキュメントクラスでは欧文のフォント設定を個別に変更しているが、
;; 違和感を感じたため、以下の記述を加えている
;;
;;   \renewcommand{\headfont}{\bfseries}
;;
;; @see http://oku.edu.mie-u.ac.jp/~okumura/jsclasses/
;;

;;; ソースコードの出力
;; 1. verbatim を利用した出力
;;    デフォルトは verbatim を利用して出力する
;;
;;    verbatim から Verbatim に置換して fancyvrb を利用する人もいたけど、オプションの設定ができないので設定はしていない
;;    @see http://qiita.com/kawabata@github/items/1b56ec8284942ff2646b
;;
;; 2. listing を利用した出力
;;    ox-latex では verbatim から listings を利用した出力に設定可能
;;    ただし、listings はコメントに日本語が含まれるとアルファベットと日本語の順序が変更される問題あり
;;    解決には jlisting の利用するが、TexLive には jlisting は含まれていないため、個別にインストールが必要
;;
;;    jlisting のインストールは下記ディレクトリに jlisting の sty ファイルを展開し、`sudo mktexlsr` を実行する
;;       TEXMFLOCAL/tex/latex/local/jlisting
;;       ※TEXMFLOCAL は MacTex を使用して TexLive2014 をインストールした場合は、
;;         /usr/local/texlive/texmf-local/ に設定されている
;;    @see http://mytexpert.sourceforge.jp/index.php?Listings
;;
;; 3. minted の利用
;;    ox-latex では listing の他にも minted を利用した出力にも設定可能
;;    ただし、euptex、uptex では @pdfshellescape に対応していないため、tex コンパイル時にエラーとなり使用できない
;;    TexLive2015 に含まれる euptex では利用できるようになる、かも？？
;; 


;;; Code:


(use-package org-install
  :mode (("\\.org$" . org-mode)
         ("\\.org.txt$"   . org-mode)
         )
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c m" . org-capture)
         )
  :config
  (progn
    ;; ディレクトリの設定
    (setq org-directory "~/org/gtd")
    (setq org-default-notes-file (concat org-directory "agenda.org"))
    )
  )

(use-package ox-latex
  :defer t
  :config
  (progn
    ;; pdf に変換するときのコマンドを定義
    (setq org-latex-pdf-process '("/usr/texbin/ptex2pdf -e -u -l -ot '-synctex=1' %f"))
    ;; latex に変換するときのデフォルト値
    (setq org-latex-default-class "jsarticle")
    (setq org-latex-classes '(("jsarticle"
                               "\\documentclass[a4j,9pt,uplatex]{jsarticle}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{url}
\\usepackage{atbegshi}
\\AtBeginShipoutFirst{\\special{pdf:tounicode EUC-UCS2}}
\\usepackage[dvipdfmx,setpagesize=false]{hyperref}
\\usepackage{ulem}
\\usepackage{listings,jlisting}
\\usepackage{color}
\\makeatletter
\\renewcommand\\verbatim@font{\\small\\ttfamily}
\\makeatother
\\renewcommand{\\headfont}{\\bfseries}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
                               ("\\section{%s}" . "\\section*{%s}")
                               ("\\subsection{%s}" . "\\subsection*{%s}")
                               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                               ("\\paragraph{%s}" . "\\paragraph*{%s}")
                               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                              ))
    ;; #+BEGIN_SRC に listing を利用する
    (setq org-latex-listings t)
    ;; listing のオプション設定
    (setq org-latex-listings-options
    '(
      ("basicstyle" "\\small\\ttfamily")
      ("keywordstyle" "\\small\\ttfamily")
      ("commentstyle" "\\small\\mdseries\\ttfamily")
      ("identifierstyle" "\\small\\ttfamily")
      ("stringstyle" "\\small\\ttfamily")
      ("frame" "single")                               ; 枠をつける
      ("showstringspaces" "false")                     ; 空白を [ で表示しない
      ("breaklines" "true")                            ; 途中の改行を許可する
      ("lineskip" "-0.5zw")                            ; 行間をデフォルトの半分にする
      ))
    )
  )
