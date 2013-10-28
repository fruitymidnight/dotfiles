; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ----------------------------------------------------------------------
;; @ macro
;;
;;   環境別に場合分け
;;   emacs-version predicates
;;   http://d.hatena.ne.jp/tomoya/20090807/1249601308

(defun x->bool (elt) (not (not elt)))

(setq emacs22-p (string-match "^22" emacs-version)
      emacs23-p (string-match "^23" emacs-version)
      emacs23.0-p (string-match "^23\.0" emacs-version)
      emacs23.1-p (string-match "^23\.1" emacs-version)
      emacs23.2-p (string-match "^23\.2" emacs-version)
      emacs24-p (string-match "^24" emacs-version)
      emacs24.1-p (string-match "^24\.1" emacs-version)
      emacs24.2-p (string-match "^24\.2" emacs-version)
      emacs24.3-p (string-match "^24\.3" emacs-version)
      )

;; system-type predicates
(setq darwin-p  (eq system-type 'darwin)
      ns-p      (eq window-system 'ns)
      carbon-p  (eq window-system 'mac)
      linux-p   (eq system-type 'gnu/linux)
      colinux-p (when linux-p
                  (let ((file "/proc/modules"))
                    (and
                     (file-readable-p file)
                     (x->bool
                      (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (re-search-forward "^cofuse\.+" nil t))))))
      cygwin-p  (eq system-type 'cygwin)
      nt-p      (eq system-type 'windows-nt)
      meadow-p  (featurep 'meadow)
      windows-p (or cygwin-p nt-p meadow-p))

;; ----------------------------------------------------------------------
;; @ macro
;;  autoload
;;  http://d.hatena.ne.jp/tomoya/20090811/1250006208
(defun autoload-if-found (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))

;; ----------------------------------------------------------------------
;; @ macro
;;  lazyload
;;  http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html
(defmacro lazyload (func lib &rest body)
  `(when (locate-library ,lib)
     ,@(mapcar (lambda (f) `(autoload ',f ,lib nil t)) func)
     (eval-after-load ,lib
       '(progn
          ,@body))))


;; ----------------------------------------------------------------------
;; @ macro
;; 関数用コーディングシステム定義マクロ
(defmacro set-function-coding-system (function decoding encoding)
  `(defadvice ,function (around ,(intern (concat
                                          "ad-"
                                          (symbol-name function)
                                          "-coding-setup"))
                                activate)
     (let ((coding-system-for-read ,decoding)
           (coding-system-for-write ,encoding))
       ad-do-it)))
