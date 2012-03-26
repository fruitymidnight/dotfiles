;;; japanese-holidays.el --- calendar functions for the Japanese calendar

;; Copyright (C) 1999 Takashi Hattori <hattori@sfc.keio.ac.jp>
;; Copyright (C) 2005 Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>

;; Author: Takashi Hattori <hattori@sfc.keio.ac.jp>
;;	Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Original program created by T. Hattori 1999/4/20

;; ‚±‚ÌƒvƒƒOƒ‰ƒ€‚ÍAcalender ‚Å•\¦o—ˆ‚é—l‚É“ú–{‚Ìj“ú‚ğİ’è‚µ‚Ü‚·B
;; g—p‚·‚é‚É‚ÍA‚±‚Ìƒtƒ@ƒCƒ‹‚ğ load-path ‚Ì’Ê‚Á‚½Š‚É’u‚«A~/.emacs ‚É
;; ˆÈ‰º‚Ìİ’è‚ğ’Ç‰Á‚µ‚Ü‚·B

;;  (add-hook 'calendar-load-hook
;;            (lambda ()
;;              (require 'japanese-holidays)
;;              (setq calendar-holidays
;;                    (append japanese-holidays local-holidays other-holidays))))
;;  (setq mark-holidays-in-calendar t)

;; g‚«‚å‚¤h‚ğƒ}[ƒN‚·‚é‚É‚ÍˆÈ‰º‚Ìİ’è‚ğ’Ç‰Á‚µ‚Ü‚·B
;;  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; “ú—j“ú‚ğÔš‚É‚·‚éê‡AˆÈ‰º‚Ìİ’è‚ğ’Ç‰Á‚µ‚Ü‚·B
;;  (setq calendar-weekend-marker 'diary)
;;  (add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
;;  (add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)

;;; Code:
;;

(eval-when-compile
  (require 'cl)
  (defvar displayed-month)
  (defvar displayed-year)
  (when noninteractive
    (require 'holidays)))

(autoload 'solar-equinoxes/solstices "solar")

(defcustom japanese-holidays
  '(;; –¾¡6”N‘¾­Š¯•z‘æ344†
    (holiday-range
     (holiday-fixed 1 3 "Œ³nÕ") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 1 5 "V”N‰ƒ‰ï") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 1 30 "F–¾“VcÕ") '(10 14 1873) '(9 3 1912))
    (holiday-range
     (holiday-fixed 2 11 "‹IŒ³ß") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 4 3 "_•“VcÕ") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 9 17 "_¦Õ") '(10 14 1873) '(7 5 1879))
    (holiday-range
     (holiday-fixed 11 3 "“V’·ß") '(10 14 1873) '(9 3 1912))
    (holiday-range
     (holiday-fixed 11 23 "V¦Õ") '(10 14 1873) '(7 20 1948))
    ;; –¾¡11”N‘¾­Š¯•z23†
    (let* ((equinox (solar-equinoxes/solstices 0 displayed-year))
	   (m (extract-calendar-month equinox))
	   (d (truncate (extract-calendar-day equinox))))
      (holiday-range
       (holiday-fixed m d "t‹Gc—ìÕ") '(6 5 1878) '(7 20 1948)))
    (let* ((equinox (solar-equinoxes/solstices 2 displayed-year))
	   (m (extract-calendar-month equinox))
	   (d (truncate (extract-calendar-day equinox))))
      (holiday-range
       (holiday-fixed m d "H‹Gc—ìÕ") '(6 5 1878) '(7 20 1948)))
    ;; –¾¡12”N‘¾­Š¯•z27†
    (holiday-range
     (holiday-fixed 10 17 "_¦Õ") '(7 5 1879) '(7 20 1948))
    ;; ‹x“úƒjŠÖƒXƒ‹Œ (‘å³Œ³”N’º—ß‘æ19†)
    (holiday-range
     (holiday-fixed 7 30 "–¾¡“VcÕ") '(9 3 1912) '(3 3 1927))
    (holiday-range
     (holiday-fixed 8 31 "“V’·ß") '(9 3 1912) '(3 3 1927))
    ;; ‘å³2”N’º—ß259†
    (holiday-range
     (holiday-fixed 10 31 "“V’·ßj“ú") '(10 31 1913) '(3 3 1927))
    ;; ‹x“úƒjŠÖƒXƒ‹Œ‰ü³ƒmŒ (º˜a2”N’º—ß‘æ25†)
    (holiday-range
     (holiday-fixed 4 29 "“V’·ß") '(3 3 1927) '(7 20 1948))
    (holiday-range
     (holiday-fixed 11 3 "–¾¡ß") '(3 3 1927) '(7 20 1948))
    (holiday-range
     (holiday-fixed 12 25 "‘å³“VcÕ") '(3 3 1927) '(7 20 1948))
    ;; ‘–¯‚Ìj“ú‚ÉŠÖ‚·‚é–@—¥‚Ìˆê•”‚ğ‰ü³‚·‚é–@—¥ (º˜a60”N–@—¥‘æ103†)
    (holiday-national
     ;; ‘–¯‚Ìj“ú‚ÉŠÖ‚·‚é–@—¥‚Ìˆê•”‚ğ‰ü³‚·‚é–@—¥ (º˜a48”N–@—¥‘æ10†)
     (holiday-substitute
      (nconc
       ;; ‘–¯‚Ìj“ú‚ÉŠÖ‚·‚é–@—¥ (º˜a23”N–@—¥‘æ178†)
       (holiday-range
	(holiday-fixed 1 1 "Œ³“ú") '(7 20 1948))
       (holiday-range
	(holiday-fixed 1 15 "¬l‚Ì“ú") '(7 20 1947) '(1 1 2000))
       (let* ((equinox (solar-equinoxes/solstices 0 displayed-year))
	      (m (extract-calendar-month equinox))
	      (d (truncate (extract-calendar-day equinox))))
	 ;; t•ª‚Ì“ú‚ÍAŒµ–§‚É‚Í‘O”N2Œ‚ÌŠ¯•ñ‚É‚æ‚èŒˆ’è‚³‚ê‚é
	 (holiday-range
	  (holiday-fixed m d "t•ª‚Ì“ú") '(7 20 1948)))
       (holiday-range
	(holiday-fixed 4 29 "“Vc’a¶“ú") '(7 20 1948) '(2 17 1989))
       (holiday-range
	(holiday-fixed 5 3 "Œ›–@‹L”O“ú") '(7 20 1948))
       (holiday-range
	(holiday-fixed 5 5 "‚±‚Ç‚à‚Ì“ú") '(7 20 1948))
       (let* ((equinox (solar-equinoxes/solstices 2 displayed-year))
	      (m (extract-calendar-month equinox))
	      (d (truncate (extract-calendar-day equinox))))
	 ;; H•ª‚Ì“ú‚ÍAŒµ–§‚É‚Í‘O”N2Œ‚ÌŠ¯•ñ‚É‚æ‚èŒˆ’è‚³‚ê‚é
	 (holiday-range
	  (holiday-fixed m d "H•ª‚Ì“ú") '(7 20 1948)))
       (holiday-range
	(holiday-fixed 11 3 "•¶‰»‚Ì“ú") '(7 20 1948))
       (holiday-range
	(holiday-fixed 11 23 "‹Î˜JŠ´Ó‚Ì“ú") '(7 20 1948))
       ;; ‘–¯‚Ìj“ú‚ÉŠÖ‚·‚é–@—¥‚Ìˆê•”‚ğ‰ü³‚·‚é–@—¥ (º˜a41”N–@—¥‘æ86†)
       ;;   Œš‘‹L”O‚Ì“ú‚Æ‚È‚é“ú‚ğ’è‚ß‚é­—ß (º˜a41”N­—ß‘æ376†)
       (holiday-range
	(holiday-fixed 2 11 "Œš‘‹L”O‚Ì“ú") '(6 25 1966))
       (holiday-range
	(holiday-fixed 9 15 "Œh˜V‚Ì“ú") '(6 25 1966) '(1 1 2003))
       (holiday-range
	(holiday-fixed 10 10 "‘Ìˆç‚Ì“ú") '(6 25 1966) '(1 1 2000))
       ;; ‘–¯‚Ìj“ú‚ÉŠÖ‚·‚é–@—¥‚Ìˆê•”‚ğ‰ü³‚·‚é–@—¥ (•½¬Œ³”N–@—¥‘æ5†)
       (holiday-range
	(holiday-fixed 4 29 "‚İ‚Ç‚è‚Ì“ú") '(2 17 1989) '(1 1 2007))
       (holiday-range
	(holiday-fixed 12 23 "“Vc’a¶“ú") '(2 17 1989))
       ;; ‘–¯‚Ìj“ú‚ÉŠÖ‚·‚é–@—¥‚Ìˆê•”‚ğ‰ü³‚·‚é–@—¥ (•½¬7”N–@—¥‘æ22†)
       (holiday-range
	(holiday-fixed 7 20 "ŠC‚Ì“ú") '(1 1 1996) '(1 1 2003))
       ;; ‘–¯‚Ìj“ú‚ÉŠÖ‚·‚é–@—¥‚Ìˆê•”‚ğ‰ü³‚·‚é–@—¥ (•½¬10”N–@—¥‘æ141†)
       (holiday-range
	(holiday-float 1 1 2 "¬l‚Ì“ú") '(1 1 2000))
       (holiday-range
	(holiday-float 10 1 2 "‘Ìˆç‚Ì“ú") '(1 1 2000))
       ;; ‘–¯‚Ìj“ú‚ÉŠÖ‚·‚é–@—¥‹y‚Ñ˜Vl•Ÿƒ–@‚Ìˆê•”‚ğ‰ü³‚·‚é–@—¥ (•½¬13”N–@—¥‘æ59†)
       (holiday-range
	(holiday-float 7 1 3 "ŠC‚Ì“ú") '(1 1 2003))
       (holiday-range
	(holiday-float 9 1 3 "Œh˜V‚Ì“ú") '(1 1 2003))
       ;; ‘–¯‚Ìj“ú‚ÉŠÖ‚·‚é–@—¥‚Ìˆê•”‚ğ‰ü³‚·‚é–@—¥ (•½¬17”N–@—¥‘æ43†)
       (holiday-range
	(holiday-fixed 4 29 "º˜a‚Ì“ú") '(1 1 2007))
       (holiday-range
	(holiday-fixed 5 4 "‚İ‚Ç‚è‚Ì“ú") '(1 1 2007)))))
    (filter-visible-calendar-holidays
     '(;; c‘¾q–¾me‰¤‚ÌŒ‹¥‚Ì‹V‚Ìs‚í‚ê‚é“ú‚ğ‹x“ú‚Æ‚·‚é–@—¥ (º˜a34”N–@—¥‘æ16†)
       ((4 10 1959) "–¾me‰¤‚ÌŒ‹¥‚Ì‹V")
       ;; º˜a“Vc‚Ì‘å‘r‚Ì—ç‚Ìs‚í‚ê‚é“ú‚ğ‹x“ú‚Æ‚·‚é–@—¥ (•½¬Œ³”N–@—¥‘æ4†)
       ((2 24 1989) "º˜a“Vc‚Ì‘å‘r‚Ì—ç")
       ;; ‘¦ˆÊ—ç³“a‚Ì‹V‚Ìs‚í‚ê‚é“ú‚ğ‹x“ú‚Æ‚·‚é–@—¥ (•½¬2”N–@—¥‘æ24†)
       ((11 12 1990) "‘¦ˆÊ—ç³“a‚Ì‹V")
       ;; c‘¾q“¿me‰¤‚ÌŒ‹¥‚Ì‹V‚Ìs‚í‚ê‚é“ú‚ğ‹x“ú‚Æ‚·‚é–@—¥ (•½¬5”N–@—¥‘æ32†)
       ((6 9 1993) "“¿me‰¤‚ÌŒ‹¥‚Ì‹V"))))
  "*Japanese holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)

(defcustom holiday-substitute-name "U‘Ö‹x“ú"
  "*Name of substitute holiday."
  :type 'string
  :group 'holidays)

(defcustom holiday-national-name "‘–¯‚Ì‹x“ú"
  "*Name of national holiday."
  :type 'string
  :group 'holidays)

(eval-and-compile
  (defun holiday-make-sortable (date)
    (+ (* (nth 2 date) 10000) (* (nth 0 date) 100) (nth 1 date))))

(defun holiday-range (holidays &optional from to)
  (let ((from (and from (holiday-make-sortable from)))
	(to   (and to   (holiday-make-sortable to))))
    (delq nil
	  (mapcar
	   (lambda (holiday)
	     (let ((date (holiday-make-sortable (car holiday))))
	       (when (and (or (null from) (<= from date))
			  (or (null to) (< date to)))
		 holiday)))
	   holidays))))

(defun holiday-find-date (date holidays)
  (let ((sortable-date (holiday-make-sortable date))
	matches)
    (dolist (holiday holidays)
      (when (= sortable-date (holiday-make-sortable (car holiday)))
	(setq matches (cons holiday matches))))
    matches))

(defun holiday-add-days (date days)
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian date) days)))

(defun holiday-subtract-date (from other)
  (- (calendar-absolute-from-gregorian from)
     (calendar-absolute-from-gregorian other)))

(defun holiday-substitute (holidays)
  (let (substitutes substitute)
    (dolist (holiday holidays)
      (let ((date (car holiday)))
	(when (and (>= (holiday-make-sortable date)
		       (eval-when-compile
			 (holiday-make-sortable '(4 12 1973))))
		   (= (calendar-day-of-week date) 0))
	  (setq substitutes
		(cons
		 (list (holiday-add-days date 1)
		       (format "%s (%s)"
			       holiday-substitute-name
			       (cadr holiday)))
		 substitutes)))))
    (when (setq substitutes
		(filter-visible-calendar-holidays substitutes))
      (setq substitutes (sort substitutes
			      (lambda (l r)
				(< (holiday-make-sortable (car l))
				   (holiday-make-sortable (car r))))))
      (while (setq substitute (car substitutes))
	(setq substitutes (cdr substitutes))
	(if (holiday-find-date (car substitute) holidays)
	    (let* ((date (car substitute))
		   (sortable-date (holiday-make-sortable date)))
	      (when (>= sortable-date
			(eval-when-compile
			  (holiday-make-sortable '(1 1 2007))))
		(setq substitutes
		      (cons
		       (list (holiday-add-days date 1) (cadr substitute))
		       substitutes))))
	  (setq holidays (cons substitute holidays)))))
    (filter-visible-calendar-holidays holidays)))

(defun holiday-national (holidays)
  (when holidays
    (setq holidays (sort holidays
			 (lambda (l r)
			   (< (holiday-make-sortable (car l))
			      (holiday-make-sortable (car r))))))
    (let* ((rest holidays)
	   (curr (pop rest))
	   prev nationals)
      (while (setq prev curr
		   curr (pop rest))
	(when (= (holiday-subtract-date (car curr) (car prev)) 2)
	  (let* ((date (holiday-add-days (car prev) 1))
		 (sotable-date (holiday-make-sortable date)))
	    (when (cond
		   ((>= sotable-date
			(eval-when-compile
			  (holiday-make-sortable '(1 1 2007))))
		    (catch 'found
		      (dolist (holiday (holiday-find-date date holidays))
			(unless (string-match
				 (regexp-quote holiday-substitute-name)
				 (cadr holiday))
			  (throw 'found nil)))
		      t))
		   ((>= sotable-date
			(eval-when-compile
			  (holiday-make-sortable '(12 27 1985))))
		    (not (or (= (calendar-day-of-week date) 0)
			     (holiday-find-date date holidays)))))
	      (setq nationals (cons (list date holiday-national-name)
				    nationals))))))
      (setq holidays (nconc holidays
			    (filter-visible-calendar-holidays nationals)))))
  holidays)

(defvar calendar-weekend '(0)
  "*List of days of week to be marked as holiday.")

(defvar calendar-weekend-marker nil)

(defun calendar-mark-weekend ()
  (let ((m displayed-month)
	(y displayed-year))
    (increment-calendar-month m y -1)
    (calendar-for-loop
     i from 1 to 3 do
     (let ((sunday (- 1 (calendar-day-of-week (list m 1 y))))
	   (last (calendar-last-day-of-month m y)))
       (while (<= sunday last)
	 (mapcar (lambda (x)
		   (let ((d (+ sunday x)))
		     (and (<= 1 d)
			  (<= d last)
			  (mark-visible-calendar-date
			   (list m d y)
			   calendar-weekend-marker))))
		 calendar-weekend)
	 (setq sunday (+ sunday 7))))
     (increment-calendar-month m y 1))))


(provide 'japanese-holidays)

;;; japanese-holidays.el ends here
