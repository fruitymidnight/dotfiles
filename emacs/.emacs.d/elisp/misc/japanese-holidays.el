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

;; ���̃v���O�����́Acalender �ŕ\���o����l�ɓ��{�̏j����ݒ肵�܂��B
;; �g�p����ɂ́A���̃t�@�C���� load-path �̒ʂ������ɒu���A~/.emacs ��
;; �ȉ��̐ݒ��ǉ����܂��B

;;  (add-hook 'calendar-load-hook
;;            (lambda ()
;;              (require 'japanese-holidays)
;;              (setq calendar-holidays
;;                    (append japanese-holidays local-holidays other-holidays))))
;;  (setq mark-holidays-in-calendar t)

;; �g���傤�h���}�[�N����ɂ͈ȉ��̐ݒ��ǉ����܂��B
;;  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; ���j����Ԏ��ɂ���ꍇ�A�ȉ��̐ݒ��ǉ����܂��B
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
  '(;; ����6�N�������z����344��
    (holiday-range
     (holiday-fixed 1 3 "���n��") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 1 5 "�V�N����") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 1 30 "�F���V�c��") '(10 14 1873) '(9 3 1912))
    (holiday-range
     (holiday-fixed 2 11 "�I����") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 4 3 "�_���V�c��") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 9 17 "�_����") '(10 14 1873) '(7 5 1879))
    (holiday-range
     (holiday-fixed 11 3 "�V����") '(10 14 1873) '(9 3 1912))
    (holiday-range
     (holiday-fixed 11 23 "�V����") '(10 14 1873) '(7 20 1948))
    ;; ����11�N�������z��23��
    (let* ((equinox (solar-equinoxes/solstices 0 displayed-year))
	   (m (extract-calendar-month equinox))
	   (d (truncate (extract-calendar-day equinox))))
      (holiday-range
       (holiday-fixed m d "�t�G�c���") '(6 5 1878) '(7 20 1948)))
    (let* ((equinox (solar-equinoxes/solstices 2 displayed-year))
	   (m (extract-calendar-month equinox))
	   (d (truncate (extract-calendar-day equinox))))
      (holiday-range
       (holiday-fixed m d "�H�G�c���") '(6 5 1878) '(7 20 1948)))
    ;; ����12�N�������z��27��
    (holiday-range
     (holiday-fixed 10 17 "�_����") '(7 5 1879) '(7 20 1948))
    ;; �x���j�փX���� (�吳���N���ߑ�19��)
    (holiday-range
     (holiday-fixed 7 30 "�����V�c��") '(9 3 1912) '(3 3 1927))
    (holiday-range
     (holiday-fixed 8 31 "�V����") '(9 3 1912) '(3 3 1927))
    ;; �吳2�N����259��
    (holiday-range
     (holiday-fixed 10 31 "�V���ߏj��") '(10 31 1913) '(3 3 1927))
    ;; �x���j�փX���������m�� (���a2�N���ߑ�25��)
    (holiday-range
     (holiday-fixed 4 29 "�V����") '(3 3 1927) '(7 20 1948))
    (holiday-range
     (holiday-fixed 11 3 "������") '(3 3 1927) '(7 20 1948))
    (holiday-range
     (holiday-fixed 12 25 "�吳�V�c��") '(3 3 1927) '(7 20 1948))
    ;; �����̏j���Ɋւ���@���̈ꕔ����������@�� (���a60�N�@����103��)
    (holiday-national
     ;; �����̏j���Ɋւ���@���̈ꕔ����������@�� (���a48�N�@����10��)
     (holiday-substitute
      (nconc
       ;; �����̏j���Ɋւ���@�� (���a23�N�@����178��)
       (holiday-range
	(holiday-fixed 1 1 "����") '(7 20 1948))
       (holiday-range
	(holiday-fixed 1 15 "���l�̓�") '(7 20 1947) '(1 1 2000))
       (let* ((equinox (solar-equinoxes/solstices 0 displayed-year))
	      (m (extract-calendar-month equinox))
	      (d (truncate (extract-calendar-day equinox))))
	 ;; �t���̓��́A�����ɂ͑O�N2���̊���ɂ�茈�肳���
	 (holiday-range
	  (holiday-fixed m d "�t���̓�") '(7 20 1948)))
       (holiday-range
	(holiday-fixed 4 29 "�V�c�a����") '(7 20 1948) '(2 17 1989))
       (holiday-range
	(holiday-fixed 5 3 "���@�L�O��") '(7 20 1948))
       (holiday-range
	(holiday-fixed 5 5 "���ǂ��̓�") '(7 20 1948))
       (let* ((equinox (solar-equinoxes/solstices 2 displayed-year))
	      (m (extract-calendar-month equinox))
	      (d (truncate (extract-calendar-day equinox))))
	 ;; �H���̓��́A�����ɂ͑O�N2���̊���ɂ�茈�肳���
	 (holiday-range
	  (holiday-fixed m d "�H���̓�") '(7 20 1948)))
       (holiday-range
	(holiday-fixed 11 3 "�����̓�") '(7 20 1948))
       (holiday-range
	(holiday-fixed 11 23 "�ΘJ���ӂ̓�") '(7 20 1948))
       ;; �����̏j���Ɋւ���@���̈ꕔ����������@�� (���a41�N�@����86��)
       ;;   �����L�O�̓��ƂȂ�����߂鐭�� (���a41�N���ߑ�376��)
       (holiday-range
	(holiday-fixed 2 11 "�����L�O�̓�") '(6 25 1966))
       (holiday-range
	(holiday-fixed 9 15 "�h�V�̓�") '(6 25 1966) '(1 1 2003))
       (holiday-range
	(holiday-fixed 10 10 "�̈�̓�") '(6 25 1966) '(1 1 2000))
       ;; �����̏j���Ɋւ���@���̈ꕔ����������@�� (�������N�@����5��)
       (holiday-range
	(holiday-fixed 4 29 "�݂ǂ�̓�") '(2 17 1989) '(1 1 2007))
       (holiday-range
	(holiday-fixed 12 23 "�V�c�a����") '(2 17 1989))
       ;; �����̏j���Ɋւ���@���̈ꕔ����������@�� (����7�N�@����22��)
       (holiday-range
	(holiday-fixed 7 20 "�C�̓�") '(1 1 1996) '(1 1 2003))
       ;; �����̏j���Ɋւ���@���̈ꕔ����������@�� (����10�N�@����141��)
       (holiday-range
	(holiday-float 1 1 2 "���l�̓�") '(1 1 2000))
       (holiday-range
	(holiday-float 10 1 2 "�̈�̓�") '(1 1 2000))
       ;; �����̏j���Ɋւ���@���y�јV�l�����@�̈ꕔ����������@�� (����13�N�@����59��)
       (holiday-range
	(holiday-float 7 1 3 "�C�̓�") '(1 1 2003))
       (holiday-range
	(holiday-float 9 1 3 "�h�V�̓�") '(1 1 2003))
       ;; �����̏j���Ɋւ���@���̈ꕔ����������@�� (����17�N�@����43��)
       (holiday-range
	(holiday-fixed 4 29 "���a�̓�") '(1 1 2007))
       (holiday-range
	(holiday-fixed 5 4 "�݂ǂ�̓�") '(1 1 2007)))))
    (filter-visible-calendar-holidays
     '(;; �c���q���m�e���̌����̋V�̍s��������x���Ƃ���@�� (���a34�N�@����16��)
       ((4 10 1959) "���m�e���̌����̋V")
       ;; ���a�V�c�̑�r�̗�̍s��������x���Ƃ���@�� (�������N�@����4��)
       ((2 24 1989) "���a�V�c�̑�r�̗�")
       ;; ���ʗ琳�a�̋V�̍s��������x���Ƃ���@�� (����2�N�@����24��)
       ((11 12 1990) "���ʗ琳�a�̋V")
       ;; �c���q���m�e���̌����̋V�̍s��������x���Ƃ���@�� (����5�N�@����32��)
       ((6 9 1993) "���m�e���̌����̋V"))))
  "*Japanese holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)

(defcustom holiday-substitute-name "�U�֋x��"
  "*Name of substitute holiday."
  :type 'string
  :group 'holidays)

(defcustom holiday-national-name "�����̋x��"
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
