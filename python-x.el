;;; python-x.el --- python.el extras for interactive evaluation

;; Author: Yuri D'Elia <wavexx@thregr.org>
;; Version: 1.0
;; URL: https://github.com/wavexx/python-x.el
;; Package-Requires: ((python "0.24"))
;; Keywords: python eval folding

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; TODO

;;; Usage:

;; Add the following to your .emacs to load 'python-x on-demand:
;;
;; (eval-after-load 'python
;;   (lambda ()
;;     (require 'python-x)
;;     ;; Suggested keybindings (ESS-like)
;;     (define-key python-mode-map (kbd "C-c C-j") 'python-shell-send-line)
;;     (define-key python-mode-map (kbd "C-c C-n") 'python-shell-send-line-and-step)
;;     (define-key python-mode-map (kbd "C-c C-f") 'python-shell-send-defun)
;;     (define-key python-mode-map (kbd "C-c C-b") 'python-shell-send-buffer)
;;     (define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-dwim)))


;;; Code:
(require 'python)
(require 'folding)

;; Verbose line evaluation stepping

(defun python-nav-eol-eos ()
  "Move point to the next statement ending exactly at the end of the line"
  (let ((point (point)))
    (end-of-line)
    (python-nav-end-of-statement)
    (when (not (eq point (point)))
      (python-nav-eol-eos))))

;;;###autoload
(defun python-shell-send-line ()
  "Send the current line (with any remaining continuations) to the inferior Python process,
printing the result of the expression on the shell."
  (interactive)
  (let ((start (line-beginning-position)) end)
    (save-excursion
      (python-nav-eol-eos)
      (setq end (point)))
    (let* ((substring (buffer-substring-no-properties start end))
	   (line (replace-regexp-in-string "\\\\\n" " " substring)))
      (python-shell-send-string line))))

;;;###autoload
(defun python-shell-send-line-and-step ()
  "Send the current line (with any remaining continuations) to the inferior Python process,
printing the result of the expression on the shell, then move on to the next statement."
  (interactive)
  (let ((start (line-beginning-position)))
    (python-nav-eol-eos)
    (let* ((end (point))
	   (substring (buffer-substring-no-properties start end))
	   (line (replace-regexp-in-string "\\\\\n" " " substring)))
      (python-shell-send-string line)))
  (python-nav-forward-statement))


;; Delimited sections

;;;###autoload
(defcustom python-section-delimiter "# ---"
  "TODO"
  :type 'string
  :group 'python-x)

;;;###autoload
(defcustom python-section-highlight (if (require 'volatile-highlights nil t) t)
  "TODO"
  :type 'boolean
  :group 'python-x)


(defun python-section-in-skiplist (pos skip)
  (if (not skip) nil
      (let ((start (car skip))
	    (end (cadr skip)))
	(or (and (>= pos start) (<= pos end))
	    (python-section-in-skiplist pos (cddr skip))))))

(defun python-section-search-skiplist (fn skip)
  (loop for pos = (funcall fn python-section-delimiter nil t)
     while pos do
       (setq pos (match-beginning 0))
       (if (not (python-section-in-skiplist pos skip))
	   (return pos))))

(defun python-section-search (rev)
  (let ((ret (folding-skip-folds rev)))
    (let ((pos (or (car-safe ret)
		   (if rev (point-min) (point-max))))
	  (skip (cdr-safe ret)))
      (if (not python-section-delimiter) pos
	  (save-restriction
	    (narrow-to-region (point) pos)
	    (save-excursion
	      (or (python-section-search-skiplist
		   (if rev 'search-backward 'search-forward)
		   (if rev skip (nreverse skip)))
		  pos)))))))

;;;###autoload
(defun python-shell-send-fold-or-section ()
  "TODO"
  (interactive)
  (unless folding-regexp
    ;; define folding markers, even when folding-mode is not active
    (folding-set-local-variables))
  (let ((start (python-section-search t))
	(end (python-section-search nil)))
    (when python-section-highlight
      (let ((start start) (end end))
	(save-excursion
	  ;; align the region to visual lines for presentation purposes
	  (unless (eq (point-min) start)
	    (goto-char start)
	    (beginning-of-line 2)
	    (setq start (point)))
	  (unless (eq (point-max) end)
	    (goto-char end)
	    (beginning-of-line)
	    (setq end (point))))
	(when (or (> start (window-start))
		  (< end (window-end)))
	  (vhl/add-range start end))))
    (python-shell-send-region start end)))

;;;###autoload
(defun python-shell-send-dwim ()
  "TODO"
  (interactive)
  (if (use-region-p)
    (python-shell-send-region (region-beginning) (region-end))
    (python-shell-send-fold-or-section)))


;; Exception handling

(defcustom python-shell-show-exceptions t
  "TODO"
  :type 'boolean
  :group 'python-x)

(defun python-shell-exception-function (buffer)
  (when python-shell-show-exceptions
    (display-buffer buffer)))

(defun python-comint-find-exceptions (output)
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "\\bTraceback (most recent call last):\n  File "
			      comint-last-output-start t)
      (python-shell-exception-function (current-buffer)))))

(add-hook 'inferior-python-mode-hook
	  (lambda ()
	    (add-hook 'comint-output-filter-functions
		      'python-comint-find-exceptions)))

(provide 'python-x)
