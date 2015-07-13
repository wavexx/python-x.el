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

;; python-x extends the built-in `python-mode' (F. Gallina's) with several
;; additional functions and behaviors inspired by `ess-mode', which are
;; targeted to interactive code evaluation with an inferior Python process.
;;
;; python-x allows to evaluate code blocks using comments as delimiters (code
;; "sections") or using arbitrarily nested folding marks.
;;
;; python-x installs an handler to show uncaught exceptions produced by
;; interactive code evaluation by default. See `python-shell-show-exceptions'
;; to control this behavior.
;;
;; The following functions are introduced:
;; - `python-shell-send-line': evaluate and print the current line, accounting
;;   for statement and line continuations.
;; - `python-shell-send-line-and-step': evaluate and current line as above,
;;   then move the point to the next automatically.
;; - `python-shell-send-fold-or-section': evaluate the region defined by the
;;   current code fold or section.
;; - `python-shell-send-dwim': evaluate the active region when active,
;;   otherwise revert to the current fold or section.
;;
;; python-x uses `volatile-highlights' for highlighting. Installation through
;; "melpa" is recommended (you don't actually need to enable
;; `volatile-highlights-mode' itself). `folding-mode' is similarly required (on
;; Debian it can be installed through the "emacs-goodies-el" package).
;;
;; Read through the Usage section in the source file for usage instructions and
;; recommended keybindings.

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

(defun python-string-to-single-line (string)
  (replace-regexp-in-string "\\s *\\\\\n\\s *" " " string))

;;;###autoload
(defcustom python-multiline-highlight (if (require 'volatile-highlights nil t) t)
  "When evaluating a statement with `python-shell-send-line' or
`python-shell-send-line-and-step' which spans more than one line, highlight
temporarily the evaluated region using `vhl/default-face'. Requires
`volatile-highlights-mode' to be installed."
  :type 'boolean
  :group 'python-x)

(defun python-shell-send-line ()
  "Send the current line (with any remaining continuations) to the inferior Python process,
printing the result of the expression on the shell."
  (interactive)
  (let (start start-lno	end end-lno)
    (save-excursion
      (back-to-indentation)
      (setq start (point)
	    start-lno (line-number-at-pos)))
    (save-excursion
      (python-nav-eol-eos)
      (setq end (point)
	    end-lno (line-number-at-pos)))
    (when (and python-multiline-highlight
	       (/= start-lno end-lno))
      (python-vhl-full-lines start end 0 0))
    (let* ((substring (buffer-substring-no-properties start end))
	   (line (python-string-to-single-line substring)))
      (python-shell-send-string line))))

;;;###autoload
(defun python-shell-send-line-and-step ()
  "Send the current line (with any remaining continuations) to the inferior Python process,
printing the result of the expression on the shell, then move on to the next statement."
  (interactive)
  (let (start start-lno	end end-lno)
    (save-excursion
      (back-to-indentation)
      (setq start (point)
	    start-lno (line-number-at-pos)))
    (save-excursion
      (python-nav-eol-eos)
      (setq end (point)
	    end-lno (line-number-at-pos)))
    (python-nav-forward-statement)
    (when (and python-multiline-highlight
	       (/= start-lno end-lno))
      (python-vhl-full-lines start (point) 0 1))
    (let* ((substring (buffer-substring-no-properties start end))
	   (line (python-string-to-single-line substring)))
      (python-shell-send-string line))))


;; Delimited sections

;;;###autoload
(defcustom python-section-delimiter "# ---"
  "Define the comment which marks the boundaries of the current code section.
See `python-shell-send-fold-or-section'."
  :type 'string
  :group 'python-x)

;;;###autoload
(defcustom python-section-highlight (if (require 'volatile-highlights nil t) t)
  "When evaluating a code fold/section with `python-shell-send-fold-or-section'
spanning more than one line, highlight temporarily the evaluated region using
`vhl/default-face'. Requires `volatile-highlights-mode' to be installed."
  :type 'boolean
  :group 'python-x)

(defun python-vhl-full-lines (start end margin-top margin-bottom)
  "Set a volatile highlight on the entire lines defined by start/end"
  (save-excursion
    (unless (eq (point-min) start)
      (goto-char start)
      (beginning-of-line (+ 1 margin-top))
      (setq start (point)))
    (unless (eq (point-max) end)
      (goto-char end)
      (beginning-of-line)
      (forward-line (- 1 margin-bottom))
      (setq end (point))))
  (when (or (> start (window-start))
	    (< end (window-end)))
    (vhl/add-range start end)))

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
  "Send the section of code at point to the inferior Python process, up to the
current fold or buffer boundaries.

A code \"section\" is delimited in both directions, and in order, by:

- The nearest section delimiter (see `python-section-delimiter') contained
  within the current fold.
- The nearest fold delimiter (see `folding-mode-marks-alist').
- The buffer boundaries.

`folding-mode' doesn't need to be enabled, but the same marks are used to
define code boundaries. See `folding-add-to-marks-list' for customization.
Nested folds and sections are included: section delimiters contained within a
nested fold are ignored.

When the region to be evaluated is longer than a single line and less than a
screenful, the region is temporarily highlighted according to
`python-section-highlight'."
  (interactive)
  (unless folding-regexp
    ;; define folding markers, even when folding-mode is not active
    (folding-set-local-variables))
  (let ((start (python-section-search t))
	(end (python-section-search nil)))
    (when python-section-highlight
      (python-vhl-full-lines start end 1 1))
    (python-shell-send-region start end)))

;;;###autoload
(defun python-shell-send-dwim ()
  "Send the current region to the inferior Python process, if active.
Otherwise, use `python-shell-send-current-fold-or-section'"
  (interactive)
  (if (use-region-p)
    (python-shell-send-region (region-beginning) (region-end))
    (python-shell-send-fold-or-section)))


;; Exception handling

(defcustom python-shell-show-exceptions t
  "Display uncaught exceptions of the inferior Python process using
`python-shell-show-exceptions'."
  :type 'boolean
  :group 'python-x)

(defun python-shell-show-exception-function (buffer)
  "Function invoked when the inferion Python process emits an uncaught
exception. Calls `display-buffer' according to `python-shell-show-exceptions'."
  (when python-shell-show-exceptions
    (display-buffer buffer)))

(defun python-comint-find-exceptions (output)
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "\\bTraceback (most recent call last):\n  File "
			      comint-last-output-start t)
      (python-shell-show-exception-function (current-buffer)))))

(add-hook 'inferior-python-mode-hook
	  (lambda ()
	    (add-hook 'comint-output-filter-functions
		      'python-comint-find-exceptions)))

(provide 'python-x)

;;; python-x.el ends here
