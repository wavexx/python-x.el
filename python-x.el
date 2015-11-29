;;; python-x.el --- python.el extras for interactive evaluation -*- lexical-binding: t -*-

;; Author: Yuri D'Elia <wavexx@thregr.org>
;; Version: 1.0
;; URL: https://github.com/wavexx/python-x.el
;; Package-Requires: ((python "0.24") (folding "0") (cl-lib "0.5"))
;; Keywords: python, eval, folding

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
;; additional functions and behaviors inspired by `ess-mode' which are targeted
;; to interactive code evaluation with an inferior Python process.
;;
;; python-x allows to evaluate code blocks using comments as delimiters (code
;; "sections") or using arbitrarily nested folding marks. By default, a code
;; section is delimited by comments starting with "# ---"; while folds are
;; defined by "# {{{" and "# }}}" (see `python-shell-send-fold-or-section').
;;
;; python-x installs an handler to show uncaught exceptions produced by
;; interactive code evaluation by default. See `python-shell-show-exceptions'
;; to control this behavior.
;;
;; The following functions are introduced:
;; - `python-shell-send-line': evaluate and print the current line, accounting
;;   for statement and line continuations.
;; - `python-shell-send-paragraph': evaluate the current paragraph.
;; - `python-shell-send-region-or-paragraph': evaluate the current region when
;;   active, otherwise evaluate the current paragraph.
;; - `python-shell-send-fold-or-section': evaluate the region defined by the
;;   current code fold or section.
;; - `python-shell-send-dwim': evaluate the region when active,
;;   otherwise revert to the current fold or section.
;; - `python-shell-print-region-or-symbol': evaluate and print the current
;;   region or symbol at point, displaying the inferior process output.
;; - `python-shell-display-shell': Display the inferior Python process output
;;   in another window.
;; - `python-forward-fold-or-section': Move forward by fold/sections.
;; - `python-backward-fold-or-section': Move backward by fold/sections.
;; - `python-mark-fold-or-section': Mark current fold or section.
;; - `python-eldoc-for-region-or-symbol': Summary help for the active
;;   region or symbol at point.
;; - `python-help-for-region-or-symbol': Display full help for the active
;;   region or symbol at point.
;;
;; All "python-shell-send-*" functions are also provided in a "*-and-step"
;; variant that moves the point after evaluation.
;;
;; python-x uses `volatile-highlights', when available, for highlighting
;; multi-line blocks. Installation through "melpa" is recommended (you don't
;; actually need to enable `volatile-highlights-mode' itself). python-x also
;; uses `folding' to interpret and define folding marks. Again, `folding-mode'
;; needs to be enabled manually if code folding is also desired.
;; `expand-region' is equally supported, when previously loaded.
;;
;; To automatically setup python-x with an ESS-like keyboard map, use
;; `python-x-setup' in your emacs startup:
;;
;; (python-x-setup)
;;
;; The keyboard map definition is currently tuned to the author's taste, and
;; may change over time. You are encouraged to look at the definition of
;; `python-x-setup' and derive your own.


;;; Code:
(require 'python)
(require 'folding)
(require 'cl-lib)

;; Optional
(require 'volatile-highlights nil t)
(eval-when-compile
  (require 'expand-region nil t))

;; Patch some buggy definitions in python.el, for which we need internal symbols
(when (version< emacs-version "25.1")
  (eval-and-compile
    (defconst python-rx-constituents
      `((block-start          . ,(rx symbol-start
				     (or "def" "class" "if" "elif" "else" "try"
					 "except" "finally" "for" "while" "with")
				     symbol-end))
	(dedenter            . ,(rx symbol-start
				    (or "elif" "else" "except" "finally")
				    symbol-end))
	(block-ender         . ,(rx symbol-start
				    (or
				     "break" "continue" "pass" "raise" "return")
				    symbol-end))
	(decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
				     (* (any word ?_))))
	(defun                . ,(rx symbol-start (or "def" "class") symbol-end))
	(if-name-main         . ,(rx line-start "if" (+ space) "__name__"
				     (+ space) "==" (+ space)
				     (any ?' ?\") "__main__" (any ?' ?\")
				     (* space) ?:))
	(symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
	(open-paren           . ,(rx (or "{" "[" "(")))
	(close-paren          . ,(rx (or "}" "]" ")")))
	(simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
	;; FIXME: rx should support (not simple-operator).
	(not-simple-operator  . ,(rx
				  (not
				   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
	;; FIXME: Use regexp-opt.
	(operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
					 "=" "%" "**" "//" "<<" ">>" "<=" "!="
					 "==" ">=" "is" "not")))
	;; FIXME: Use regexp-opt.
	(assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
					 ">>=" "<<=" "&=" "^=" "|=")))
	(string-delimiter . ,(rx (and
				  ;; Match even number of backslashes.
				  (or (not (any ?\\ ?\' ?\")) point
				      ;; Quotes might be preceded by a escaped quote.
				      (and (or (not (any ?\\)) point) ?\\
					   (* ?\\ ?\\) (any ?\' ?\")))
				  (* ?\\ ?\\)
				  ;; Match single or triple quotes of any kind.
				  (group (or  "\"" "\"\"\"" "'" "'''")))))
	(coding-cookie . ,(rx line-start ?# (* space)
			      (or
			       ;; # coding=<encoding name>
			       (: "coding" (or ?: ?=) (* space) (group-n 1 (+ (or word ?-))))
			       ;; # -*- coding: <encoding name> -*-
			       (: "-*-" (* space) "coding:" (* space)
				  (group-n 1 (+ (or word ?-))) (* space) "-*-")
			       ;; # vim: set fileencoding=<encoding name> :
			       (: "vim:" (* space) "set" (+ space)
				  "fileencoding" (* space) ?= (* space)
				  (group-n 1 (+ (or word ?-))) (* space) ":")))))
      "Additional Python specific sexps for `python-rx'")

    (defmacro python-rx (&rest regexps)
      "Python mode specialized rx macro.
This variant of `rx' supports common Python named REGEXPS."
      (let ((rx-constituents (append python-rx-constituents rx-constituents)))
	(cond ((null regexps)
	       (error "No regexp"))
	      ((cdr regexps)
	       (rx-to-string `(and ,@regexps) t))
	      (t
	       (rx-to-string (car regexps) t))))))

  ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21086
  (defun python-shell-buffer-substring (start end &optional nomain)
    "Send buffer substring from START to END formatted for shell.
This is a wrapper over `buffer-substring' that takes care of
different transformations for the code sent to be evaluated in
the python shell:
  1. When optional argument NOMAIN is non-nil everything under an
     \"if __name__ == '__main__'\" block will be removed.
  2. When a subregion of the buffer is sent, it takes care of
     appending extra empty lines so tracebacks are correct.
  3. When the region sent is a substring of the current buffer, a
     coding cookie is added.
  4. Wraps indented regions under an \"if True:\" block so the
     interpreter evaluates them correctly."
    (let* ((start (save-excursion
		    ;; Normalize start to the line beginning position.
		    (goto-char start)
		    (line-beginning-position)))
	   (substring (buffer-substring-no-properties start end))
	   (starts-at-point-min-p (save-restriction
				    (widen)
				    (= (point-min) start)))
	   (encoding (python-info-encoding))
	   (toplevel-p (zerop (save-excursion
				(goto-char start)
				(python-util-forward-comment 1)
				(current-indentation))))
	   (fillstr (when (not starts-at-point-min-p)
		      (concat
		       (format "# -*- coding: %s -*-\n" encoding)
		       (make-string
			;; Subtract 2 because of the coding cookie.
			(- (line-number-at-pos start) 2) ?\n)))))
      (with-temp-buffer
	(python-mode)
	(when fillstr
	  (insert fillstr))
	(insert substring)
	(goto-char (point-min))
	(when (not toplevel-p)
	  (insert "if True:")
	  (delete-region (point) (line-end-position)))
	(when nomain
	  (let* ((if-name-main-start-end
		  (and nomain
		       (save-excursion
			 (when (python-nav-if-name-main)
			   (cons (point)
				 (progn (python-nav-forward-sexp-safe)
					;; Include ending newline
					(forward-line 1)
					(point)))))))
		 ;; Oh destructuring bind, how I miss you.
		 (if-name-main-start (car if-name-main-start-end))
		 (if-name-main-end (cdr if-name-main-start-end))
		 (fillstr (make-string
			   (- (line-number-at-pos if-name-main-end)
			      (line-number-at-pos if-name-main-start)) ?\n)))
	    (when if-name-main-start-end
	      (goto-char if-name-main-start)
	      (delete-region if-name-main-start if-name-main-end)
	      (insert fillstr))))
	;; Ensure there's only one coding cookie in the generated string.
	(goto-char (point-min))
	(when (looking-at-p (python-rx coding-cookie))
	  (forward-line 1)
	  (when (looking-at-p (python-rx coding-cookie))
	    (delete-region
	     (line-beginning-position) (line-end-position))))
	(buffer-substring-no-properties (point-min) (point-max))))))


;; Verbose line evaluation/stepping

(defun python-string-to-statement (string)
  "Tweak the Python code string so that it can be evaluated as a single-line
statement for display purposes"
  (replace-regexp-in-string "\\s *\\\\\n\\s *" " " string))

;;;###autoload
(defcustom python-multiline-highlight (featurep 'volatile-highlights)
  "When evaluating a statement which spans more than one line and less than a
screenful, highlight temporarily the evaluated region using `vhl/default-face'.
Requires `volatile-highlights' to be installed."
  :type 'boolean
  :group 'python-x)

(defun python--vhl-full-lines (start end margin-top margin-bottom)
  "Set a volatile highlight on the entire lines defined by start/end. The
highlight is not set if spanning a single line or the entire visible region."
  (save-excursion
    (goto-char start)
    (unless (eq (point-min) start)
      (beginning-of-line (+ 1 margin-top)))
    (setq start (point))
    (goto-char end)
    (unless (eq (point-max) end)
      (beginning-of-line)
      (forward-line (- 1 margin-bottom)))
    (setq end (point)))
  (when (and (> (count-lines start end) 1)
	     (or (> start (window-start))
		 (< end (window-end))))
    (vhl/add-range start end)))

(defun python-shell--send-block-with-motion (move-start move-end step as-region)
  (let (start end)
    (save-excursion
      (funcall move-start)
      (setq start (point)))
    (save-excursion
      (funcall move-end)
      (setq end (point)))
    (when step
      (when (functionp step)
	(funcall step))
      (python-nav-forward-statement))
    (when python-multiline-highlight
      (let ((margin-start (if as-region 1 0))
	    (margin-end (if (or step as-region) 1 0)))
	(python--vhl-full-lines start (if step (point) end)
				margin-start margin-end)))
    (if as-region
	(python-shell-send-region start end)
	(let* ((substring (buffer-substring-no-properties start end))
	       (stm (python-string-to-statement substring)))
	  (python-shell-send-string stm)))))


;; Send with motion, by lines

;;;###autoload
(defun python-shell-send-line ()
  "Send the current line (with any remaining continuations) to the inferior Python process,
printing the result of the expression on the shell."
  (interactive)
  (python-shell--send-block-with-motion 'python-nav-beginning-of-statement
					'python-nav-end-of-statement
					nil nil))

;;;###autoload
(defun python-shell-send-line-and-step ()
  "Send the current line (with any remaining continuations) to the inferior Python process,
printing the result of the expression on the shell, then move on to the next
statement."
  (interactive)
  (python-shell--send-block-with-motion 'python-nav-beginning-of-statement
					'python-nav-end-of-statement
					t nil))


;; Send with motion, by paragraphs

;;;###autoload
(defun python-shell-send-paragraph ()
  "Send the current paragraph to the inferior Python process"
  (interactive)
  (python-shell--send-block-with-motion 'backward-paragraph 'forward-paragraph
					nil t))

;;;###autoload
(defun python-shell-send-paragraph-and-step ()
  "Send the current paragraph to the inferior Python process, then move on to
the next."
  (interactive)
  (python-shell--send-block-with-motion 'backward-paragraph 'forward-paragraph
					'forward-paragraph t))

;;;###autoload
(defun python-shell-send-region-or-paragraph ()
  "Send the current region to the inferior Python process, if active.
Otherwise, send the current paragraph."
  (interactive)
  (if (use-region-p)
      (python-shell-send-region (region-beginning) (region-end))
      (python-shell-send-paragraph)))


;; Delimited sections

;;;###autoload
(defcustom python-section-delimiter "# ---"
  "Define the comment which marks the boundaries of the current code section.
See `python-shell-send-fold-or-section'."
  :type 'string
  :group 'python-x)

;;;###autoload
(defcustom python-section-highlight (featurep 'volatile-highlights)
  "When evaluating a code fold/section with `python-shell-send-fold-or-section'
spanning more than one line, highlight temporarily the evaluated region using
`vhl/default-face'. Requires `volatile-highlights' to be installed."
  :type 'boolean
  :group 'python-x)

(defun python-section--skip-p (pos skip)
  (if (not skip) nil
      (let ((start (car skip))
	    (end (cadr skip)))
	(or (and (>= pos start) (<= pos end))
	    (python-section--skip-p pos (cddr skip))))))

(defun python-section--search-backward (bound skip)
  (ignore-errors
   (forward-char (length python-section-delimiter)))
  (cl-loop for pos = (search-backward python-section-delimiter bound t)
     while pos do
       (setq pos (match-beginning 0))
       (if (not (python-section--skip-p pos skip))
	   (cl-return pos))))

(defun python-section--search-forward (bound skip)
  (ignore-errors
    (forward-char))
  (cl-loop for pos = (search-forward python-section-delimiter bound t)
     while pos do
       (setq pos (match-beginning 0))
       (if (not (python-section--skip-p pos (nreverse skip)))
	   (cl-return pos))))

(defun python-section-search (rev)
  (unless folding-regexp
    ;; define folding markers, even when folding-mode is not active
    (folding-set-local-variables))
  (let ((case-fold-search nil)
	(ret (folding-skip-folds rev)))
    (let ((pos (or (car-safe ret)
		   (if rev (point-min) (point-max))))
	  (skip (cdr-safe ret)))
      (if (not python-section-delimiter) pos
	  (save-excursion
	    (or (if rev
		    (python-section--search-backward pos skip)
		    (python-section--search-forward pos skip))
		pos))))))

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
  (let ((start (python-section-search t))
	(end (python-section-search nil)))
    (when python-section-highlight
      (python--vhl-full-lines start end 1 1))
    (python-shell-send-region start end)))

;;;###autoload
(defun python-shell-send-fold-or-section-and-step ()
  "Send the section of code at point to the inferior Python process, up to the
current fold or buffer boundaries, then move on to the next."
  (interactive)
  (python-shell-send-fold-or-section)
  (python-forward-fold-or-section))

;;;###autoload
(defun python-shell-send-dwim ()
  "Send the current region to the inferior Python process, if active.
Otherwise, use `python-shell-send-current-fold-or-section'"
  (interactive)
  (if (use-region-p)
      (python-shell-send-region (region-beginning) (region-end))
      (python-shell-send-fold-or-section)))

;;;###autoload
(defun python-forward-fold-or-section (&optional count)
  "Move the point forward to the next fold or section marker. When a prefix
argument is provided, move COUNT times forward."
  (interactive "p")
  (unless count (setq count 1))
  (catch 'end
    (dotimes (i (abs count))
      (forward-line (if (> count 0) 1 -1))
      (let ((pos (python-section-search (< count 0))))
	(when (eq pos (point))
	  (throw 'end nil))
	(goto-char pos)))))

;;;###autoload
(defun python-backward-fold-or-section (&optional count)
  "Move the point backward to the previous fold or section marker. When a
prefix argument is provided, move COUNT times backward."
  (interactive "p")
  (unless count (setq count 1))
  (python-forward-fold-or-section (- count)))

;;;###autoload
(defun python-mark-fold-or-section (&optional arg allow-extend)
  "Put point at beginning of this fold/section, mark at end.
The region marked is the one that contains point or follows point.

With argument ARG, puts mark at end of a following fold/section, so that the
number of sections marked equals ARG.

If ARG is negative, point is put at end of this fold/section, mark is put at
beginning of this or a previous paragraph.

Interactively (or if ALLOW-EXTEND is non-nil), if this command is repeated
or (in Transient Mark mode) if the mark is active, it marks the next ARG
sections after the ones already marked."
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero sections"))
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (and transient-mark-mode mark-active)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (python-forward-fold-or-section arg)
	    (point))))
	(t
	 (python-forward-fold-or-section arg)
	 (push-mark nil t t)
	 (python-backward-fold-or-section arg))))


;; Exception handling

(defcustom python-shell-show-exceptions t
  "Display uncaught exceptions of the inferior Python process using
`python-shell-show-exception-function'."
  :type 'boolean
  :group 'python-x)

(defvar python-shell-show-exception-function
  (lambda (buffer)
    (when python-shell-show-exceptions
      (display-buffer buffer)))
  "Function invoked when the inferion Python process emits an uncaught
exception. By default, simply call `display-buffer' according to
`python-shell-show-exceptions'.")

(defvar python-comint-exceptions-regex
  (concat "\\(" (mapconcat
		 'identity
		 '("\\bTraceback (most recent call last):\n  File \""
		   "  File \"[^\"]+\", line [0-9]+\n.*\n +\\^\n\\(Syntax\\|Indentation\\)Error: ")
		 "\\|") "\\)")
  "Regular expression used to search for exceptions in the output")

(defun python-comint-find-exceptions (output)
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward python-comint-exceptions-regex
			      comint-last-output-start t)
      (funcall python-shell-show-exception-function (current-buffer)))))

(add-hook 'inferior-python-mode-hook
	  (lambda ()
	    (add-hook 'comint-output-filter-functions
		      'python-comint-find-exceptions)))


;; ElDoc/Help
(define-derived-mode python-help-mode special-mode "Python Help")

;;;###autoload
(defun python-eldoc-for-region-or-symbol ()
  "ElDoc for the current region or symbol at point. Similar to
`python-eldoc-at-point', but never prompts."
  (interactive)
  (let* ((substring (if (use-region-p)
			(buffer-substring-no-properties (region-beginning) (region-end))
			(python-info-current-symbol)))
	 (stm (python-string-to-statement substring)))
    (python-eldoc-at-point stm)))

;;;###autoload
(defun python-help-for-region-or-symbol ()
  "Display documentation for the current region or symbol at point."
  (interactive)
  (let* ((substring (if (use-region-p)
			(buffer-substring-no-properties (region-beginning) (region-end))
			(python-info-current-symbol)))
	 (stm (python-string-to-statement substring)))
    (let ((buffer (get-buffer-create "*help[Python]*"))
	  (output (python-shell-send-string-no-output (concat "help(" stm ")"))))
      (with-current-buffer buffer
	;; TODO: this is *very* rough
	(setq buffer-read-only nil)
	(buffer-disable-undo)
	(delete-region (point-min) (point-max))
	(insert output)
	(set-buffer-modified-p 'nil)
	(goto-char (point-min))
	(python-help-mode))
      (display-buffer buffer))))


;; Utilities

;;;###autoload
(defun python-shell-display-shell ()
  "Display the inferior Python process in another window."
  (interactive)
  (display-buffer (process-buffer (python-shell-get-process)) t))

;;;###autoload
(defun python-shell-print-region-or-symbol ()
  "Send the current region to the inferior Python process, if active; otherwise
the send the symbol at point. Print and display the result on the output buffer."
  (interactive)
  (let* ((substring (if (use-region-p)
			(buffer-substring-no-properties (region-beginning) (region-end))
			(python-info-current-symbol)))
	 (stm (python-string-to-statement substring)))
    (python-shell-send-string stm)
    (python-shell-display-shell)))


;; Configuration and setup

(defun python-x-mode-expansions ()
  "Add `python-x' specific expansions for `expand-region'"
  (set (make-local-variable 'er/try-expand-list)
    (append er/try-expand-list '(python-mark-fold-or-section))))

;;;###autoload
(defun python-x-setup ()
  "Setup an ESS-like keyboard map in python-mode"
  (define-key python-mode-map (kbd "C-c C-j") 'python-shell-send-line)
  (define-key python-mode-map (kbd "C-c C-n") 'python-shell-send-line-and-step)
  (define-key python-mode-map (kbd "C-c C-f") 'python-shell-send-defun)
  (define-key python-mode-map (kbd "C-c C-b") 'python-shell-send-buffer)
  (define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-dwim)
  (define-key python-mode-map (kbd "C-c C-z") 'python-shell-switch-to-shell)
  (define-key python-mode-map (kbd "C-c C-S-z") 'python-shell-display-shell)
  (define-key python-mode-map (kbd "M-<up>") 'python-backward-fold-or-section)
  (define-key python-mode-map (kbd "M-<down>") 'python-forward-fold-or-section)
  (define-key python-mode-map (kbd "M-<return>") 'python-shell-send-fold-or-section-and-step)
  (define-key python-mode-map (kbd "C-c C-h") 'python-eldoc-for-region-or-symbol)
  (define-key python-mode-map (kbd "C-c p p") 'python-shell-print-region-or-symbol)
  (define-key python-mode-map (kbd "C-c p h") 'python-help-for-region-or-symbol)
  (when (featurep 'expand-region)
    (er/enable-mode-expansions 'python-mode 'python-x-mode-expansions)))

(provide 'python-x)

;;; python-x.el ends here
