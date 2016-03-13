;;; python-x.el --- python.el extras for interactive evaluation  -*- lexical-binding: t -*-

;; Author: Yuri D'Elia <wavexx@thregr.org>

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
;; to control this behavior. The execution status of the inferior process is
;; tracked in the modeline, in order to know when the evaluation is complete.
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
;; - `python-shell-switch-to-shell-or-buffer': Switch between the buffer and
;;   the inferior (cycling command).
;; - `python-shell-restart-process': Restart the inferior Python process.
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
;;
;; See the `python-x' customization group for additional settings.


;;; Code:
(require 'python)
(require 'folding)
(require 'cl-lib)

;; Optional
(eval-when-compile
  (require 'expand-region nil t))

(defgroup python-x nil
    "Python eXtensions"
    :group 'python)


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
(defconst python--vhl-available (if (require 'volatile-highlights nil t) t))

;;;###autoload
(defcustom python-multiline-highlight python--vhl-available
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
      (funcall move-end)
      (setq end (point))
      (funcall move-start)
      (setq start (point)))
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
	       (string (python-string-to-statement substring)))
	  (python-shell-send-string string)))))


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
(defcustom python-section-highlight python--vhl-available
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
    ;; catch the mark at point (if any)
    (forward-char (length python-section-delimiter))
    (setq bound (min (point) bound)))
  (cl-loop for pos = (search-backward python-section-delimiter bound t)
     while pos do
       (setq pos (match-beginning 0))
       (if (not (python-section--skip-p pos skip))
	   (cl-return pos))))

(defun python-section--search-forward (bound skip)
  (ignore-errors
    ;; skip the mark at point (if any)
    (forward-char)
    (setq bound (max (point) bound)))
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
(defun python-shell-send-fold-or-section (&optional arg)
  "Send the section of code at point to the inferior Python process, up to the
current fold or buffer boundaries. When a 0 argument is provided, evaluate from
the beginning of the buffer up the current section. With a negative argument,
restart the process as well.

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
  (interactive "p")
  (unless arg (setq arg 1))
  (when (< arg 0)
    (python-shell-restart-process))
  (let ((start (if (< arg 1) (point-min) (python-section-search t)))
	(end (python-section-search nil)))
    (when python-section-highlight
      (python--vhl-full-lines start end 1 1))
    (python-shell-send-region start end)))

;;;###autoload
(defun python-x-shell-send-buffer (&optional arg)
  "Send the entire buffer to inferior Python process.
When called with a non-zero prefix argument, allow execution of code inside
blocks delimited by \"if __name__== '__main__':\". With a negative prefix
argument, restart the python process before evaluation."
  (interactive "p")
  (unless arg (setq arg 1))
  (when (< arg 0)
    (python-shell-restart-process))
  (call-interactively #'python-shell-send-buffer))

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
      (call-interactively #'python-shell-send-fold-or-section)))

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


;; Process/Exception handling
(defface python-x-modeline-ready-face
    '((t nil))
  "Face used for the \"ready\" state in the mode-line."
  :group 'python-x)

(defface python-x-modeline-running-face
    '((t :inherit compilation-mode-line-run))
  "Face used for the \"running\" state in the mode-line."
  :group 'python-x)

(defface python-x-modeline-error-face
    '((t :inherit compilation-mode-line-fail))
  "Face used for the \"error\" state in the mode-line."
  :group 'python-x)

(defface python-x-modeline-exited-face
    '((t :inherit compilation-mode-line-exit))
  "Face used for the \"exited\" state in the mode-line."
  :group 'python-x)

;; We need to keep an explicit reference to the inferior buffer to track
;; execution status among shared processes, along with the final command and
;; dedicated status. We hook the three main entry points, since there's no
;; single place we can get all of them.
(defvar-local python-shell--inferior-buffer nil)
(defvar-local python-shell--dedicated-p nil)
(defvar-local python-comint--current-cmd nil)

(defun python-shell--register-inferior (&rest r)
  (setq python-shell--inferior-buffer (process-buffer (python-shell-get-process))))
(add-function :after (symbol-function 'python-shell-get-or-create-process)
	      #'python-shell--register-inferior)

(defun python-shell--register-dedicated (dedicated)
  (setq python-shell--dedicated-p dedicated))
(add-function :filter-return (symbol-function 'run-python)
	      #'python-shell--register-dedicated)

(defun python-shell--register-cmd (f cmd &rest r)
  (let ((buffer (apply f cmd r)))
    (with-current-buffer (get-buffer buffer)
      (setq python-comint--current-cmd cmd))
    buffer))
(add-function :around (symbol-function 'python-shell-make-comint)
	      #'python-shell--register-cmd)

(defun python-comint--related-buffers ()
  "From an inferior process, return a list of buffers that are connected back
to us (in descending order of recency)."
  (let ((inferior-buffer (current-buffer)))
    (cl-remove-if-not
     (lambda (buffer)
       (with-current-buffer buffer
	 (when (eq major-mode 'python-mode)
	   (when (bufferp python-shell--inferior-buffer)
	     (eq inferior-buffer python-shell--inferior-buffer)))))
     (buffer-list))))

(defmacro python-comint--with-related-buffers (&rest body)
  "From an inferior process, evaluate BODY in all related buffers"
  `(dolist (buffer (python-comint--related-buffers))
     (with-current-buffer buffer ,@body)))

(defvar-local python-comint--process-state nil)

(defun python-comint--process-state-changed (state)
  (setq mode-line-process
	(cond ((eq state 'ready)
	       '(:propertize ":ok" face python-x-modeline-ready-face))
	      ((eq state 'running)
	       '(:propertize ":run" face python-x-modeline-running-face))
	      ((eq state 'error)
	       '(:propertize ":fail" face python-x-modeline-error-face))
	      ((eq state 'exited)
	       '(:propertize ":exit" face python-x-modeline-exited-face))))
  (force-mode-line-update))

(defun python-comint--update-process-state (state)
  (unless (eq python-comint--process-state state)
    (python-comint--with-related-buffers
     (python-comint--process-state-changed state))
    (setq python-comint--process-state state)))

(defun python-comint--process-state-run (&rest r)
  ;; We might run from either the main or inferior process, so setup the
  ;; initial buffer to be always the inferior
  (let ((process (python-shell-get-process)))
    (when process
      (with-current-buffer (process-buffer process)
	(python-comint--update-process-state 'running)))))

(add-function :after (symbol-function 'python-shell-send-string)
	      #'python-comint--process-state-run)

;;;###autoload
(defun python-shell-restart-process ()
  "Restart the current Python process"
  (interactive)
  (let ((proc-name (python-shell-get-process-name python-shell--dedicated-p))
	(process (python-shell-get-process)))
    (when process
      (with-current-buffer (process-buffer process)
	(comint-quit-subjob)
	(setq python-comint--process-state 'nil) ; clear the error state
	(let* ((cmdlist (split-string-and-unquote python-comint--current-cmd))
	       (interpreter (car cmdlist))
	       (args (cdr cmdlist)))
	  (comint-exec (current-buffer) proc-name interpreter nil args)
	  (run-hooks 'inferior-python-mode-hook))))))

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

(defun python-comint--process-sentinel (process event)
  (let ((buffer (process-buffer process)))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
	(python-comint--update-process-state
	 (if (zerop (process-exit-status process)) 'exited 'error))))))

(defun python-comint--output-filter (output)
  (unless (eq python-comint--process-state 'error)
    (let ((case-fold-search nil))
      (save-excursion
	(goto-char (point-max))
	(cond ((re-search-backward python-comint-exceptions-regex
				   comint-last-output-start t)
	       ;; exception in output
	       (python-comint--update-process-state 'error)
	       (funcall python-shell-show-exception-function (current-buffer)))
	      ((and (equal (comint-check-proc (current-buffer)) '(run stop))
		    (looking-back comint-prompt-regexp))
	       ;; ready
	       (python-comint--update-process-state 'ready)))))))

(defun python-comint--input-send (proc string)
  (let ((inhibit-send nil))
    (when (string-match (python-rx line-start (* whitespace)
				   (group symbol-name) (* whitespace)
				   (group "(" (* whitespace) (+ any) (* whitespace) ")")
				   (* whitespace) line-end)
			string)
      ;; function call
      (let ((func (match-string-no-properties 1 string))
	    (args (match-string-no-properties 2 string)))
	(when (and python-shell-capture-help
		   (string-equal func "help"))
	  (setq inhibit-send t)
	  (python-help--display-for-string proc args))))

    (python-comint--update-process-state 'running)
    (comint-simple-send proc (if inhibit-send "" string))))

(defun python-x--comint-setup ()
  (add-hook 'comint-output-filter-functions #'python-comint--output-filter)
  (setq-local comint-input-sender #'python-comint--input-send)
  (add-function :after (process-sentinel (get-buffer-process (current-buffer)))
		#'python-comint--process-sentinel)
  ;; python-shell--parent-buffer is (erroneusly) let-bound in python.el
  (setq-local python-shell--parent-buffer python-shell--parent-buffer)
  (python-comint--update-process-state 'ready))

(add-hook 'inferior-python-mode-hook #'python-x--comint-setup)


;; ElDoc/Help

(defcustom python-shell-capture-help t
  "When invoking help() from the prompt, capture the output into a regular *Help* buffer."
  :type 'boolean
  :group 'python-x)

;;;###autoload
(defun python-eldoc-for-region-or-symbol (string)
  "ElDoc for the current region or symbol at point. Similar to
`python-eldoc-at-point', but doesn't prompt unless given a prefix argument."
  (interactive
   (let* ((substring (if (use-region-p)
			 (buffer-substring-no-properties (region-beginning) (region-end))
			 (python-info-current-symbol)))
	  (string (python-string-to-statement substring)))
     (list (if current-prefix-arg
	       (read-string "ElDoc for: " string t)
	       string))))
    (python-eldoc-at-point string))

(defun python-help--display-for-string (proc string)
  (let ((buffer (get-buffer-create "*help[Python]*"))
	(output (python-shell-send-string-no-output (concat "help(" string ")") proc)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (insert output)
      (goto-char (point-min))
      (python-help-mode)
      (setq python-help--parent-proc proc))
    (display-buffer buffer)))

;;;###autoload
(defun python-help-for-region-or-symbol (string)
  "Display documentation for the current region or symbol at point. If a prefix
argument is given, prompt for a statement to inspect."
  (interactive
   (let* ((substring (if (use-region-p)
			 (buffer-substring-no-properties (region-beginning) (region-end))
			 (python-info-current-symbol)))
	  (string (python-string-to-statement substring)))
     (list (if current-prefix-arg
	       (read-string "Help for: " string t)
	       string))))
  (python-help--display-for-string (python-shell-get-process) string))


;; Utilities

;;;###autoload
(defun python-shell-display-shell ()
  "Display the inferior Python process in another window."
  (interactive)
  (display-buffer (process-buffer (python-shell-get-process)) t))

(defun python-shell-switch-to-buffer ()
  "From an inferior process, switch back to parent Python buffer."
  (interactive)
  (let ((buffer (car-safe (python-comint--related-buffers))))
    (if buffer
	(pop-to-buffer buffer)
	(message "No associated Python buffer"))))

(defun python-shell-switch-to-shell-or-buffer ()
  "From a Python script, display the inferior process in another window. From
an inferior process, switch back to parent Python buffer.

  This is a single-key command. Assuming that it is bound to C-c C-z, you can
navigate back and forth between the buffers with C-c C-z C-z C-z ..."
  (interactive)
  (let ((ev (vector last-command-event))
	(map (make-sparse-keymap))
	(fun (lambda ()
	       (interactive)
	       (if (eq major-mode 'inferior-python-mode)
		   (python-shell-switch-to-buffer)
		   (python-shell-switch-to-shell)))))
    (define-key map ev fun)
    (set-transient-map map t)
    (funcall fun)))

;;;###autoload
(defun python-shell-print-region-or-symbol ()
  "Send the current region to the inferior Python process, if active; otherwise
the send the symbol at point. Print and display the result on the output buffer."
  (interactive)
  (let* ((substring (if (use-region-p)
			(buffer-substring-no-properties (region-beginning) (region-end))
			(python-info-current-symbol)))
	 (string (python-string-to-statement substring)))
    (python-shell-send-string string)
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
  (define-key python-mode-map (kbd "C-c C-b") 'python-x-shell-send-buffer)
  (define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-dwim)
  (define-key python-mode-map (kbd "C-c C-z") 'python-shell-switch-to-shell-or-buffer)
  (define-key python-mode-map (kbd "C-c C-S-z") 'python-shell-display-shell)
  (define-key python-mode-map (kbd "M-<up>") 'python-backward-fold-or-section)
  (define-key python-mode-map (kbd "M-<down>") 'python-forward-fold-or-section)
  (define-key python-mode-map (kbd "M-<return>") 'python-shell-send-fold-or-section-and-step)
  (define-key python-mode-map (kbd "C-c C-h") 'python-eldoc-for-region-or-symbol)
  (define-key python-mode-map (kbd "C-c p p") 'python-shell-print-region-or-symbol)
  (define-key python-mode-map (kbd "C-c p h") 'python-help-for-region-or-symbol)
  (define-key inferior-python-mode-map (kbd "C-c C-z") 'python-shell-switch-to-shell-or-buffer)
  (when (featurep 'expand-region)
    (er/enable-mode-expansions 'python-mode 'python-x-mode-expansions)))

(provide 'python-x)

;;; python-x.el ends here
