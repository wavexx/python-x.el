;;; python-help-mode.el --- python documentation special mode  -*- lexical-binding: t -*-

(defconst python-help-keywords
  '((;; Various forms of intro
     ("\\`Help on "
      ("\\<function \\([[:word:]_.]+\\|<lambda>\\)" nil nil (1 'font-lock-function-name-face))
      ("\\<\\(?:package\\|module\\|class\\) \\([[:word:]_.]+\\)" nil nil (1 'font-lock-type-face))
      ("\\<\\([[:word:]_.]+\\) object:$" nil nil (1 'font-lock-type-face))
      ("\\<in \\([[:word:]_.]+\\):$" nil nil (1 'font-lock-type-face)))
     ;; Sections
     ("^[A-Z][A-Z ]+[A-Z]$" 0 'info-title-4)
     ;; Functions (with args)
     ("^ *\\(?:|  \\)*\\(?:[[:word:]_.]+ = \\)?\\([[:word:]_.]+\\)("
      (1 'font-lock-function-name-face)
      ("\\<self\\|True\\|False\\|None\\>" nil nil (0 'font-lock-keyword-face)))
     ;; Variables
     ("^ *\\(?:|  \\)*\\([[:word:]_.]+\\) = "
      (1 'font-lock-variable-name-face)
      ("\\<True\\|False\\|None\\>" nil nil (0 'font-lock-keyword-face)))
     ;; Classes
     ("^ *\\(?:|  \\)*\\(class\\) \\([[:word:]_.]+\\)("
      (1 'font-lock-keyword-face)
      (2 'font-lock-type-face)
      ("\\([[:word:]_.]+\\)" nil nil (0 'font-lock-type-face)))
     ;; Specials
     ("^ *\\(?:|  \\)*\\(__\\w+__\\)" 1 'font-lock-function-name-face))))

(defvar python-help-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    table))

(defvar-local python-help--parent-proc nil)

;;;###autoload
(define-derived-mode
    python-help-mode special-mode "Python Help"
    (buffer-disable-undo)
    (set-buffer-modified-p nil)
    (setq truncate-lines nil
	  word-wrap t
	  font-lock-defaults python-help-keywords))

(provide 'python-help-mode)

;;; python-help-mode.el ends here
