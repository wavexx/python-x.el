;;; python-help-mode.el --- python documentation special mode  -*- lexical-binding: t -*-

;;;###autoload
(define-derived-mode
    python-help-mode special-mode "Python Help"
    (setq truncate-lines nil
	  word-wrap t))

(provide 'python-help-mode)

;;; python-help-mode.el ends here
