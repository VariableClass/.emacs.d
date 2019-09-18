;;; Disable mouse input
(defun setup-handoff()

  ;; Disable mouse input with amusing consequences
  (handoff-global-mode))

;;; Set multi-term shell to environment shell and set as initial buffer
(defun setup-multi-term()

  ;; Configure multi-term
  (require 'multi-term)
  (setq-default multi-term-program (getenv "SHELL"))

  ;; Open multi-term in main buffer on startup
  (setq-default initial-buffer-choice 'multi-term))

;;; Set soft line wrap
(defun setup-visual-fill-column(line-wrap-index)

  ;; Enable soft wrap at 120 characters
  (setq-default visual-fill-column-width line-wrap-index)
  (global-visual-fill-column-mode t)
  (global-visual-line-mode t))

(provide 'package-config)
