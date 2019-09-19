;;; package-config.el --- Configure installed packages
;;; Commentary:

;;; Code:
(defun setup-auto-complete()
  "Setup auto completion."

  (require 'auto-complete)

  ;; Use default auto-complete configuration
  (ac-config-default))

(defun setup-editorconfig()
  "Setup use of .editorconfig files."

  (require 'editorconfig)
  
  (editorconfig-mode 1))

(defun setup-flycheck()
  "Enable flycheck for all programming modes."

  (require 'flycheck)
  
  ;; Add programming hook
  (add-hook 'prog-mode-hook 'flycheck-mode))

(defun setup-handoff()
  "Disable mouse input."

  (require 'handoff)
  
  ;; Disable mouse input with amusing consequences
  (handoff-global-mode))

(defun setup-multi-term()
  "Set multi-term shell to environment shell and set as initial buffer."

  (require 'multi-term)
  
  ;; Configure multi-term
  (setq-default multi-term-program (getenv "SHELL"))

  ;; Open multi-term in main buffer on startup
  (setq-default initial-buffer-choice 'multi-term))

(defun setup-omnisharp()
  "Install Omnisharp server and add server start to csharp hook."

  (require 'omnisharp)
  (declare-function omnisharp--install-server "omnisharp" (reinstall silent-installation))
  
  ;; Install Omnisharp server
  (omnisharp--install-server nil t)
  
  ;; Start Omnisharp when opening a C# file
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(defun setup-projectile()
  "Enable projectile to support Omnisharp in finding project files."

  (require 'projectile)
  
  ;; Enable projectile
  (projectile-mode +1))

(defun setup-visual-fill-column(line-wrap-index)
  "Set soft line wrap at the provided LINE-WRAP-INDEX."

  (require 'visual-fill-column)
  
  ;; Enable soft wrap at 120 characters
  (setq-default visual-fill-column-width line-wrap-index)
  (global-visual-fill-column-mode t)
  (global-visual-line-mode t))

(provide 'package-config)
;;; package-config.el ends here
