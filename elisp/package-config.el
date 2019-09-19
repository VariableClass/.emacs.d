;;; package-config.el --- Configure installed packages
;;; Commentary:

;;; Code:
(defun setup-company()
  "Setup company."

  (require 'company)
  (require 'company-quickhelp)

  ;; Add Emacs initialisation hook for company and set popup delay to 0
  (add-hook 'after-init-hook 'global-company-mode)
  (setq-default company-idle-delay 0)

  ;; Enable company-quickhelp documentation popup
  (company-quickhelp-mode t)

  ;; Inherit colour scheme
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

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

  (require 'company)
  (require 'omnisharp)
  (declare-function omnisharp--install-server "omnisharp" (reinstall silent-installation))
  
  ;; Install Omnisharp server
  (omnisharp--install-server nil t)

  ;; Add omnisharp to company's list of backends
  (add-to-list 'company-backends 'company-omnisharp)
  
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
