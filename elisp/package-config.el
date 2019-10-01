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
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  
  ;; Set keybindings
  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-g") 'omnisharp-go-to-definition))

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

(defun setup-yaml-mode()
  "Enable YAML syntax highlighting and newline behaviour."

  (require 'yaml-mode)

  ;; Enable automatic YAML file syntax highlighting
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

  ;; Set up automatic newline with indent behaviour
  (add-hook 'yaml-mode-hook
            '(lambda()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


(provide 'package-config)
;;; package-config.el ends here
