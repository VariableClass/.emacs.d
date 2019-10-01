;;; init.el --- Emacs initialisation
;;; Commentary:

;;; Code:
(defun init()
  "Initialise Emacs."

  ;; Determine operating system
  (defvar linux-p (string-match "linux" (symbol-name system-type)))
  (defvar macos-p (string-match "darwin" (symbol-name system-type)))
  (defvar mswindows-p (string-match "windows" (symbol-name system-type)))

  ;; Add .emacs.d/lisp to load path and import local modules
  (add-to-list 'load-path "~/.emacs.d/elisp/")

  ;; Perform any setup that can be achieved without external packages
  (require 'core-setup)
  (declare-function customise-frame 'core-setup ())
  (declare-function set-theme 'core-setup ())
  (declare-function configure-editor 'core-setup ())
  (declare-function define-mouse-behaviour 'core-setup ())
  (declare-function divert-customize 'core-setup (custom-file-path))
  (customise-frame)
  (set-theme)
  (configure-editor)
  (define-mouse-behaviour)
  (divert-customize "~/.emacs.d/elisp/custom.el")

  ;; Attempt package installation
  (require 'install-packages)
  (declare-function install-packages "install-packages" (packages))
  (install-packages
   '(centered-cursor-mode company company-quickhelp csharp-mode dash editorconfig epl f flycheck handoff magit multi-term omnisharp pkg-info popup projectile s sx visual-fill-column yaml-mode))

  ;; Configure the various downloaded packages
  (require 'package-config)
  (declare-function setup-visual-fill-column "package-config" (line-wrap-index))
  (declare-function setup-handoff "package-config" ())
  (declare-function setup-projectile "package-config" ())
  (declare-function setup-editorconfig "package-config" ())
  (declare-function setup-flycheck "package-config" ())
  (declare-function setup-company "package-config" ())
  (declare-function setup-omnisharp "package-config" ())
  (declare-function setup-yaml-mode "package-config" ())
  (declare-function setup-rainbow-mode "package-config" ())
  (declare-function setup-centered-cursor-mode "package-config" ())
  (setup-visual-fill-column 120)
  (setup-handoff)
  (setup-projectile)
  (setup-editorconfig)
  (setup-flycheck)
  (setup-company)
  (setup-omnisharp)
  (setup-yaml-mode)
  (setup-centered-cursor-mode))

(init)

(provide 'init)
;;; init.el ends here
