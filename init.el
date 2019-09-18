;;; Add .emacs.d/lisp to load path and import local modules
(add-to-list 'load-path "~/.emacs.d/elisp/")

;;; Perform any setup that can be achieved without external packages
(require 'core-setup)
(customise-frame)
(set-theme)
(configure-editor)
(define-mouse-behaviour)
(divert-customize "~/.emacs.d/elisp/custom.el")

;;; Attempt package installation
(require 'install-packages)
(install-packages
  '(auto-complete csharp-mode dash epl f flycheck handoff multi-term omnisharp pkg-info popup s visual-fill-column))

;;; Configure the various downloaded packages
(require 'package-config)
(setup-multi-term)
(setup-visual-fill-column 120)
(setup-handoff)
