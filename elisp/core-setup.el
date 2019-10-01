;;; core-setup.el --- Configure all settings which only rely on core Emacs modules
;;; Commentary:

;;; Code:
(defun configure-editor()
  "Configure editor options."

  ;; Turn on line numbers
  (global-display-line-numbers-mode)
  
  ;; Enable column index indicator
  (setq-default column-number-mode t)

  ;; Replace tabs with spaces and set tab width
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)

  ;; Bracket matching, first removing delay
  (setq-default show-paren-delay 0)
  (show-paren-mode 1)

  ;; Enable auto brace insertion and newline behaviour
  (require 'electric)
  (add-to-list 'electric-layout-rules '(?{ . around))
  (electric-layout-mode 1)
  (electric-pair-mode 1))


(defun define-mouse-behaviour()
  "Make pointer visible and have the cursor chase it."
  
  ;; Make mouse pointer visible
  (setq-default make-pointer-invisible nil)

  ;; Enable cat and mouse
  (mouse-avoidance-mode 'cat-and-mouse))

(defun divert-customize(custom-file-path)
  "Point the Custom tool to a separate file at CUSTOM-FILE-PATH."
  
  ;; Set custom file to be written elsewhere
  (setq-default custom-file custom-file-path))

(defun customise-frame()
  "Define frame appearance."
  
  ;; Disable menu bar and toolbar"
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  ;; Disable scroll-bar
  (toggle-scroll-bar -1)

  ;; Set frame to fullscreen
  (toggle-frame-fullscreen)

  ;; Disable splash screen
  (setq-default inhibit-splash-screen t))

(defun set-theme()
  "Define colours and fonts."

  ;; Set colour theme
  (load-theme 'misterioso)
  
  ;; Set default fontface
  (set-face-attribute 'default nil
   :inherit nil
   :stipple nil
   :background "#2E4747"
   :foreground "#DBC9AF"
   :inverse-video nil
   :box nil
   :strike-through nil
   :overline nil
   :underline nil
   :slant 'normal
   :weight 'normal
   :height 113
   :width 'normal
   :foundry "DAMA"
   :family "Ubuntu Mono")
  
  (custom-set-faces
   '(term-color-blue ((t (:foreground "#00FFFF"))))))

(provide 'core-setup)
;;; core-setup.el ends here
