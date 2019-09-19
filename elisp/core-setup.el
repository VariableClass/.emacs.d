;;; core-setup.el --- Configure all settings which only rely on core Emacs modules
;;; Commentary:

;;; Code:
(defun configure-editor()
  "Configure editor options."
  
  ;; Enable column index indicator
  (setq-default column-number-mode t)

  ;; Replace tabs with spaces and Set tab width
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))

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
  (setq-default inhibit-splash-screen t)

  ;; Split frame into two columns
  (split-window-horizontally))

(defun set-theme()
  "Define colours and fonts."

  ;; Set colour theme
  (load-theme 'misterioso)

  ;; Set font
  (set-face-attribute 'default nil
   :inherit nil
   :stipple nil
   :background "#2d3743"
   :foreground "#e1e1e0"
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
   :family "Ubuntu Mono"))

(provide 'core-setup)
;;; core-setup.el ends here
