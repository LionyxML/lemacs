;; early-init.el --- LEmacs (Lionyx Emacs)
;;; Commentary:
;; Early init configuration for LEmacs

(setenv "LSP_USE_PLISTS" "true")

(defun lemacs/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs."
  (setq mode-line-format nil)
  ;; These colors should match your selected theme for maximum effect
  ;; Note that for catppuccin whenever we create a new frame or open it on terminal
  ;; it is necessary to reload the theme.
  (set-face-attribute 'mode-line nil :background "#1E1E2D" :foreground "#1E1E2D" :box 'unspecified))


;; (lemacs/avoid-initial-flash-of-light)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

(setq inhibit-compacting-font-caches t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here

