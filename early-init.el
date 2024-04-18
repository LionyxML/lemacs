;; early-init.el --- LEmacs (Lionyx Emacs)
;;; Commentary:
;; Early init configuration for LEmacs

(setenv "LSP_USE_PLISTS" "true")

(defun lemacs/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs."
    (setq mode-line-format nil)
    (set-face-attribute 'default nil :background "#191724" :foreground "#ffffff")
    (set-face-attribute 'mode-line nil :background "#191724" :foreground "#ffffff" :box 'unspecified))

(lemacs/avoid-initial-flash-of-light)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here

