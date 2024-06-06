;; early-init.el --- LEmacs (Lionyx Emacs)
;;; Commentary:
;; Early init configuration for LEmacs

(setenv "LSP_USE_PLISTS" "true")

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

(setq inhibit-compacting-font-caches t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here

