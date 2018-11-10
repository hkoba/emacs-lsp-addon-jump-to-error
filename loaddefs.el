;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "on-save-jump-to-lsp-error" "on-save-jump-to-lsp-error.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from on-save-jump-to-lsp-error.el

(autoload 'on-save-jump-to-lsp-error-mode "on-save-jump-to-lsp-error" "\
Toggle on-save-jump-to-lsp-error-mode.

On-Save-Jump-To-Lsp-Error mode is a buffer-local minor mode used with
`lsp-mode'. When enabled and when you save the buffer,
Emacs automatically jumps to the first error position if it exists.
This mode also set mode-line color to `on-save-jump-to-lsp-error-alert-color'.

Note: This mode does not depend on flycheck. It directly reads
lsp--diagnostics.

\(fn &optional ARG)" t nil)

(autoload 'on-save-jump-to-lsp-error-mode-run "on-save-jump-to-lsp-error" "\
Inspect lsp--diagnostics and jump to it's first error if it exists.
Also sets mode-line color to on-save-jump-to-lsp-error-alert-color.

\(fn)" t nil)

(autoload 'on-save-jump-to-lsp-error-set-mode-line-alert "on-save-jump-to-lsp-error" "\
Add alert color to mode-line when ERR is true.

\(fn ERR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "on-save-jump-to-lsp-error" '("on-save-jump-to-lsp-error-alert-")))

;;;***

;;;### (autoloads nil nil ("subdirs.el") (0 0 0 0))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
