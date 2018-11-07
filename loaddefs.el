;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "lsp-addon-jump-to-error" "lsp-addon-jump-to-error.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-addon-jump-to-error.el

(autoload 'lsp-addon-jump-to-error-mode "lsp-addon-jump-to-error" "\
Toggle lsp-addon-jump-to-error-mode.

Lsp-Addon-Jump-To-Error mode is a buffer-local minor mode used with
`lsp-mode'. When enabled and when you save the buffer,
Emacs automatically jumps to the first error position if it exists.
This mode also set mode-line color to `lsp-addon-jump-to-error-alert-color'.

Note: This mode does not depend on flycheck. It directly reads
lsp--diagnostics.

\(fn &optional ARG)" t nil)

(autoload 'lsp-addon-jump-to-error-mode-run "lsp-addon-jump-to-error" "\
Inspect lsp--diagnostics and jump to it's first error if it exists.
Also sets mode-line color to lsp-addon-jump-to-error-alert-color.

\(fn)" t nil)

(autoload 'lsp-addon-jump-to-error-set-mode-line-alert "lsp-addon-jump-to-error" "\
Add alert color to mode-line when ERR is true.

\(fn ERR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-addon-jump-to-error" '(#("lsp-addon-jump-to-error-alert-" 0 30 (fontified t face font-lock-variable-name-face)))))

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
