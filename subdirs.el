;;
;; This file does 2 things.
;;
;;  * adds this directory to load-path
;;  * adds autoload (by loading "loaddefs.el") if missing
;;

(let ((fsym 'lsp-addon-jump-to-error-mode)
      (dir (or (and load-file-name (file-name-directory load-file-name))
	       default-directory)));; for ^X^E
  (add-to-list 'load-path dir)
  (if (or (not (fboundp fsym))
	  (autoloadp (symbol-function fsym)))
      (load (concat dir "loaddefs.el"))))

