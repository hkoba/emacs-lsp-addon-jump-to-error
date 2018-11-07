;;; lsp-addon-jump-to-error.el --- Minor mode for jump-to-error on save with lsp-mode -*- lexical-binding: t -*-

;;; Copyright (C) 2018 Kobayasi, Hiroaki <buribullet@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Kobayasi, Hiroaki <buribullet@gmail.com>
;; URL: https://github.com/hkoba/emacs-lsp-addon-jump-to-error
;; Package-Requires: ((lsp-mode "5"))
;; Version: 0.1

;;; Commentary:

;;; Code:

(require 'lsp-mode)
(require 'cl-seq)

(make-variable-buffer-local
 (defvar lsp-addon-jump-to-error-face-remap-cookie nil
   "Holds original mode-line color."))

(defvar lsp-addon-jump-to-error-alert-face '(mode-line mode-line-inactive)
  ;; 'fringe
  "Target face to notify alert.")

(defvar lsp-addon-jump-to-error-alert-color "orange" "Default color for alert.")

;;;###autoload
(define-minor-mode lsp-addon-jump-to-error-mode
  "Toggle lsp-addon-jump-to-error-mode.

Lsp-Addon-Jump-To-Error mode is a buffer-local minor mode used with
`lsp-mode'. When enabled and when you save the buffer,
Emacs automatically jumps to the first error position if it exists.
This mode also set mode-line color to `lsp-addon-jump-to-error-alert-color'.

Note: This mode does not depend on flycheck. It directly reads
lsp--diagnostics."

  :lighter ":Jmp2Err"
  (when lsp-mode
    (let ((hook 'after-save-hook) (fn 'lsp-addon-jump-to-error-mode-run)
	  (buf (current-buffer)))
      (cond
       (lsp-addon-jump-to-error-mode
        (add-hook hook fn nil t))
       (t
        (remove-hook hook fn t))))))

;;;###autoload
(defun lsp-addon-jump-to-error-mode-run ()
  "Inspect lsp--diagnostics and jump to it's first error if it exists.
Also sets mode-line color to lsp-addon-jump-to-error-alert-color."
  (interactive)
  (when lsp-mode
    (let* ((diag-list
            (or (gethash buffer-file-name lsp--diagnostics)
                (gethash (file-truename buffer-file-name) lsp--diagnostics)))
           (error-diag
            (cl-find-if (lambda (diag) (eq (lsp-diagnostic-severity diag) 1))
                        diag-list)))
      (when error-diag
        (goto-line (1+ (lsp-diagnostic-line error-diag)))
        (forward-char (lsp-diagnostic-column error-diag)))
      (lsp-addon-jump-to-error-set-mode-line-alert error-diag))))
  
;;;###autoload
(defun lsp-addon-jump-to-error-set-mode-line-alert (err)
  "Add alert color to mode-line when ERR is true."
  ;; First remove old face remapping
  (when lsp-addon-jump-to-error-face-remap-cookie
    (dolist (ck lsp-addon-jump-to-error-face-remap-cookie)
      (face-remap-remove-relative ck)))
  ;; Then add new face remapping if err
  (setq lsp-addon-jump-to-error-face-remap-cookie
        (when err
          (mapcar (lambda (f)
                    (face-remap-add-relative
                     f ':background lsp-addon-jump-to-error-alert-color))
                  lsp-addon-jump-to-error-alert-face))))

(provide 'lsp-addon-jump-to-error)
;;; lsp-addon-jump-to-error ends here
