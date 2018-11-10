;;; on-save-jump-to-lsp-error.el --- Minor mode for jump-to-error on save with lsp-mode -*- lexical-binding: t -*-

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
;; URL: https://github.com/hkoba/emacs-on-save-jump-to-lsp-error
;; Package-Requires: ((lsp-mode "5"))
;; Version: 0.1

;;; Commentary:

;;; Code:

(require 'lsp-mode)
(require 'cl-seq)

(make-variable-buffer-local
 (defvar on-save-jump-to-lsp-error-face-remap-cookie nil
   "Holds original mode-line color."))

(defvar on-save-jump-to-lsp-error-alert-face '(mode-line mode-line-inactive)
  ;; 'fringe
  "Target face to notify alert.")

(defvar on-save-jump-to-lsp-error-alert-color "orange" "Default color for alert.")

;;;###autoload
(define-minor-mode on-save-jump-to-lsp-error-mode
  "Toggle on-save-jump-to-lsp-error-mode.

On-Save-Jump-To-Lsp-Error mode is a buffer-local minor mode used with
`lsp-mode'. When enabled and when you save the buffer,
Emacs automatically jumps to the first error position if it exists.
This mode also set mode-line color to `on-save-jump-to-lsp-error-alert-color'.

Note: This mode does not depend on flycheck. It directly reads
lsp--diagnostics."

  :lighter ":Jmp2Err"
  (when lsp-mode
    (let ((hook 'after-save-hook) (fn 'on-save-jump-to-lsp-error-mode-run)
	  (buf (current-buffer)))
      (cond
       (on-save-jump-to-lsp-error-mode
        (add-hook hook fn nil t))
       (t
        (remove-hook hook fn t))))))

;;;###autoload
(defun on-save-jump-to-lsp-error-mode-run ()
  "Inspect lsp--diagnostics and jump to it's first error if it exists.
Also sets mode-line color to on-save-jump-to-lsp-error-alert-color."
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
      (on-save-jump-to-lsp-error-set-mode-line-alert error-diag))))
  
;;;###autoload
(defun on-save-jump-to-lsp-error-set-mode-line-alert (err)
  "Add alert color to mode-line when ERR is true."
  ;; First remove old face remapping
  (when on-save-jump-to-lsp-error-face-remap-cookie
    (dolist (ck on-save-jump-to-lsp-error-face-remap-cookie)
      (face-remap-remove-relative ck)))
  ;; Then add new face remapping if err
  (setq on-save-jump-to-lsp-error-face-remap-cookie
        (when err
          (mapcar (lambda (f)
                    (face-remap-add-relative
                     f ':background on-save-jump-to-lsp-error-alert-color))
                  on-save-jump-to-lsp-error-alert-face))))

(provide 'on-save-jump-to-lsp-error)
;;; on-save-jump-to-lsp-error ends here
