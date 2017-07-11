;;; eslintd-fix.el --- use eslint_d to automatically fix js files

;; Copyright (C) 2016 by Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/aaronjensen/eslintd-fix
;; Version: 1.0.0

;;; Commentary:

;; This package provides the eslintd-fix minor mode, which will use eslint_d
;; (https://github.com/mantoni/eslint_d.js) to automatically fix javascript code
;; before it is saved.

;; To use it, require it, make sure `eslint_d' is in your path and add it to
;; your favorite javascript mode:

;;    (add-hook 'js2-mode-hook #'eslintd-fix-mode)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(defgroup eslintd-fix nil
  "Fix javascript code with eslint_d"
  :group 'tools)

(defcustom eslintd-fix-executable "eslint_d"
  "The eslint_d executable used by `eslintd-fix'."
  :group 'eslintd-fix
  :type 'string)

(defcustom eslintd-fix-preprocess-command nil
  "The shell command to pipe the buffer into before piping to
  eslintd. This is useful for integrating `prettier', for
  example. It is ignored if `nil'."
  :group 'eslintd-fix
  :type 'string)

(defvar-local eslintd-fix--verified nil
  "Set to t if eslintd has been verified as working for this buffer.")

(defun eslintd-fix--goto-line (line)
  "Move point to LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun eslintd-fix--delete-whole-line (&optional arg)
    "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
    (setq arg (or arg 1))
    (if (and (> arg 0)
             (eobp)
             (save-excursion (forward-visible-line 0) (eobp)))
        (signal 'end-of-buffer nil))
    (if (and (< arg 0)
             (bobp)
             (save-excursion (end-of-visible-line) (bobp)))
        (signal 'beginning-of-buffer nil))
    (cond ((zerop arg)
           (delete-region (progn (forward-visible-line 0) (point))
                          (progn (end-of-visible-line) (point))))
          ((< arg 0)
           (delete-region (progn (end-of-visible-line) (point))
                          (progn (forward-visible-line (1+ arg))
                                 (unless (bobp)
                                   (backward-char))
                                 (point))))
          (t
           (delete-region (progn (forward-visible-line 0) (point))
                                                  (progn (forward-visible-line arg) (point))))))

(defun eslintd-fix--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in eslintd-fix--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (eslintd-fix--goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (eslintd-fix--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in eslintd-fix--apply-rcs-patch")))))))))

(defun eslintd-fix--compatible-versionp ()
  (let ((executable (executable-find eslintd-fix-executable)))
    (and executable
         (file-executable-p executable)
         (zerop (call-process-shell-command (concat
                                "("
                                executable
                                " --help | grep -qe '--fix-to-stdout'"
                                ")"))))))

(defun eslintd-fix--eslint-config-foundp ()
  (let ((executable (executable-find eslintd-fix-executable))
        (filename (buffer-file-name)))
    (with-temp-buffer
      (and filename
           (zerop
            (call-process executable nil t nil "--print-config" filename))))))

(defun eslintd-fix--verify ()
  (or eslintd-fix--verified
      (cond ((not (eslintd-fix--compatible-versionp))
             (eslintd-fix-mode -1)
             (message "eslintd-fix: Could not find eslint_d or it does not have the `--fix-to-stdout' feature.")
             nil)
            ((not (eslintd-fix--eslint-config-foundp))
             (eslintd-fix-mode -1)
             (message "eslintd-fix: Could not find an eslint config file.")
             nil)
            (t (setq eslintd-fix--verified t)))))

(defun eslintd-fix ()
  "Replace buffer contents with \"fixed\" code from eslint_d."
  (interactive)
  (let ((executable (executable-find eslintd-fix-executable)))
    (when (and (eslintd-fix--verify)
               executable
               (file-executable-p executable))
      (let ((command (concat
                      "("
                      " set -o pipefail;"
                      " original=$(cat);"
                      "<<<\"$original\" "
                      (when eslintd-fix-preprocess-command (concat eslintd-fix-preprocess-command " | "))
                      executable
                      " --stdin"
                      " --fix-to-stdout"
                      " --stdin-filename " buffer-file-name
                      " | ( diff -n <(echo \"$original\") -; true )"
                      " )"))
            (buffer (current-buffer))
            (buffer-min (point-min))
            (buffer-max (point-max)))
        (with-temp-buffer
          (insert-buffer-substring buffer buffer-min buffer-max)
          (when (zerop
                 (shell-command-on-region
                  ;; Region
                  (point-min)
                  (point-max)
                  ;; Command
                  command
                  ;; Output to current buffer
                  t
                  ;; Replace buffer
                  t
                  ;; Error buffer name
                  "*eslintd-fix error*"
                  ;; Display errors
                  t))
            (let ((patch-buffer (current-buffer)))
              (with-current-buffer buffer
                (eslintd-fix--apply-rcs-patch patch-buffer)))))))))

;;;###autoload
(define-minor-mode eslintd-fix-mode
  "Use eslint_d to automatically fix javascript before saving."
  :lighter " fix"
  (if eslintd-fix-mode
      (add-hook 'before-save-hook #'eslintd-fix nil t)
    (setq eslintd-fix--verified nil)
    (remove-hook 'before-save-hook #'eslintd-fix t)))

(provide 'eslintd-fix)
;;; eslintd-fix.el ends here
