;;; eslintd-fix.el --- use eslint_d to automatically fix js files -*- lexical-binding: t -*-

;; Copyright (C) 2016 by Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/aaronjensen/eslintd-fix
;; Version: 1.0.0
;; Package-Requires: ((dash "2.13.0"))

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
(require 'dash)

(defgroup eslintd-fix nil
  "Fix javascript code with eslint_d"
  :group 'tools)

(defcustom eslintd-fix-executable "eslint_d"
  "The eslint_d executable used by `eslintd-fix'."
  :group 'eslintd-fix
  :type 'string)

(defcustom eslintd-fix-portfile "~/.eslint_d"
  "The file written by eslint_d containing the port and token."
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

(defvar eslintd-fix-connection nil
  "An open, not-yet-used connection to eslint_d.")

(defun eslintd-fix--compatible-versionp (executable)
  (and (file-executable-p executable)
       (zerop (call-process-shell-command
               (concat
                "("
                executable
                " --help | grep -qe '--fix-to-stdout'"
                ")")))))

(defun eslintd-fix--eslint-config-foundp (executable)
  (let ((filename (buffer-file-name)))
    (and filename
         (zerop (call-process-shell-command
                 (concat
                  executable
                  " --print-config "
                  filename))))))

(defun eslintd-fix--verify (executable)
  (or eslintd-fix--verified
      (cond ((not (eslintd-fix--compatible-versionp executable))
             (eslintd-fix-mode -1)
             (message "eslintd-fix: Could not find eslint_d or it does not have the `--fix-to-stdout' feature.")
             nil)
            ((not (eslintd-fix--eslint-config-foundp executable))
             (eslintd-fix-mode -1)
             (message "eslintd-fix: Could not find an eslint config file.")
             nil)
            (t (setq eslintd-fix--verified t)))))

(defun eslintd-fix--read-portfile ()
  "Read and return contents of ~/.eslint_d as a list."
  (when (file-exists-p eslintd-fix-portfile)
    (with-temp-buffer
      (insert-file-contents eslintd-fix-portfile)
      (split-string (buffer-string) " " t))))

(defun eslintd-fix--start ()
  "Start eslint_d.

Return t if it successfully starts."
  (message "starting")
  (-if-let* ((executable (executable-find eslintd-fix-executable)))
      (and
       (eslintd-fix--verify executable)
       (zerop (call-process-shell-command
               (concat executable " start"))))))

(defun eslintd-fix--connection-sentinel (connection status)
  (message "sentinel %S" (process-status connection))
  (pcase (process-status connection)
    ('failed
     (if (process-get connection 'eslintd-fix-retry)
         (message "Could not start or connect to eslint_d.")
       (and (eslintd-fix--start)
            (eslintd-fix--open-connection t))))
    ('closed
     (-when-let* ((output-buffer (process-get connection 'eslintd-fix-output-buffer))
                  (output-file (process-get connection 'eslintd-fix-output-file))
                  (buffer (process-get connection 'eslintd-fix-buffer)))
       (message "outputting: %s" output-file)
       (with-current-buffer output-buffer
         (goto-char (point-max))
         (beginning-of-line)
         ;; Do not replace contents if there was an error
         (unless (looking-at "# exit [[:digit:]]+")
           (write-file output-file)
           (with-current-buffer buffer
             (insert-file-contents output-file nil nil nil t))))
       (kill-buffer output-buffer)
       (delete-file output-file)))))

(defun eslintd-fix--connection-filter (connection output)
  (-when-let* ((output-buffer (process-get connection 'eslintd-fix-output-buffer)))
    (with-current-buffer output-buffer
      (insert output))))

(defun eslintd-fix--open-connection (&optional is-retry)
  "Open a connection to eslint_d.

Return nil if eslint_d is not running. Also close the existing,
cached connection if it is already open."
  (and eslintd-fix-connection
       (delete-process eslintd-fix-connection))
  (-when-let* ((portfile (or (eslintd-fix--read-portfile)
                             (and (eslintd-fix--start)
                                  (eslintd-fix--read-portfile))))
               (port (car portfile))
               (token (cadr portfile))
               (connection
                (open-network-stream "eslintd-fix" nil "localhost" port :nowait t)))
    (process-put connection 'eslintd-fix-token token)
    (process-put connection 'eslintd-fix-retry is-retry)
    (set-process-query-on-exit-flag connection nil)
    (set-process-sentinel connection 'eslintd-fix--connection-sentinel)
    (message "%S" (process-status connection))
    (setq eslintd-fix-connection connection)))

(defun eslintd-fix--wait-for-connection (connection)
  "Wait for CONNECTION to connect.

Return the CONNECTION if, after waiting it is open, otherwise nil."
  (when connection
    (while (eq (process-status connection) 'connect)
      (sleep-for 0.01))
    (when (eq (process-status connection) 'open)
      connection)))

(defun eslintd-fix--get-connection ()
  "Return an open connection to eslint_d.

Will open a connection if there is not one and will Immediately
begin opening a new connection."
  (or (eslintd-fix--wait-for-connection eslintd-fix-connection)
      (eslintd-fix--wait-for-connection (eslintd-fix--open-connection))))

(defun eslintd-fix ()
  (interactive)
  (-when-let* ((connection (eslintd-fix--get-connection))
               (output-file (make-temp-file "eslintd-fix-"))
               ;; TODO Try making this not a file buffer
               (output-buffer (create-file-buffer output-file))
               (buffer (current-buffer))
               (token (process-get connection 'eslintd-fix-token)))
    (process-put connection 'eslintd-fix-buffer buffer)
    (process-put connection 'eslintd-fix-output-buffer output-buffer)
    (process-put connection 'eslintd-fix-output-file output-file)
    (set-process-filter connection 'eslintd-fix--connection-filter)
    (process-send-string connection
                         (concat token
                                 " " default-directory
                                 " --fix-to-stdout"
                                 " --stdin-filename " buffer-file-name
                                 " --stdin\n"))
    (process-send-region connection (point-min) (point-max))
    (process-send-eof connection)

    ;; Wait for connection to close
    (catch 'done
      (dotimes (_ 50)
        (if (eq (process-status connection) 'open)
            (accept-process-output connection 0.01 nil t)
          (throw 'done nil))))

    (message "done! %S" (process-status connection))
    ;; Open a new connection to save us time next time
    (eslintd-fix--open-connection)))

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
