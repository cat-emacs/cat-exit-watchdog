;;; cat-exit-watchdog.el --- Diagnose intermittent exit hangs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Misaka

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: processes, tools, debugging
;; URL: https://github.com/cat-emacs/cat-exit-watchdog

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `cat-exit-watchdog-mode' captures Emacs Lisp and native stacks when
;; `save-buffers-kill-emacs' takes longer than a configurable threshold.
;; Native capture uses sample on macOS, gdb/gstack/pstack/eu-stack on Unix,
;; and cdb on Windows when one of those tools is available.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup cat-exit-watchdog nil
  "Capture diagnostics when exiting Emacs takes too long."
  :group 'environment)

(defcustom cat-exit-watchdog-delay 8
  "Seconds to wait before capturing exit diagnostics."
  :type 'number
  :group 'cat-exit-watchdog)

(defcustom cat-exit-watchdog-native-duration 5
  "Seconds available to a native stack capture."
  :type 'integer
  :group 'cat-exit-watchdog)

(defcustom cat-exit-watchdog-log-directory
  (expand-file-name "exit-watchdog/"
                    (if (boundp 'cat-cache-dir)
                        cat-cache-dir
                      user-emacs-directory))
  "Directory in which exit diagnostics are stored."
  :type 'directory
  :group 'cat-exit-watchdog)

(defcustom cat-exit-watchdog-retained-logs 10
  "Maximum number of exit attempts whose logs are retained."
  :type 'natnum
  :group 'cat-exit-watchdog)

(defvar cat-exit-watchdog--current-log nil)

(defun cat-exit-watchdog--append (file format-string &rest args)
  "Append FORMAT-STRING with ARGS to FILE."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (apply #'format format-string args) nil
                  file 'append 'silent)))

(defun cat-exit-watchdog--executable (&rest names)
  "Return the first executable found among NAMES."
  (seq-some #'executable-find names))

(defun cat-exit-watchdog--native-command (pid output)
  "Return a native stack command for PID writing to OUTPUT."
  (pcase system-type
    ('darwin
     (when-let* ((sample (executable-find "sample")))
       (list sample pid
             (number-to-string cat-exit-watchdog-native-duration)
             "10" "-file" output)))
    ((or 'gnu/linux 'berkeley-unix 'cygwin)
     (cond
      ((cat-exit-watchdog--executable "gdb")
       (list (cat-exit-watchdog--executable "gdb") "--batch" "--quiet"
             "-ex" "set pagination off"
             "-ex" (format "set logging file %S" output)
             "-ex" "set logging overwrite on"
             "-ex" "set logging enabled on"
             "-ex" "thread apply all backtrace full"
             "-ex" "detach" "-p" pid))
      ((cat-exit-watchdog--executable "gstack" "pstack" "eu-stack")
       (let ((tool (cat-exit-watchdog--executable
                    "gstack" "pstack" "eu-stack")))
         (list (or (executable-find "sh") "/bin/sh") "-c"
               (format "exec %s %s > %s 2>&1"
                       (shell-quote-argument tool)
                       (if (string-match-p "eu-stack\\'" tool)
                           (concat "-p " (shell-quote-argument pid))
                         (shell-quote-argument pid))
                       (shell-quote-argument output)))))))
    ((or 'windows-nt 'ms-dos)
     (when-let* ((cdb (cat-exit-watchdog--executable "cdb.exe" "cdb")))
       (list cdb "-pv" "-p" pid "-logo" output "-c" "~* kb; q")))
    (_ nil)))

(defconst cat-exit-watchdog--library-file
  (or load-file-name (locate-library "cat-exit-watchdog"))
  "Absolute path to this library for the external helper.")

(defun cat-exit-watchdog--local-process-attributes (pid)
  "Return local process attributes for PID."
  (let ((default-directory temporary-file-directory))
    (process-attributes pid)))

(defun cat-exit-watchdog--helper-run (parent-pid parent-start delay command log)
  "After DELAY, run COMMAND if PARENT-PID still has PARENT-START."
  (sleep-for delay)
  (when (equal (alist-get 'start
                          (cat-exit-watchdog--local-process-attributes
                           parent-pid))
               parent-start)
    (condition-case err
        (let ((status (apply #'call-process (car command) nil nil nil
                             (cdr command))))
          (unless (and (integerp status) (zerop status))
            (cat-exit-watchdog--append
             log "Native capture failed with status %S\n" status)))
      (error
       (cat-exit-watchdog--append
        log "Native capture failed: %s\n" (error-message-string err))))))

(defun cat-exit-watchdog--emacs-command (form)
  "Return a command that evaluates FORM in a minimal Emacs process."
  (list (expand-file-name invocation-name invocation-directory)
        "--batch" "-Q" "-l" cat-exit-watchdog--library-file
        "--eval" (prin1-to-string form)))

(defun cat-exit-watchdog--start-helper (native-command native-log)
  "Start an external watchdog for NATIVE-COMMAND writing NATIVE-LOG."
  (when-let* ((native-command)
              (parent-start
               (alist-get 'start
                          (cat-exit-watchdog--local-process-attributes
                           (emacs-pid)))))
    (make-process
     :name "cat-exit-watchdog-helper" :buffer nil :noquery t
     :connection-type 'pipe
     :command
     (cat-exit-watchdog--emacs-command
      `(cat-exit-watchdog--helper-run
        ,(emacs-pid) ',parent-start ,cat-exit-watchdog-delay
        ',native-command ,native-log)))))

(defun cat-exit-watchdog--prune-logs ()
  "Remove exit attempts older than `cat-exit-watchdog-retained-logs'."
  (when (file-directory-p cat-exit-watchdog-log-directory)
    (let* ((logs (directory-files cat-exit-watchdog-log-directory t
                                  "\\`exit-.*\\.log\\'"))
           (newest-first
            (sort logs
                  (lambda (a b)
                    (time-less-p
                     (file-attribute-modification-time (file-attributes b))
                     (file-attribute-modification-time (file-attributes a)))))))
      (dolist (log (nthcdr cat-exit-watchdog-retained-logs newest-first))
        (ignore-errors (delete-file log))
        (ignore-errors (delete-file (concat log ".native")))))))

(defun cat-exit-watchdog--new-log ()
  "Return a unique log path for the current exit attempt."
  (make-directory cat-exit-watchdog-log-directory t)
  (expand-file-name
   (format "exit-%s-%d-%06d.log"
           (format-time-string "%Y%m%dT%H%M%S")
           (emacs-pid) (random 1000000))
   cat-exit-watchdog-log-directory))

(defun cat-exit-watchdog--buffer-state ()
  "Return state relevant to buffers that can delay exiting."
  (mapcar (lambda (buffer)
            (list :name (buffer-name buffer)
                  :file (buffer-file-name buffer)
                  :modified (buffer-modified-p buffer)))
          (seq-filter
           (lambda (buffer)
             (or (buffer-modified-p buffer) (buffer-file-name buffer)))
           (buffer-list))))

(defun cat-exit-watchdog--process-state ()
  "Return state relevant to processes that can delay exiting."
  (mapcar (lambda (process)
            (list :name (process-name process)
                  :status (process-status process)
                  :query-on-exit (process-query-on-exit-flag process)
                  :buffer (when-let* ((buffer (process-buffer process)))
                            (buffer-name buffer))))
          (process-list)))

(defun cat-exit-watchdog--capture-elisp-backtrace (log)
  "Append the current Lisp stack to LOG."
  (cat-exit-watchdog--append
   log "\n%s Elisp watchdog fired\n%s\n"
   (format-time-string "%FT%T%z")
   (with-output-to-string (backtrace))))

(defun cat-exit-watchdog--around (original &rest args)
  "Run ORIGINAL with ARGS and diagnose a slow exit."
  (let* ((log (cat-exit-watchdog--new-log))
         (native-log (concat log ".native"))
         (native-command
          (cat-exit-watchdog--native-command
           (number-to-string (emacs-pid)) native-log))
         (timer nil)
         (helper nil))
    (setq cat-exit-watchdog--current-log log)
    (cat-exit-watchdog--append
     log
     "%s exit started\nsystem=%S\nemacs=%S\nbuffers=%S\nprocesses=%S\nquery=%S\nhook=%S\nnative-command=%S\n"
     (format-time-string "%FT%T%z") system-type emacs-version
     (cat-exit-watchdog--buffer-state)
     (cat-exit-watchdog--process-state)
     kill-emacs-query-functions kill-emacs-hook native-command)
    (unless native-command
      (cat-exit-watchdog--append
       log "native capture unavailable: no supported debugger found\n"))
    (cat-exit-watchdog--prune-logs)
    (setq timer
          (run-at-time cat-exit-watchdog-delay nil
                       #'cat-exit-watchdog--capture-elisp-backtrace log)
          helper (cat-exit-watchdog--start-helper native-command native-log))
    (unwind-protect (apply original args)
      (when (timerp timer) (cancel-timer timer))
      (when (process-live-p helper) (delete-process helper)))))

;;;###autoload
(define-minor-mode cat-exit-watchdog-mode
  "Capture diagnostics when `save-buffers-kill-emacs' is slow."
  :global t
  :group 'cat-exit-watchdog
  (if cat-exit-watchdog-mode
      (advice-add #'save-buffers-kill-emacs :around
                  #'cat-exit-watchdog--around)
    (advice-remove #'save-buffers-kill-emacs
                   #'cat-exit-watchdog--around)))

(provide 'cat-exit-watchdog)
;;; cat-exit-watchdog.el ends here
