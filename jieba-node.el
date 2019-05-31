;;; jieba-node.el  --- nodejieba backend  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; Keywords: chinese

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'jieba)
(require 'jsonrpc)

;;; JSONRPC setup

(defvar jieba--current-node-conn nil)

(defun jieba--json-read-string (s)
  (if (fboundp 'json-parse-string)
      (json-parse-string s
                         :object-type 'plist
                         :false-object :json-false
                         :null-object nil)
    (let ((json-object-type 'plist)
          (json-false :json-false)
          (json-null nil))
      (json-read-from-string s))))

(defclass jieba-node-connection (jsonrpc-process-connection) ()
  "A connection based on stdio to contact with jieba server.")

(cl-defmethod jsonrpc-connection-send ((conn jieba-node-connection)
                                       &rest args
                                       &key method &allow-other-keys)
  "Override send method, because we just send JSON without HTTP headers."
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((and method (symbolp method)) (symbol-name method)))))
  (let* ((message `(:jsonrpc "2.0" ,@args))
         (json (jsonrpc--json-encode message)))
    (process-send-string
     (jsonrpc--process conn)
     json)))

(cl-defmethod initialize-instance ((conn jieba-node-connection) _slots)
  (cl-call-next-method)
  ;; Set a new process filter for `jieba-node-connection'.
  ;; Because our messages don't contain HTTP headers.
  (let ((proc (jsonrpc--process conn)))
    (when proc
      (set-process-filter proc #'jieba--process-filter))))

(defun jieba--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point))
        (let ((json-message (condition-case-unless-debug oops
                                (jieba--json-read-string string)
                              (error
                               (jsonrpc--warn "Invalid JSON: %s %s"
                                              (cdr oops) (buffer-string))
                               nil)))
              (conn (process-get proc 'jsonrpc-connection)))
          (when json-message
            (with-temp-buffer
              (jsonrpc-connection-receive conn
                                          json-message))))))))

(defun jieba--node-connect ()
  "Connect to our nodejieba server."
  (let* ((name "JIEBA-SERVER")
         (default-directory (jieba--current-dir))
         (conn (jieba-node-connection
                :process (lambda ()
                           (make-process
                            :name name
                            :command jieba-server-start-args
                            :coding 'utf-8-emacs-unix
                            :noquery t
                            :connection-type 'pipe
                            :stderr (get-buffer-create
                                     (format "*%s stderr*" name)))))))
    ;; Ask server to load default dict.
    (jsonrpc-notify conn :hello nil)
    (setq jieba--current-node-conn conn)))

;;; Backend implementation

(cl-defmethod jieba--initialize-backend ((_backend (eql node)))
  (jieba--node-connect))

(cl-defmethod jieba--shutdown-backend ((_backend (eql node)))
  (jsonrpc-shutdown jieba--current-node-conn))

(cl-defmethod jieba--backend-available? ((_backend (eql node)))
  (and (cl-typep jieba--current-node-conn 'jieba-node-connection)
       (jsonrpc-running-p jieba--current-node-conn)))

(cl-defmethod jieba-load-dict ((_backend (eql node)) dicts)
  (jsonrpc-async-request jieba--current-node-conn
                         :loadDict (vconcat dicts)))

(cl-defmethod jieba-do-split ((_backend (eql node)) str)
  (jsonrpc-request jieba--current-node-conn :split str))

(provide 'jieba-node)

;;; jieba-node.el ends here
