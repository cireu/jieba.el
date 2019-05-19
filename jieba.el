;;; jieba.el  --- Description  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL:
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: chinese

;; This file is NOT a part of GNU Emacs.

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

(require 'eieio)
(require 'jsonrpc)
(require 'thingatpt)

(eval-when-compile
  (require 'pcase))

(defgroup jieba ()
  ""
  :group 'chinese
  :prefix "jieba-")

(defcustom jieba-server-start-args
  (list
   "node"
   (let* ((this-file (cond
                      (load-in-progress load-file-name)
                      ((and (boundp 'byte-compile-current-file)
                            byte-compile-current-file)
                       byte-compile-current-file)
                      (:else (buffer-file-name))))
          (dir (file-name-directory this-file)))
     (expand-file-name "simple_jieba_server.js" dir)))
  ""
  :type 'list
  :group 'jieba)

;;; JSONRPC setup

(defvar jieba--current-connection nil)

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

(defclass jieba-connection (jsonrpc-process-connection) ())

(cl-defmethod jsonrpc-connection-send ((conn jieba-connection)
                                       &rest args
                                       &key method &allow-other-keys)
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((and method (symbolp method)) (symbol-name method)))))
  (let* ((message `(:jsonrpc "2.0" ,@args))
         (json (jsonrpc--json-encode message)))
    (process-send-string
     (jsonrpc--process conn)
     json)))

(cl-defmethod initialize-instance ((conn jieba-connection) _slots)
  (cl-call-next-method)
  (let ((proc (jsonrpc--process conn)))
    (when proc
      (set-process-filter proc #'jieba--process-filter))))

(defun jieba--process-filter (proc string)
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

(defun jieba--connect ()
  (let* ((name "JIEBA-SERVER")
         (conn (jieba-connection
                :process (lambda ()
                           (make-process
                            :name name
                            :command jieba-server-start-args
                            :coding 'utf-8-emacs-unix
                            :noquery t
                            :connection-type 'pipe
                            :stderr (get-buffer-create
                                     (format "*%s stderr*" name)))))))
    (setq jieba--current-connection conn)))

(defun jieba-ensure (&optional force-restart?)
  (interactive "P")
  (if (not (and (cl-typep jieba--current-connection 'jsonrpc-connection)
                (jsonrpc-running-p jieba--current-connection)))
      (jieba--connect)
    (when force-restart?
      (jsonrpc-shutdown jieba--current-connection)
      (jieba--connect))))

;;;

(defsubst jieba-split-chinese-word-sync (str)
  (jsonrpc-request jieba--current-connection :split
                   str))

;;; Export function

(defsubst jieba-chinese-word? (s)
  (string-match-p (format "\\cC\\{%d\\}"
                          (length s)) s))

(defalias 'jieba-chinese-word-p 'jieba-chinese-word?)

;;;###autoload
(defun jieba-chinese-word-atpt-bounds ()
  (let ((word (thing-at-point 'word)))
    (when (and word
               (jieba-chinese-word? word))
      (pcase-let* ((`(,beg . ,end) (bounds-of-thing-at-point 'word))
                   (cur (point))
                   (index beg)
                   (old-index beg))
        (catch 'return
          (mapc (lambda (x)
                  (setq index (+ index (length x)))
                  (cond
                   ((and (>= cur old-index)
                         (< cur index))
                    (throw 'return (cons old-index index)))
                   ((= index end)
                    (throw 'return (cons old-index index)))
                   (t
                    (setq old-index index))))
                (jieba-split-chinese-word-sync word)))))))

(defun jieba--move-chinese-word (backward?)
  (pcase-let* ((`(,beg . ,end) (jieba-chinese-word-atpt-bounds))
               (dest (if backward? beg end))
               (cur (point))
               (arg (if backward? -1 1)))
    (cond
     ((null (or beg end))
      (forward-word arg))
     ((= dest cur)
      (forward-char arg)
      (jieba--move-chinese-word backward?))
     (t
      (goto-char dest)))))

;;;###autoload
(defun jieba-forward-word (arg)
  (interactive "p")
  (let ((backward? (< arg 0)))
    (dotimes (_ (abs arg))
      (jieba--move-chinese-word backward?))))

;;;###autoload
(defun jieba-backward-word (arg)
  (interactive "p")
  (jieba-forward-word (- arg)))

;;; Minor mode

(defvar jieba-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-word] #'jieba-forward-word)
    (define-key map [remap backward-word] #'jieba-backward-word)
    map))

(defun turn-on-jieba-mode ()
  (jieba-mode))

(defun turn-off-jieba-mode ()
  (jieba-mode -1))

;;;###autoload
(define-minor-mode jieba-mode
  ""
  :keymap jieba-mode-map
  :lighter " Jieba")

;;;###autoload
(define-globalized-minor-mode global-jieba-mode jieba-mode turn-on-jieba-mode)

(provide 'jieba)

;; Define text object
(put 'jieba-chinese-word
     'bounds-of-thing-at-point 'jieba-chinese-word-atpt-bounds)

;; Start server
(jieba-ensure t)

;;; jieba.el ends here
