;;; jieba.el  --- Use nodejieba chinese segmentation in Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL: https://github.com/cireu/jieba.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2") (jsonrpc "1.0.7"))
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

;; This package use JSONRPC protocol to contact with a simple wrapper
;; of nodejieba, A chinese word segmentation tool.

;;; Code:

(require 'eieio)
(require 'jsonrpc)
(require 'thingatpt)

(eval-when-compile
  (require 'cl-lib))

;;; Customize

(defgroup jieba ()
  ""
  :group 'chinese
  :prefix "jieba-")

(defcustom jieba-server-start-args
  `("node" "simple-jieba-server.js")
  ""
  :type 'list
  :group 'jieba)

(defcustom jieba-split-algorithm 'mix
  ""
  :type '(choice (const :tag "MP Segment Algorithm" mp)
                 (const :tag "HMM Segment Algorithm" hmm)
                 (const :tag "Mix Segment Algorithm" mix)))

;;; JSONRPC Setup

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

(defclass jieba-connection (jsonrpc-process-connection) ()
  "A connection based on stdio to contact with jieba server.")

(cl-defmethod jsonrpc-connection-send ((conn jieba-connection)
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

(cl-defmethod initialize-instance ((conn jieba-connection) _slots)
  (cl-call-next-method)
  ;; Set a new process filter for `jieba-connection'.
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

(defun jieba--current-dir ()
  (let* ((this-file (cond
                     (load-in-progress load-file-name)
                     ((and (boundp 'byte-compile-current-file)
                           byte-compile-current-file)
                      byte-compile-current-file)
                     (t (buffer-file-name))))
         (dir (file-name-directory this-file)))
    dir))

(defun jieba--connect ()
  (let* ((name "JIEBA-SERVER")
         (default-directory (jieba--current-dir))
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
    ;; Ask server to load default dict.
    (jsonrpc-notify conn :hello nil)
    (setq jieba--current-connection conn)))

(defun jieba-ensure (&optional interactive-restart?)
  (interactive "P")
  (if (not (and (cl-typep jieba--current-connection 'jsonrpc-connection)
                (jsonrpc-running-p jieba--current-connection)))
      (jieba--connect)
    (when (and
           interactive-restart?
           (y-or-n-p "Jieba server is running now, do you want to restart it?"))
      (jsonrpc-shutdown jieba--current-connection)
      (jieba--connect))))

(defun jieba--assert-server (&optional interactive-start?)
  "Assert the server is running, throw an error when assertion failed.

If INTERACTIVE-START? is non-nil, will ask user to start server first."
  (when (not (and (cl-typep jieba--current-connection 'jieba-connection)
                  (jsonrpc-running-p jieba--current-connection)))
    (cond
     ((and interactive-start?
           (y-or-n-p "Do you want to start jieba server?"))
      (jieba-ensure))
     (t
      (error "[JIEBA] Jieba server is not running now!")))))

;;; Data Cache

(defvar jieba--cache (make-hash-table :test #'equal))

(defun jieba--cache-gc ())

;;;

(defun jieba-load-dict (dicts)
  (jsonrpc-async-request jieba--current-connection :loadDict
                         (vconcat dicts)
                         :success-fn (lambda (result)
                                       (message "[JIEBA] %s" result))
                         :error-fn (cl-function
                                    (lambda (&key message data &allow-other-keys)
                                      (error "[JIEBA] Remote Error: %s %s"
                                             message data)))))

;;; Export function

(defvar jieba--single-chinese-char-re "\\cC")

(defun jieba-split-chinese-word (str)
  (let* ((not-found (make-symbol "hash-not-found"))
         (result (gethash str jieba--cache not-found)))
    (if (eq result not-found)
        (prog1
            (setq result (jsonrpc-request
                          jieba--current-connection :split
                          str))
          (puthash str result jieba--cache))
      result)))

(defsubst jieba-chinese-word? (s)
  "Return t when S is a real chinese word (All its chars are chinese char.)"
  (and (string-match-p (format "%s\\{%d\\}"
                               jieba--single-chinese-char-re
                               (length s)) s)
       t))

(defalias 'jieba-chinese-word-p 'jieba-chinese-word?)

;;;###autoload
(defun jieba-chinese-word-atpt-bounds ()
  (jieba--assert-server)
  (pcase (bounds-of-thing-at-point 'word)
    (`(,beg . ,end)
     (let ((word (buffer-substring-no-properties beg end)))
       (when (jieba-chinese-word? word)
         (let ((cur (point))
               (index beg)
               (old-index beg))
           (cl-block retval
             (mapc (lambda (x)
                     (cl-incf index (length x))
                     (cond
                      ((or (< old-index cur index)
                           (= old-index cur))
                       (cl-return-from retval (cons old-index index)))
                      ((= index end)
                       (cl-return-from retval (cons old-index index)))
                      (t
                       (setq old-index index))))
                   (jieba-split-chinese-word word)))))))))


(defun jieba--move-chinese-word (backward?)
  (cl-labels ((find-dest
               (backward?)
               (pcase (jieba-chinese-word-atpt-bounds)
                 (`(,beg . ,end)
                  (if backward? beg end))))

              (try-backward-move
               (backward?)
               (let (pnt beg)
                 (save-excursion
                   (if backward? (backward-char) (forward-char))
                   (setq pnt (point))
                   (setq beg (find-dest backward?)))
                 (goto-char pnt)
                 (when (or (null beg)
                           (not (= beg pnt)))
                   (jieba--move-chinese-word backward?)))))

    (let* ((dest (find-dest backward?))
           (cur (point)))
      (cond
       ((null dest)
        (if backward?
            (if (looking-back jieba--single-chinese-char-re
                              (car (bounds-of-thing-at-point 'word)))
                (try-backward-move backward?)
              (backward-word))
          (forward-word)))
       ((= dest cur)
        (try-backward-move backward?))
       (t
        (goto-char dest))))))

;;;###autoload
(defun jieba-forward-word (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (let ((backward? (< arg 0)))
    (dotimes (_ (abs arg))
      (jieba--move-chinese-word backward?))))

;;;###autoload
(defun jieba-backward-word (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (jieba-forward-word (- arg)))

;;;###autoload
(defun jieba-kill-word (arg)
  (interactive "p")
  (kill-region (point) (progn (jieba-forward-word arg) (point))))

;;;###autoload
(defun jieba-backward-kill-word (arg)
  (interactive "p")
  (jieba-kill-word (- arg)))

;;;###autoload
(defun jieba-mark-word ()
  (interactive)
  (end-of-thing 'jieba-chinese-word)
  (set-mark (point))
  (beginning-of-thing 'jieba-chinese-word))

;;; Minor mode

;;;###autoload
(defvar jieba-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-word] #'jieba-forward-word)
    (define-key map [remap backward-word] #'jieba-backward-word)
    (define-key map [remap kill-word] #'jieba-kill-word)
    (define-key map [remap backward-kill-word] #'jieba-backward-kill-word)
    map))

;;;###autoload
(define-minor-mode jieba-mode
  ""
  :global t
  :keymap jieba-mode-map
  :lighter " Jieba"
  (when jieba-mode (jieba-ensure t)))

(provide 'jieba)

;; Define text object
(put 'jieba-chinese-word
     'bounds-of-thing-at-point 'jieba-chinese-word-atpt-bounds)

;;; jieba.el ends here
