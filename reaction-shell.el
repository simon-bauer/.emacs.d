;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;
;; external interface

(provide 'reaction-shell)

(defun make-reaction-shell (shell-name-string init-cmd-string-list)
  (interactive)
  (kill-reaction-shell-buffer-if-exist shell-name-string)
  (let ((shell-hash (make-hash-table :test 'equal)))
    (puthash :init-cmd-string-list init-cmd-string-list shell-hash)
    (puthash :initialized nil shell-hash)
    (puthash :mutex (make-mutex) shell-hash)
    (puthash :queue () shell-hash)
    (puthash :shell-busy nil shell-hash)
    (puthash :current-output-hook-action (lambda ()) shell-hash)
    (puthash shell-name-string shell-hash reaction-shell-registry)))

(defun reaction-shell-queue-cmd (shell-name-string shell-cmd-string reaction-function)
  "The pair of shell-cmd and reaction-function is queued for the given shell.\
The shell-cmd is started as soon as the existing cmd pairs in the queue are\
finished. The reaction-function is invoced as soon as the corresponding\
shell-cmd is finished.\
If the given shell is not running it is started and initialized."
  (interactive)
  (let ((buffer-name (reaction-shell-buffer-name shell-name-string)))
        (shell buffer-name)
        (unless (gethash :initialized (gethash shell-name-string reaction-shell-registry))
          (reaction-shell-queue-cmd-pairs-internal shell-name-string
           (mapcar (lambda (cmd-string) (cons cmd-string 'ignore ))
                   (gethash :init-cmd-string-list (gethash shell-name-string reaction-shell-registry))))
          (puthash :initialized t (gethash shell-name-string reaction-shell-registry)))
        (reaction-shell-queue-cmd-pairs-internal
         shell-name-string
         (list (cons shell-cmd-string reaction-function)))))

;; internal mechanism:
;; - each shell-command is appended the insertion of an end marker
;; - before each shell-command the action-hook of the buffer is set to the reaction-function
;; - before an reaction function the actino-hook is set to an empty function
;; - after an reaction function a cmd-pair-finished trigger is activated
;; - changes on state :queue/:shell-busy must be protected by mutex
 
;; - states:
;;          |
;;          | cmd-pair is finished
;;          v
;;   - queue empty, shell not busy
;;          |
;;          | cmd-pair is queued
;;          v
;;   - queue has entry, shell not busy
;;          |
;;          | cmd-pair is taken from queue and given to shell
;;          v
;;   - queue empty, shell busy
;;          |
;;          | cmd-pair is queued
;;          v 
;;   - queue has entry, shell busy
;;          |
;;          | cmd-pair is finished
;;          v
;;   - queue has entry, shell not busy
;;          |
;;          | cmd-pair is taken from queue and given to shell
;;          v
;;   - queue empty, shell busy
;;          |
;; - main trigger:
;;   - cmd-pair-queued
;;   - cmd-pair-finished



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal functions and data


(defun kill-reaction-shell-buffer-if-exist (shell-name-string)
  ;; remove confirmation for process buffers
  (setq kill-buffer-query-functions
	(delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  (let ((buffer-name (reaction-shell-buffer-name shell-name-string)))
        (when (get-buffer buffer-name)
            (kill-buffer buffer-name))))

(defun reaction-shell-buffer-name (shell-name-string)
  (concat "*" shell-name-string "*"))

(defun reaction-shell-queue-cmd-pairs-internal (shell-name-string cmd-pair-list)
  (with-mutex (gethash :mutex (gethash shell-name-string reaction-shell-registry))
    (puthash :queue 
             (append (gethash :queue (gethash shell-name-string reaction-shell-registry)) cmd-pair-list)
             (gethash shell-name-string reaction-shell-registry))))


;; hash-table containing for each shell another hash-table with all shell related variables
(setq reaction-shell-registry (make-hash-table :test 'equal))
