;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;
;; external interface

(provide 'reaction-shell)

(defun rs-init (shell-name init-cmd-list)
  (interactive)
  (rs-kill-buffer shell-name)
  (let ((shell-hash (make-hash-table :test 'equal)))
    (puthash :init-cmd-list init-cmd-list shell-hash)
    (puthash :initialized nil shell-hash)
    (puthash :mutex (make-mutex) shell-hash)
    (puthash :queue () shell-hash)
    (puthash :shell-busy nil shell-hash)
    (puthash :current-output-hook-action 'ignore shell-hash)
    (puthash shell-name shell-hash rs-registry)))

(defun rs-cmd (shell-name shell-cmd callback)
  "The pair of shell-cmd and callback is queued for the given shell.
The shell-cmd is started as soon as the existing cmd pairs in the queue are
finished. The callback is invoced as soon as the corresponding
shell-cmd is finished.
If the given shell is not running it is started and initialized."
  (interactive)
  (let ((buffer-name (rs-buffer-name shell-name)))
        (shell buffer-name)
        (unless (gethash :initialized (gethash shell-name rs-registry))
          (rs-cmd-internal shell-name
           (mapcar (lambda (cmd-string) (cons cmd-string 'ignore ))
                   (gethash :init-cmd-list (gethash shell-name rs-registry))))
          (puthash :initialized t (gethash shell-name rs-registry)))
        (rs-cmd-internal
         shell-name
         (list (cons shell-cmd callback)))))

;; internal mechanism:
;; - each shell-command is appended the insertion of an end marker
;; - before each shell-command the action-hook of the buffer is set to the callback
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


(defun rs-kill-buffer (shell-name)
  ;; remove confirmation for process buffers
  (setq kill-buffer-query-functions
	(delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  (let ((buffer-name (rs-buffer-name shell-name)))
        (when (get-buffer buffer-name)
            (kill-buffer buffer-name))))

(defun rs-buffer-name (shell-name)
  (concat "*" shell-name "*"))

(defun rs-cmd-internal (shell-name cmd-pair-list)
  (with-mutex (gethash :mutex (gethash shell-name rs-registry))
    (puthash :queue 
             (append (gethash :queue (gethash shell-name rs-registry)) cmd-pair-list)
             (gethash shell-name rs-registry)))
  (rs-trigger shell-name))

(defun rs-trigger (shell-name)
  (let ((queue-empty (not (gethash :queue (gethash shell-name rs-registry))))
        (shell-busy (gethash :shell-busy (gethash shell-name rs-registry))))
    (when (and (not queue-empty) (not shell-busy))
      (rs-process-next shell-name))))

(defun rs-process-next (shell-name)
  )

;; hash-table containing for each shell another hash-table with all shell related variables
(setq rs-registry (make-hash-table :test 'equal))
