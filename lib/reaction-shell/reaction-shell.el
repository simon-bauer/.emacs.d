;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;
;; external interface

(defun rs-init (shell-name init-cmd-list)
  (interactive)
  (rs-kill-buffer shell-name)
  (let ((shell-hash (make-hash-table :test 'equal)))
    (puthash :init-cmd-list init-cmd-list shell-hash)
    (puthash :initialized nil shell-hash)
    (puthash :mutex (make-mutex) shell-hash)
    (puthash :queue () shell-hash)
    (puthash :shell-busy nil shell-hash)
    (puthash :current-callback 'ignore shell-hash)
    (puthash :current-cmd "" shell-hash)
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
        (unless (rs-get shell-name :initialized)
          (rs-cmd-internal shell-name
                           (mapcar (lambda (cmd-string) (cons cmd-string 'ignore ))
                                   (rs-get shell-name :init-cmd-list)))
          (rs-set shell-name :initialized t))
        (rs-cmd-internal
         shell-name
         (list (cons shell-cmd callback)))))


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
  (with-mutex (rs-get shell-name :mutex)
    (rs-mod shell-name :queue (lambda (old-queue) (append old-queue cmd-pair-list))))
  (rs-trigger shell-name))

(defun rs-trigger (shell-name)
  (let ((queue-empty (not (rs-get shell-name :queue)))
        (shell-busy (rs-get shell-name :shell-busy)))
    (when (and (not queue-empty) (not shell-busy))
      (rs-process-next shell-name))))

(defun rs-process-next (shell-name)
  (with-mutex (rs-get shell-name :mutex)
    (let* ((cmd-callback-pair (car (rs-get shell-name :queue)))
           (cmd (car cmd-callback-pair))
           (callback (cdr cmd-callback-pair)))
      (rs-set shell-name :current-callback callback)
      (rs-set shell-name :current-cmd cmd)
      (rs-mod shell-name :queue 'cdr)
      (rs-set shell-name :shell-busy t)))
  (rs-process-current shell-name))

(defun rs-process-current (shell-name)
  (shell (rs-buffer-name shell-name))
  (add-hook 'comint-output-filter-functions (lambda (string) (rs-output-filter shell-name string) ) t t))

(defun rs-output-filter (shell-name string)
  )

;; hash-table containing for each shell another hash-table with all shell related variables
(setq rs-registry (make-hash-table :test 'equal))

;; access rs-registry
(defun rs-get (shell-name key)
  (gethash key (gethash shell-name rs-registry)))

(defun rs-set (shell-name key value)
  (puthash key value (gethash shell-name rs-registry)))

(defun rs-mod (shell-name key func)
  (rs-set shell-name key (funcall func (rs-get shell-name :queue))))


(provide 'reaction-shell)
