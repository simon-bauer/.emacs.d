;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;
;; external interface



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal functions and data



(provide 'kauri)

(defun kauri-config-is-root (org-element)
  (eq (car org-element) 'org-data))

(defun kauri-config-is-key (org-element)
  (and (eq (car org-element) 'headline)
       (> (length org-element) 2)))

(defun kauri-config-is-value (org-element)
  (and (eq (car org-element) 'headline)
       (equal (length org-element) 2)))

(defun kauri-config-get-key (org-element)
  (plist-get (cadr org-element) :title))

(defun kauri-config-get-value (org-element)
  (kauri-config-get-key org-element))

(defun kauri-config-plist (org-ast)
  "convert headlines of an org ast to a nested plist"
  (cond ((kauri-config-is-root org-ast)
         (kauri-config-plist-2 (cddr org-ast)))
        ((kauri-config-is-key org-ast)
         (cons
          (kauri-config-get-key org-ast)
          (kauri-config-plist-2 (cddr org-ast))))
        ((kauri-config-is-value org-ast)
         (kauri-config-get-value org-ast))))

(defun kauri-config-plist-2 (org-child-list)
  (if org-child-list
      (kauri-config-plist (car org-child-list))
;;      (cons (kauri-config-plist (car org-child-list))
;;            (kauri-config-plist-2 (cdr org-child-list)))
    nil))
