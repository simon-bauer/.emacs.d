;; -*- lexical-binding: t; -*-

;; setup package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package ivy
  :ensure t
  :config  
  (require 'subr-x)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package avy
  :ensure t
  :bind (( "C-;" . avy-goto-char)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package magit
  :ensure t
  :config
  (defadvice server-ensure-safe-dir (around
                                     my-around-server-ensure-safe-dir
                                     activate)
    "Ignores any errors raised from server-ensure-safe-dir"
    (ignore-errors ad-do-it)))

(use-package ag
  :ensure t)

(use-package wgrep-ag
  :ensure t
  :config
  ;; enable editing inline in the ag result buffer
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(use-package clang-format
  :ensure t)

(use-package origami
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package ggtags
  :ensure t
  :config
  ;; ggtags
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(use-package dash
  :ensure t)


;; which key: on the fly docu for key bindings
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; nicer scrolling
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

(use-package smartparens
  :ensure t
  :init
  (use-package smartparens-config))

(use-package whole-line-or-region
  :ensure t
  :init
  ;; delete whole line if no region selected
  (whole-line-or-region-global-mode))

(use-package buttercup
  :ensure t)

;; org
(setq org-startup-indented t)
(setq org-hide-leading-stars t)

;; work specific configuration
(when (file-exists-p "~/.emacs.d/work/init.el")
  (load "~/.emacs.d/work/init.el"))


;; choose a dark theme
(load-theme 'tango-dark t)

(when (string-equal system-type "windows-nt")
  ;; font type and size
  (set-face-attribute 'default nil :font "Consolas-10" )
  (set-frame-font "Consolas-10" nil t))

;; suppress welcome buffer, toolbar and scroolbar
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq c-default-style "bsd"
      c-basic-offset 4)

;; tabs instead of spaces
(setq-default indent-tabs-mode nil)

;; open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(global-display-line-numbers-mode)

;; shorten yes/no answer to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "M-o") 'open-previous-line)


;; switch between windows
(windmove-default-keybindings)

;; highlight matching paren
(show-paren-mode 1)

;; % paren matching
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
	((looking-at "\\s)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))



;; revert without confirmation
(defun revert-buffer-without-confirmation ()
  (interactive)
  (revert-buffer t t))
(global-set-key (kbd "<f5>") 'revert-buffer-without-confirmation)



;; keep history of window configurations
(winner-mode 1)

;; e.g. a yank replaces current region
(delete-selection-mode 1)

;; easier eval-buffer
(global-set-key (kbd "C-c e") 'eval-buffer)

;; prototyping using an org file/buffer as life configuration
(find-file "~/.emacs.d/kauri.org")

(require 'org-element)

;; format: (type properties child1 child2 ...)

(defun kauri-plist (org-ast)
  "convert headlines of an org ast to a nested plist"
  (cond ((eq (car org-ast) 'org-data)
         (kauri-plist-2 (-drop 2 org-ast)))
        ((eq (car org-ast) 'headline)
         (cons
          (plist-get (cadr org-ast) :title)
          (kauri-plist-2 (-drop 2 org-ast))))))

(defun kauri-plist-2 (org-element-list)
  (if org-element-list
      (cons (kauri-plist (car org-element-list))
            (kauri-plist-2 (cdr org-element-list)))
    nil))

(kauri-plist (with-current-buffer "kauri.org"
               (org-element-parse-buffer 'headline)))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (buttercup smartparens yasnippet whole-line-or-region which-key wgrep-ag use-package swiper smooth-scrolling pkg-info pfuture origami magit hydra ht ggtags f expand-region clang-format auto-complete ag ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
