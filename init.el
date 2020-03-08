;;; init.el --- My emacs configuration
;;; Commentary:
;;; Code:

;; Local Libs
(add-to-list 'load-path "~/.emacs.d/lisp")

;; My identity
(setq user-full-name "David Rueda"
      user-mail-address "davd33@gmail.com")

;; Memory (RAM / files)
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; Package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; shell environment
(use-package exec-path-from-shell
  :ensure t
  :demand t)

;; Visual
;(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

;; Helpers
(global-hl-line-mode +1)
(line-number-mode +1)
;(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)

;; no start-up page
(setq inhibit-startup-screen t)

;; show full path of current file
(setq frame-title-format
      '((:eval (if (buffer-file-name)
           (abbreviate-file-name (buffer-file-name))
         "%b"))))

;; scrolling improvements
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; theming - visual-studio-like
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))

;; smart mode line - it's sexy
(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'powerline)
  (add-hook 'after-init-hook 'sml/setup))

;; give a home to emacs' backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; be lazy - y/n - tabs - auto update - kill this buffer - noisy whitespaces
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(setq-default tab-width 4
              indent-tabs-mode nil)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; don't show current minor modes
(use-package diminish
         :ensure t)

;; smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

;; select expand region
(use-package expand-region
  :ensure t
  :bind ("M-m" . er/expand-region))

;; better defaults
(use-package crux
  :ensure t
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-c n" . crux-cleanup-buffer-or-region)
  ("C-c f" . crux-recentf-find-file)
  ("C-a" . crux-move-beginning-of-line))

;; which key?
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

;; avy - goto char
(use-package avy
  :ensure t
  :bind
  ("C-=" . avy-goto-char)
  :config
  (setq avy-background t))

;; company - autocomplete
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; org mode
(use-package org
  :ensure t
  :config
  (setq org-src-tab-acts-natively t))

;; git - magit
(use-package magit
  :ensure t
  :demand t
  :bind
  (("C-M-g" . magit-status))
  :init
  (global-set-key (kbd "C-c g g") 'helm-grep-do-git-grep))

;; projects - projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers)
   ("C-c p b" . helm-projectile))
  :config
  (projectile-mode +1))

;; helm
(use-package helm
  :ensure t
  :defer 2
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
    helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  )

;; ag search
(use-package ag
  :ensure t
  :demand t)

;; tree view
(use-package neotree
  :ensure t
  :demand t
  :config
  (setq neo-window-fixed-size nil)
  (global-set-key [f8] 'neotree-toggle))

;; hydra
(use-package hydra
  :ensure t
  :demand t)

;; sidebar - dependencies
;; (add-to-list 'load-path "/home/mm785/.emacs.d/font-lock+.el")
;; (require 'font-lock+)
;; (use-package dash
;;   :ensure t)
;; (use-package dash-functional
;;   :ensure t)
;; (use-package s
;;   :ensure t)
;; (use-package ov
;;   :ensure t)
;; (use-package frame-local
;;   :ensure t)

;; sidebar
;; (add-to-list 'load-path "~/.local/share/icons-in-terminal/") ;; If it's not already done
;; (add-to-list 'load-path "~/projects/sidebar.el/")
;; (require 'sidebar)
;; (global-set-key (kbd "C-c x f") 'sidebar-open)
;; (global-set-key (kbd "C-c x a") 'sidebar-buffers-open)

;; XML lint
(defun xml-pretty-print (beg end &optional arg)
  "Reformat the region between BEG and END.
With optional ARG, also auto-fill."
  (interactive "*r\nP")
  (shell-command-on-region beg end "xmllint --format -" (current-buffer)))
(global-set-key (kbd "C-c x f") 'xml-pretty-print)

;; java
(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

(use-package meghanada
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (google-set-c-style)
              (google-make-newline-indent)
              (meghanada-mode t)
              (smartparens-mode t)
              (rainbow-delimiters-mode t)
              (highlight-symbol-mode t)
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

  :config
  (use-package realgud
    :ensure t)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  ;;(setq meghanada-java-path "java")
  (setq meghanada-java-path "/home/mm785/apps/idea-IU-193.6015.39/jbr/bin/java")
  ;;(setq meghanada-java-path "/home/mm785/apps/jdk1.8.0_241/bin/java")
  ;; (setq meghanada-jvm-option "")
  (setq meghanada-maven-path "mvn")
  :bind
  (:map meghanada-mode-map
        ("C-S-t" . meghanada-switch-testcase)
        ("M-RET" . meghanada-local-variable)
        ("C-M-." . helm-imenu)
        ("M-r" . meghanada-reference)
        ("M-t" . meghanada-typeinfo)
        ("C-z" . hydra-meghanada/body))
  :commands
  (meghanada-mode))

(defhydra hydra-meghanada (:hint nil :exit t)
"
^Edit^                           ^Tast or Task^
^^^^^^-------------------------------------------------------
_f_: meghanada-compile-file      _m_: meghanada-restart
_c_: meghanada-compile-project   _t_: meghanada-run-task
_o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
_s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
_v_: meghanada-local-variable    _R_: meghanada-run-junit-recent
_i_: meghanada-import-all        _r_: meghanada-reference
_g_: magit-status                _T_: meghanada-typeinfo
_l_: helm-ls-git-ls
_q_: exit
"
  ("f" meghanada-compile-file)
  ("m" meghanada-restart)

  ("c" meghanada-compile-project)
  ("o" meghanada-optimize-import)
  ("s" meghanada-switch-test-case)
  ("v" meghanada-local-variable)
  ("i" meghanada-import-all)

  ("g" magit-status)
  ("l" helm-ls-git-ls)

  ("t" meghanada-run-task)
  ("T" meghanada-typeinfo)
  ("j" meghanada-run-junit-test-case)
  ("J" meghanada-run-junit-class)
  ("R" meghanada-run-junit-recent)
  ("r" meghanada-reference)

  ("q" exit)
  ("z" nil "leave"))

;; multiple cursors
(use-package multiple-cursors
  :demand t
  :ensure t
  :config
  ;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  ;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c t j") 'set-justification-full))

;; duplicate lines
(global-set-key (kbd "C-c M-d") 'crux-duplicate-current-line-or-region)

;; helm - projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;; ace window
(use-package ace-window
  :ensure t
  :init (global-set-key (kbd "M-o") 'ace-window))

;; pdf tools
(use-package pdf-tools
  :ensure t)

;; eshell
(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

;; REST client
(use-package restclient
  :ensure t
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;:init (setq markdown-command ".../pandoc.exe")
  )

;; redo
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)

;; COMMON LISP - SLIME
(use-package slime
  :ensure t
  :config
  ;; ROSWELL
  (setq inferior-lisp-program "ros -Q run")
  (load (expand-file-name "~/.roswell/helper.el"))
  ;(setq inferior-lisp-program "sbcl")
  ;(load (expand-file-name "~/quicklisp/slime-helper.el"))
  )
(use-package slime-company
  :ensure t)
(use-package slime-repl-ansi-color
  :ensure t)

;; proxy
;; (setq url-proxy-services '(("no_proxy" . "localhost")
;;                            ("http" . "http://")
;;                            ("https" . "http://")))

;; tramp | sudo on remote host
(use-package tramp
  :ensure t
  :config (add-to-list 'tramp-default-proxies-alist
                       '(".*" "\\`root\\'" "/ssh:%h:")))

;; Environment PATH
;; (setq exec-path
;;       (append exec-path
;;               (list "/home/davd/apps/apache-activemq-5.15.8/bin:$PATH")))

;; daemon mode
(require 'server)
(if (not (server-running-p)) (server-start))

;; END OF CUSTOM CONFIG
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(package-selected-packages
   (quote
    (redo+ redo slime-repl-ansi-color slime-company slime markdown-mode restclient pdf-tools ace-window helm-projectile multiple-cursors google-c-style autodisass-java-bytecode hydra neotree ag helm projectile magit which-key use-package smartparens smart-mode-line-powerline-theme git-commit flycheck expand-region exec-path-from-shell doom-themes diminish crux company avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
