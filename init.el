;;; init.el --- My emacs configuration
;;; Commentary:
;;; Code:

;; Packages
(defvar davd33/packages '(;; SHELL
                          exec-path-from-shell
                          (eterm-256color :config
                                          (add-hook 'term-mode-hook #'eterm-256color-mode))
                          (vterm :commands vterm
                                 :config
                                 (setq term-prompt-regexp "davd@davd-tower:.*")
                                 (setq vterm-max-scrollback 10000))
                          ;; QUELPA PACKAGE MANAGER
                          quelpa
                          ;; THEMING
                          (doom-themes :config
                                       ;;(load-theme 'doom-one-light t)
                                       (load-theme 'doom-one t)
                                       ;;(load-theme 'doom-acario-light t)
                                       ;;(load-theme 'doom-Iosvkem t)
                                       (doom-themes-visual-bell-config))
                          ;; UI
                          diminish    ; don't show current minor modes
                          ace-window
                          smart-mode-line-powerline-theme
                          (smart-mode-line :config
                                           (setq sml/theme 'powerline)
                                           (add-hook 'after-init-hook 'sml/setup))
                          ;; EMACS LISP DEV
                          helpful
                          ;; EDITOR
                          jq-mode
                          (expand-region :bind
                                         ("M-m" . er/expand-region))
                          (crux :bind
                                ("C-k" . crux-smart-kill-line)
                                ("C-c n" . crux-cleanup-buffer-or-region)
                                ("C-c f" . crux-recentf-find-file)
                                ("C-a" . crux-move-beginning-of-line))
                          (which-key :diminish which-key-mode
                                     :config
                                     (which-key-mode +1))
                          (avy :bind
                               ("C-," . avy-goto-char-timer)
                               ("C-M-," . avy-goto-char-timer)
                               ("C-M-'" . avy-goto-line)
                               :config
                               (setq avy-background t))
                          (company :diminish company-mode
                                   :config
                                   ;; (add-hook 'after-init-hook #'global-company-mode)
                                   )
                          (auto-complete :init
                                         (require 'auto-complete-config)
                                         (ac-config-default))
                          (flycheck :diminish flycheck-mode
                                    :config
                                    (add-hook 'after-init-hook #'global-flycheck-mode))
                          multiple-cursors
                          ;; NETWORKING
                          (tramp :config (add-to-list 'tramp-default-proxies-alist
                                                      '(".*" "\\`root\\'" "/ssh:%h:")))
                          ;; ORG-MODE
                          (org :config
                               (setq org-src-tab-acts-natively t))
                          htmlize
                          ;; PROJECTS
                          (magit :bind
                                 (("C-M-g" . magit-status)))
                          (projectile :diminish projectile-mode
                                      :bind
                                      (("C-c p f" . helm-projectile-find-file)
                                       ("C-c p p" . helm-projectile-switch-project)
                                       ("C-c p s" . projectile-save-project-buffers)
                                       ("C-c p b" . helm-projectile))
                                      :config
                                      (projectile-mode +1))
                          (helm :defer 2
                                :bind
                                ("M-x" . helm-M-x)
                                ("C-x C-f" . helm-find-files)
                                ("M-y" . helm-show-kill-ring)
                                ("C-x b" . helm-mini)
                                :config
                                (add-hook 'after-init-hook
                                          (lambda ()
                                            (require 'helm-config)
                                            (helm-mode 1)
                                            (setq helm-split-window-inside-p t
                                                  helm-move-to-line-cycle-in-source t)
                                            (setq helm-autoresize-max-height 0)
                                            (setq helm-autoresize-min-height 20)
                                            (helm-autoresize-mode 1)
                                            ;; rebind tab to run persistent action
                                            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
                                            ;; make TAB work in terminal
                                            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
                                            ;; list actions using C-z
                                            (define-key helm-map (kbd "C-z")  'helm-select-action))))
                          (helm-projectile :config
                                           (helm-projectile-on))
                          ag
                          (neotree :config ; change for sidebar (see below, line ~298)
                                   (setq neo-window-fixed-size nil))
                          hydra
                          ;; SIDEBAR
                          dash
                          dash-functional
                          s
                          ov
                          ;; (frame-local :config
                          ;;              ;; FONT LOCK+
                          ;;              (add-to-list 'load-path "/home/mm785/.emacs.d/font-lock+.el")
                          ;;              (require 'font-lock+)
                          ;;              ;; ICONS IN TERMINAL
                          ;;              (add-to-list 'load-path "~/.local/share/icons-in-terminal/")
                          ;;              ;; SIDEBAR
                          ;;              (add-to-list 'load-path "~/projects/sidebar.el/")
                          ;;              (require 'sidebar)
                          ;;              (global-set-key (kbd "C-c x f") 'sidebar-open)
                          ;;              (global-set-key (kbd "C-c x a") 'sidebar-buffers-open))
                          ;; PDF
                          pdf-tools
                          ;; REST
                          (restclient :config
                                      (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))
                          ;; MARKDOWN
                          (markdown-mode :commands (markdown-mode gfm-mode)
                                         :mode (("README\\.md\\'" . gfm-mode)
                                                ("\\.md\\'" . markdown-mode)
                                                ("\\.markdown\\'" . markdown-mode)))
                          ;; JAVA
                          (lsp-java :config
                                    (add-hook 'java-mode-hook #'lsp))
                          ((autodisass-java-bytecode :config-group :java)
                           :defer t)
                          ((google-c-style :config-group :java)
                           :defer t
                           :commands
                           (google-set-c-style))
                          (realgud)
                          ((meghanada :config-group :java)
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
                           (setq indent-tabs-mode nil)
                           (setq tab-width 2)
                           (setq c-basic-offset 2)
                           (setq meghanada-server-remote-debug t)
                           (setq meghanada-javac-xlint "-Xlint:all,-processing")
                           (setq meghanada-java-path "java")
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
                          ;; LISP
                          (smartparens :diminish smartparens-mode
                                       :config
                                       (progn
                                         (require 'smartparens-config)
                                         (smartparens-global-mode 1)
                                         (show-paren-mode t)))
                          paredit
                          (slime :config
                                 (slime-setup '(slime-fancy))
                                 (setq inferior-lisp-program "ros -Q run")
                                 (load (expand-file-name "~/.roswell/helper.el")))
                          ;;slime-company
                          (lispy :config
                                 (let ((run-lispy-mode
                                        (lambda () (lispy-mode 1))))
                                   (add-hook 'emacs-lisp-mode-hook run-lispy-mode)
                                   (add-hook 'lisp-power-mode-hook run-lispy-mode)
                                   (add-hook 'clojure-mode-hook run-lispy-mode)))
                          slime-repl-ansi-color
                          ;; (ac-slime :config
                          ;;           (add-hook 'slime-mode-hook 'set-up-slime-ac)
                          ;;           (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
                          ;;           (add-hook 'common-lisp-mode-hook 'set-up-slime-ac)
                          ;;           (eval-after-load "auto-complete"
                          ;;             '(add-to-list 'ac-modes 'slime-repl-mode)))
                          ;; EMAIL
                          ((mu4e-alert :config-group :email
                                       :after mu4e
                                       :init
                                       (setq mu4e-alert-interesting-mail-query
                                             (concat "flag:unread maildir:/Gmail/INBOX"))
                                       (mu4e-alert-enable-mode-line-display)
                                       (defun gjstein-refresh-mu4e-alert-mode-line ()
                                         (interactive)
                                         (mu4e~proc-kill)
                                         (mu4e-alert-enable-mode-line-display))
                                       (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)
                                       ))
                          ;; JAVASCRIPT
                          ((js2-mode :config-group :js)
                           :config
                           (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
                           (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))
                          ((js2-refactor :config-group :js)
                           :config
                           (add-hook 'js2-mode-hook #'js2-refactor-mode))
                          ((xref-js2 :config-group :js)
                           :config
                           (add-hook 'js2-mode-hook
                                     (lambda ()
                                       (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
                          ((tern :config-group :js)
                           :config
                           (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
                           (eval-after-load 'tern
                             '(progn
                                (require 'tern-auto-complete)
                                (tern-ac-setup))))
                          ((tern-auto-complete :config-group :js))
                          ((tern-context-coloring :config-group :js))
                          ((rjsx-mode :config-group :js)
                           :config
                           (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))
                          ((tide :config-group :js)
                           :config
                           (defun setup-tide-mode ()
                             (interactive)
                             (tide-setup)
                             (flycheck-mode +1)
                             (setq flycheck-check-syntax-automatically '(save mode-enabled))
                             (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
                                                         t
                                                         :placeOpenBraceOnNewLineForFunctions nil
                                                         :placeOpenBraceOnNewLineForControlBlocks nil
                                                         :indenSize 2
                                                         :tabSize 2))
                             (eldoc-mode +1)
                             (tide-hl-identifier-mode +1)
                             ;; company is an optional dependency. You have to
                             ;; install it separately via package-install
                             ;; `M-x package-install [ret] company`
                             (company-mode +1))

                           ;; aligns annotation to the right hand side
                           (setq company-tooltip-align-annotations t)

                           ;; formats the buffer before saving
                           (add-hook 'before-save-hook 'tide-format-before-save)

                           (add-hook 'typescript-mode-hook #'setup-tide-mode))
                          ;; R
                          ((ess :config-group :ess))
                          ((ess-R-data-view :config-group :ess))
                          ((ess-r-insert-obj :config-group :ess))
                          ((ess-smart-equals :config-group :ess))
                          ((ess-smart-underscore :config-group :ess))
                          ((ess-view :config-group :ess))
                          ((ess-view-data :config-group :ess))
                          ;; CLOJURE
                          ((clojure-mode :config-group clojure))
                          ((cider :config-group clojure))
                          ;; MATRIX CHAT
                          ;; (matrix-client
                          ;;  :quelpa (matrix-client :fetcher github :repo "alphapapa/matrix-client.el"
                          ;;                         :files (:defaults "logo.png" "matrix-client-standalone.el.sh")))
                          ))

;; Javascript configuration - a bit old
;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
;; https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html
;; For tern, should read https://ternjs.net/doc/manual.html#emacs

;; It should be possible to disable some packages.
;; Exclude some configuration groups by theme (js, java...).
(defvar davd33/config-exclude '(;; :basics
                                ;; :js
                                :fullscreen
                                :java
                :clojure
                                ;; :email
                                ;; :ess
                                ))

(defmacro davd33/when-config-group (config-group &rest body)
  "Run BODY when CONFIG-GROUP is not found in DAVD33/CONFIG-EXCLUDE."
  (when (not (memq config-group davd33/config-exclude))
    `(progn ,@body)))

;; Local Libs
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'elpa-mirror)
(davd33/when-config-group
 :email
 (setq load-path (append load-path '("/usr/share/emacs/site-lisp/mu4e/")))
 (require 'mu4e))

;; Memory (RAM / files)
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; Package management
(when (member emacs-version '("26.1" "26.2"))
  ;; A bug that impedes the download of packages because of TLS issues.
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(require 'package)
(setq package-enable-at-startup nil)
;(add-to-list 'package-archives '("mymelpa" . "~/projects/emacs-packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(defun davd33/package-config-group (package)
  "Return the value of the config-group of the PACKAGE."
  (when (and (listp package)
             (listp (car package)))
    (cl-getf (cdar package) :config-group)))

(defun davd33/package-name (package)
  "Return the name of a package from a package definition.
PACKAGE: [p-list shaped|symbol] package definition."
  (if (and (listp package) (listp (car package)))
      (caar package)
    (if (listp package)
        (car package)
      package)))

(defun davd33/use-package (package)
  "Call use-package as defined in the given package definition.
PACKAGE: [p-list shaped|symbol] package definition."
  (cl-labels ((gen-use-package-call (p)
                                    (if (listp p)
                                        `(use-package ,(davd33/package-name p)
                                           ,@(cdr p))
                                      `(use-package ,(davd33/package-name p)))))
    (eval (gen-use-package-call package))))

(defun davd33/packages-installed-p (package-list)
  "Return non-nil if all packages in PACKAGE-LIST are installed, return nil otherwise."
  (cl-loop for p in package-list
           when (not (package-installed-p (davd33/package-name p)))
           do (cl-return nil)
           finally (cl-return t)))

(defun davd33/select-packages (package-list excluded-config-groups)
  "Return the filtered PACKAGE-LIST from EXCLUDED-CONFIG-GROUPS."
  (cl-remove-if (lambda (p)
                  (memq (davd33/package-config-group p) excluded-config-groups))
                package-list))

(let ((selected-packages (davd33/select-packages davd33/packages davd33/config-exclude)))
  (unless (davd33/packages-installed-p selected-packages)
    (message "%s" "Refreshing package database...")
    (dolist (p selected-packages)
      (when (not (package-installed-p (davd33/package-name p)))
        (package-install (davd33/package-name p))))))

(cl-loop for p in davd33/packages
         do (davd33/use-package p))

(davd33/when-config-group
 :fullscreen

 (defun fullscreen (&optional f)
   (interactive)
   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                          '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                          '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
 (fullscreen))

(davd33/when-config-group
 :basics
 ;; Visual
 ;;(menu-bar-mode -1)
 (toggle-scroll-bar -1)
 (tool-bar-mode -1)
 (blink-cursor-mode -1)
 (add-to-list 'default-frame-alist '(font . "Fira Code-13"))
 (set-face-attribute 'default t :font "Fira Code-13")

 ;; Helpers
 (global-hl-line-mode +1)
 (line-number-mode +1)
 ;;(global-display-line-numbers-mode 1)
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
 (add-hook 'before-save-hook 'whitespace-cleanup)
 ;; erc auto archive
 (setq erc-log-channels-directory "~/.erc/logs/")
 (setq erc-save-buffer-on-part t)
 ;; (setq erc-hide-timestamps t)
 )

;; Identity
(setq user-full-name "David Rueda"
      user-mail-address "davd33@gmail.com"
      message-signature
      (concat
       "David Rueda\n"
       "http://davd33.org/\n"))

;; EMAIL
(davd33/when-config-group
 :email

 ;; default
 (setq mu4e-maildir (expand-file-name "~/Maildir/"))

 (setq mu4e-drafts-folder "/[Gmail].Drafts")
 (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
 (setq mu4e-trash-folder  "/[Gmail].Trash")

 ;; don't save message to Sent Messages, GMail/IMAP will take care of this
 (setq mu4e-sent-messages-behavior 'delete)

 ;; setup some handy shortcuts
 (setq mu4e-maildir-shortcuts
       '(("/INBOX"             . ?i)
         ("/[Gmail].Sent Mail" . ?s)
         ("/[Gmail].Trash"     . ?t)))

 ;; allow for updating mail using 'U' in the main view:
 (setq mu4e-get-mail-command "offlineimap -o")

 ;;(setq mu4e-mu-home "/home/pi/.mu-cache")

 (setq mu4e-index-cleanup t)
 (setq mu4e-index-lazy-check nil)



 ;; sending mail -- replace USERNAME with your gmail username
 ;; also, make sure the gnutls command line utils are installed
 ;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.

 (require 'smtpmail)
 (setq message-send-mail-function 'smtpmail-send-it
       starttls-use-gnutls t
       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
       smtpmail-auth-credentials
       '(("smtp.gmail.com" 587 "davd33@gmail.com" nil))
       smtpmail-default-smtp-server "smtp.gmail.com"
       smtpmail-smtp-server "smtp.gmail.com"
       smtpmail-smtp-service 587)

 ;; MAIL & ORG-MODE
 (require 'org-mu4e)

 ;; store link to message if in header view, not to header query
 (setq org-mu4e-link-query-in-headers-mode nil)
 )

(setq org-capture-templates
       '(("t" "todo" entry (file+headline "~/todo.org" "Tasks")
          "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

;; XML lint
(defun xml-pretty-print (beg end &optional arg)
  "Reformat the region between BEG and END.
With optional ARG, also auto-fill."
  (interactive "*r\nP")
  (shell-command-on-region beg end "xmllint --format -" (current-buffer)))

;; JAVA
(davd33/when-config-group
 :java

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
   ("z" nil "leave")))

;; eshell
(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

;; redo
(require 'redo+)

;; LISP MODES
(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                                        ;scheme-mode
                                        ;clojure-mode
                   ))

(defvar lisp-power-map (make-keymap))
(define-minor-mode lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap lisp-power-map
  (paredit-mode t))
(define-key lisp-power-map [delete] 'paredit-forward-delete)
(define-key lisp-power-map [backspace] 'paredit-backward-delete)

(defun davd33/engage-lisp-power ()
  "Activate LISP power mode."
  (lisp-power-mode t))

(dolist (mode lisp-modes)
  (add-hook (intern (format "%s-hook" mode))
            #'davd33/engage-lisp-power))

;; proxy
;; (setq url-proxy-services '(("no_proxy" . "localhost")
;;                            ("http" . "http://")
;;                            ("https" . "http://")))

;; Environment PATH
;; (setq exec-path
;;       (append exec-path
;;               (list "/home/davd/bin:$PATH")))

;; GLOBAL CUSTOM KEYBINDINGS
;; Emacs Lisp
(global-set-key (kbd "C-h z f") #'helpful-callable)
(global-set-key (kbd "C-h z v") #'helpful-variable)
(global-set-key (kbd "C-h z k") #'helpful-key)
(global-set-key (kbd "C-h z x") #'helpful-command)
(global-set-key (kbd "C-h z .") #'helpful-at-point)
(global-set-key (kbd "C-h z F") #'helpful-function)
;; move to next window
(global-set-key (kbd "M-o") 'other-window)
;; kill current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;; redo
(global-set-key (kbd "C-?") 'redo)
;; When you have an active region that spans multiple lines,
;; the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; When you want to add multiple cursors not based on
;; continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; open/close tree panel
(global-set-key [f8] 'neotree-toggle)
;; magit grep
(global-set-key (kbd "M-RET g g") 'helm-grep-do-git-grep)
;; pretty print xml
(global-set-key (kbd "M-RET x p p") 'xml-pretty-print)
;; duplicate lines
(global-set-key (kbd "M-RET e d l") 'crux-duplicate-current-line-or-region)
;; email create todo
(davd33/when-config-group
 :email
 (global-set-key (kbd "M-RET m t") 'mu4e-org-store-and-capture)
 (global-set-key (kbd "M-RET m i") 'mu4e-update-index)
 (global-set-key (kbd "M-RET m m") 'mu4e))
;; org mode
(global-set-key (kbd "M-RET o a") 'org-agenda)
;; javascript
(davd33/when-config-group
 :js
 (js2r-add-keybindings-with-prefix "C-c C-r")
 (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
 ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
 ;; unbind it.
 ;; (define-key js-mode-map (kbd "M-.") nil)
 ;; kill line without errors
 ;; Disable completion keybindings, as we use xref-js2 instead
 ;; (define-key tern-mode-keymap (kbd "M-.") nil)
 ;; (define-key tern-mode-keymap (kbd "M-,") nil)
 )

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
   '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default))
 '(erc-autojoin-channels-alist '(("")))
 '(erc-autojoin-mode t)
 '(erc-button-mode t)
 '(erc-fill-mode t)
 '(erc-irccontrols-mode t)
 '(erc-list-mode t)
 '(erc-match-mode t)
 '(erc-menu-mode t)
 '(erc-move-to-prompt-mode t)
 '(erc-netsplit-mode t)
 '(erc-networks-mode t)
 '(erc-noncommands-mode t)
 '(erc-pcomplete-mode t)
 '(erc-readonly-mode t)
 '(erc-ring-mode t)
 '(erc-stamp-mode t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-position-in-mode-line t t)
 '(org-agenda-files '("~/Desktop/Todo.org"))
 '(package-selected-packages
   '(redo+ redo slime-repl-ansi-color slime markdown-mode restclient pdf-tools ace-window helm-projectile multiple-cursors google-c-style autodisass-java-bytecode hydra neotree ag helm projectile magit which-key use-package smartparens smart-mode-line-powerline-theme git-commit flycheck expand-region exec-path-from-shell doom-themes diminish crux avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
