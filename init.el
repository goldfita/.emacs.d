;;; -*- coding: utf-8; lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Do this before everything else in case something goes wrong while loading
;; init. I don't like having to contort my fingers to reach for C-x.
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)
(define-key key-translation-map (kbd "<XF86Tools>") (kbd "M-SPC"))  ; https://github.com/microsoft/wslg/issues/1068
(define-key key-translation-map (kbd "M-t")   (kbd "M-x"))
(define-key key-translation-map (kbd "M-x")   (kbd "M-t"))
(define-key key-translation-map (kbd "C-M-x") (kbd "C-M-t"))
(define-key key-translation-map (kbd "C-M-t") (kbd "C-M-x"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization and basic support packages

(setq gc-cons-threshold (* 512 1000 1000))  ; shrink this at the end of init
(setq tg/vertico-center-height 35
      tg/win-root-path         (or (getenv "WIN_PATH") "C:")
      tg/host-root-path        (or (getenv "HOST_PATH") "/")
      tg/host-home-path        (when tg/host-root-path (expand-file-name (concat "home/" tg/user) tg/host-root-path))
      tg/win-deps-path         (or (getenv "WIN_DEPS_PATH") "C:/software")
      tg/using-windows         (memq system-type '(windows-nt ms-dos))
      tg/using-wsl             (getenv "WSL_DISTRO_NAME")
      tg/git-directories       `((,tg/home-path . 1))
      tg/git-path              (expand-file-name "git" tg/win-deps-path)
      tg/path-separator        (if tg/using-windows ";" ":")
      tg/fonts-path            (expand-file-name ".fonts" user-emacs-directory)
      tg/highlight-face        (defface tg/hi-minibuffer '((t (:background "#355E3B")))
                                 "Face for hi-lock mode."
                                 :group 'display))

;; Bootstrap use-package
(require 'package)
(let* ((no-ssl (and tg/using-windows (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/"))))
(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 40)
                          ("melpa"  . 30)
                          ("nongnu" . 20)))
(unless tg/using-windows
  (customize-set-variable 'package-user-dir (expand-file-name ".emacs.d/elpa" tg/host-home-path)))
(package-initialize)
(require 'use-package)
(customize-set-variable 'use-package-always-ensure t)
;; https://github.com/jwiegley/use-package/issues/256
(advice-add 'package-install
            :before
            (let ((done nil))
              (lambda (&args)
                (when (not done)
                  (message "Refreshing contents from package-install")
                  (package-refresh-contents)
                  (setq done t)))))
(unless (or (package-installed-p 'gnu-elpa-keyring-update) tg/using-windows)
  (use-package gnu-elpa-keyring-update)
  (use-package gnutls
    :config
    (add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem")))

(use-package page-break-lines
  :diminish
  :custom
  (page-break-lines-modes '(text-mode prog-mode special-mode fundamental-mode))
  :config
  (global-page-break-lines-mode))
(use-package cl-lib)
(use-package diminish)
(use-package bind-key)
(use-package hydra)
(use-package f)
(use-package which-key
  :after diminish
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; Backup/lock
(setopt tg/aux-path                    (f-join user-emacs-directory "aux/")
        backup-directory-alist         `((".*" . ,tg/aux-path))
        auto-save-file-name-transforms `(("\\`/.*/\\([^/]+\\)\\'" ,(concat tg/aux-path "\\1") t))
        lock-file-name-transforms      `(("\\`/.*/\\([^/]+\\)\\'" ,(concat tg/aux-path "\\1") t))
        auto-save-list-file-prefix     tg/aux-path)

(if tg/using-wsl
    (progn
      (load "~/.emacs.d/wsl-path.el")
      (wsl-path-activate)
      (startup-redirect-eln-cache (f-join tg/host-home-path ".emacs.d/eln-cache")))
  (defun wsl-path-convert-to-win (path) path)
  (defun wsl-path-convert-to-linux (path) path))

;; M-x use-package-report with the following to see package load/init info
(let* ((root-path (if tg/using-windows (f-join tg/win-deps-path "emacs") "/usr/local/share/emacs/"))
       (ver-path (f-join root-path emacs-version)))
  (setopt source-directory               root-path
          configure-info-directory       (if tg/using-windows
                                             (f-join root-path "share/info")
                                           "/usr/localto/share/info/")
          doc-directory                  (f-join ver-path "etc/")
          data-directory                 (f-join ver-path "etc/")
          tutorial-directory             (f-join ver-path "etc/tutorials/")
          widget-image-directory         (f-join ver-path "etc/images/")
          lisp-directory                 (f-join ver-path "lisp/")
          ;;package-directory-list         (list (expand-file-name "site-lisp/elpa" ver-path))
          custom-file                    null-device
          default-directory              "~/"
          warning-minimum-level          :error
          use-package-compute-statistics t))

;; Add basic unix commands to path
(when (file-directory-p tg/git-path)
  (let ((bin-path (f-join tg/git-path "usr/bin")))
    (setenv "PATH" (concat bin-path tg/path-separator (getenv "PATH")))
    (add-to-list 'exec-path bin-path tg/git-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
(use-package vertico
  ;; :bind
  ;; ("M-G" . #'vertico-multiform-grid)
  ;; ("M-V" . #'vertico-multiform-vertical)
  :custom
  (vertico-count  20)
  (vertico-resize nil)
  (vertico-cycle  t)
  (vertico-multiform-commands
   `((find-file posframe grid (vertico-count . ,tg/vertico-center-height))
     (consult-projectile-find-file posframe (vertico-count . ,tg/vertico-center-height) (vertico-posframe-width . 120))
     (t (:not posframe))))
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode)
  (savehist-mode)
  (vertico-mouse-mode))
(use-package vertico-grid
  :after vertico
  :ensure nil)
(use-package marginalia
  :config
  (defun tg/marginalia-annotate-buffer (cand)
    "Highlight unsaved buffers."
    (let ((buffer (get-buffer cand))
          (annotated-buf (marginalia-annotate-buffer cand)))
      (when buffer
        (when (and (buffer-file-name buffer) (buffer-modified-p buffer))
          (add-face-text-property 0 (length annotated-buf) tg/highlight-face t annotated-buf))
        annotated-buf)))
  (add-to-list 'marginalia-annotator-registry '(buffer tg/marginalia-annotate-buffer))
  (marginalia-mode))
(use-package orderless)
(use-package consult
  :after orderless
  :custom-face
  (consult-highlight-match ((t :inherit orderless-match-face-0)))
  :custom
  (consult-narrow-key             (kbd ";"))
  (completion-in-region-function  'consult-completion-in-region)
  (xref-show-xrefs-function       'consult-xref)
  (xref-show-definitions-function 'consult-xref)
  (consult-preview-key            '(:debounce 0.4 any))
  (consult-project-function       (lambda (_) (projectile-project-root)))
  (consult-async-split-style      nil)
  :bind
  ("C-c i"   . consult-imenu)
  ("C-x C-r" . consult-recent-file)
  ("C-x r b" . consult-bookmark)
  ([remap switch-to-buffer]            . consult-buffer)
  ([remap projectile-switch-to-buffer] . consult-projectile-switch-to-buffer)
  ([remap goto-line]                   . consult-goto-line)
  ([remap yank-pop]                    . consult-yank-pop)
  :init
  ;; https://github.com/jwiegley/use-package/issues/413
  (with-eval-after-load 'projectile
    (bind-keys
     ([remap projectile-switch-project]   . consult-projectile-switch-project)
     ([remap projectile-switch-to-buffer] . consult-projectile-switch-to-buffer)
     ([remap projectile-find-file]        . consult-projectile-find-file)
     :map projectile-command-map
     ("s"                                 . consult-ripgrep)))
  :config
  (consult-customize
   consult-ripgrep consult-buffer consult-recent-file consult-bookmark ;consult-git-log-grep
   :preview-key (list :debounce 0.4 "<up>" "<down>" "M-."))
  (autoload 'projectile-project-root "projectile"))
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))
(use-package corfu
  :custom
  (corfu-auto           t)
  (corfu-auto-prefix    3)
  (corfu-auto-delay     0.75)
  (corfu-quit-no-match  'separator)
  (corfu-min-width      80)
  (corfu-max-width      corfu-min-width)
  (corfu-cycle          t)
  (corfu-on-exact-match 'quit)
  ;;(corfu-preselect-first nil)
  (lsp-completion-provider :none)
  :bind
  (:map corfu-map
        (("C-n"      . corfu-next)
         ("C-p"      . corfu-previous)
         ("C-g"      . corfu-quit)
         ("<escape>" . corfu-quit)
         ("<return>" . corfu-insert)))
  :init
  (global-corfu-mode))

(defun tg/basic-remote-try-completion (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-try-completion string table pred point)))
(defun tg/basic-remote-all-completions (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-all-completions string table pred point)))
(setq completion-styles-alist
      (append
       '((basic-remote tg/basic-remote-try-completion tg/basic-remote-all-completions nil))
       completion-styles-alist
       (mapcar
        (lambda (completion-style)
          (let* ((completions-symbol-string (symbol-name (nth 2 completion-style)))
                 (completions-symbol-prefix-string
                  (progn
                    (string-match  "\\(.+\\)-all" completions-symbol-string)
                    (match-string 1 completions-symbol-string)))
                 (completion-func (intern (concat completions-symbol-prefix-string "-nonexact-all-completions"))))
            (fset completion-func
                  (lambda (string table pred point)
                    (let* ((results (funcall (nth 2 completion-style) string table pred point))
                           (end (last results))
                           (base (cdr end)))
                      (when results
                        (setcdr end nil)
                        (when (or (minibufferp)
                                  (bound-and-true-p corfu--input)
                                  (not (member string results)))
                          (setcdr end base)
                          results)))))
            (list (intern (concat "nonexact-" (symbol-name (car completion-style))))
                  (nth 1 completion-style)
                  completion-func
                  (nth 3 completion-style))))
        completion-styles-alist)))
(setopt completion-styles                '(nonexact-orderless nonexact-basic)
        completion-category-overrides    '((file (styles basic-remote partial-completion)))
        completions-detailed             t
        hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          ;; try-expand-dabbrev-from-kill
          ;; try-expand-all-abbrevs
          ;; try-expand-list
          ;; try-expand-line
          ))
        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))
(use-package eldoc
  :after diminish
  :diminish
  :ensure nil)
(use-package atomic-chrome :disabled) ; atomic-chrome-start-server
(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-default-method (if tg/using-windows "plink" "ssh"))
  (tramp-default-host (car tg/remote-hosts))
  ;; https://emacs.stackexchange.com/questions/26560/bookmarking-remote-directories-trampsudo
  ;;(tramp-save-ad-hoc-proxies t)
  :config
  ;;(modify-coding-system-alist 'process "plink" 'utf-8-unix)
  ;; https://stackoverflow.com/questions/2177687/open-file-via-ssh-and-sudo-with-emacs
  (add-to-list 'tramp-default-user-alist
               (list tramp-default-method nil tg/remote-user))
  (add-to-list 'tramp-default-user-alist '("root" nil "root"))
  (dolist (elem tg/remote-hosts)
    (add-to-list 'tramp-default-proxies-alist
                 `(,(concat "^" elem "$")
                   "^root$"
                   ,(concat "/" tramp-default-method ":" elem ":"))))
  ;; https://stackoverflow.com/questions/71957101/plink-sudo-in-emacs-connection-without-password
  (add-to-list 'tramp-methods
               `("root"
    	         (tramp-login-program        "sudo")
                 (tramp-login-args           (("su") ("-" "%u")))
                 (tramp-remote-shell         ,tramp-default-remote-shell)
                 (tramp-remote-shell-login   ("-l"))
                 (tramp-remote-shell-args    ("-c"))
                 (tramp-connection-timeout   10)
                 (tramp-session-timeout      300))))
(use-package switch-window
  :disabled ; https://github.com/dimitri/switch-window/issues/87
  :custom
  (switch-window-shortcut-style      'qwerty)
  (switch-window-minibuffer-shortcut ?z)
  :bind
  ([remap other-window] . switch-window)
  :config
  (set-face-attribute 'switch-window-label nil :height 20.0))
(use-package embark
  :disabled t
  :bind
  (("C-."   . embark-act)
   ("M-."   . embark-dwim)
   ("C-b"   . embark-become)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))
  (defun embark-which-key-indicator ()
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  (defun embark-hide-which-key-indicator (fn &rest args)
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(setopt ;;gc-cons-threshold               (* 128 1024 1024)
        read-process-output-max         (* 1024 1024)
        initial-scratch-message         nil
        hl-line-sticky-flag             nil
        scroll-preserve-screen-position nil
        cursor-in-non-selected-windows  nil
        read-minibuffer-restore-windows t
        use-short-answers               t
        tab-always-indent               t
        kill-do-not-save-duplicates     t
        help-window-select              t
        read-minibuffer-restore-windows t
        load-prefer-newer               t
        delete-by-moving-to-trash       t
        mouse-wheel-tilt-scroll         t
        mouse-wheel-flip-direction      t
        dired-dwim-target               t)
(global-auto-revert-mode t)
(repeat-mode)

;; emacs disables certain behaviors for beginners
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
;;(defun risky-local-variable-p (sym &optional _ignored) nil)

;; hide unwanted ^M in non file buffers
(defun tg/special-buffer-p ()
  (or (string= "*" (substring (buffer-name) 0 1))
      (string= " *" (substring (buffer-name) 0 2))
      (eq major-mode 'special-mode)))
(defun tg/hide-carriage-return-special-buffers ()
  (when (and (tg/special-buffer-p)
             (not buffer-display-table))
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M [])))
(defun tg/get-buffer-create (&rest args)
  (with-current-buffer (car args) (tg/hide-carriage-return-special-buffers)))
(advice-add 'get-buffer-create :after #'tg/get-buffer-create)
;; doesn't work for fundamental mode
(add-hook 'after-change-major-mode-hook 'tg/hide-carriage-return-special-buffers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version Control
(use-package magit
  :diminish (magit-auto-revert-mode auto-revert-mode)
  :custom
  (magit-repository-directories  tg/git-directories)
  (magit-bury-buffer-function    'magit-restore-window-configuration)
  (magit-diff-highlight-trailing nil)
  :bind
  (("C-c g"                          . magit-status)
   ([remap vc-revision-other-window] . magit-find-file-other-window)
   :map magit-mode-map
   ([remap magit-mode-bury-buffer] . tg/magit-mode-bury-buffer)
   ([remap magit-log-bury-buffer]  . tg/magit-log-bury-buffer)
   :map magit-diff-section-base-map
   ("RET"        . tg/magit-diff-visit-worktree-file-other-window)
   ("<C-return>" . magit-diff-visit-worktree-file-other-window)
   ;; :map magit-file-section-map
   ;; ("RET"        . tg/magit-diff-visit-worktree-file-other-window)
   ;; ("<C-return>" . magit-diff-visit-worktree-file-other-window)
   ;; :map magit-hunk-section-map
   ;; ("RET"        . tg/magit-diff-visit-worktree-file-other-window)
   ;; ("<C-return>" . magit-diff-visit-worktree-file-other-window)
   :map magit-log-mode-map
   ("RET"        . tg/magit-visit-ref-other-window)
   ("<C-return>" . magit-visit-ref))
  :init
  (defun tg/magit-mode-bury-buffer ()
    (interactive)
    (magit-mode-bury-buffer t))
  (defun tg/magit-log-bury-buffer ()
    (interactive)
    (magit-log-bury-buffer 10))
  (defun tg/magit-diff-visit-worktree-file-other-window ()
    (interactive)
    (magit-diff-visit-worktree-file-other-window (magit-file-at-point t t))
    (other-window 1))
  (defun tg/magit-visit-ref-other-window ()
    (interactive)
    (magit-visit-ref)
    (other-window 1)))
(use-package diff-hl
  :hook
  (magit-pre-refresh  . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode))
(use-package ediff
  :ensure nil
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :hook
  (ediff-before-setup   . tg/ediff-open)
  (ediff-cleanup        . tg/ediff-cleanup)
  (ediff-quit           . tg/kill-ediff-restore)
  (ediff-prepare-buffer . tg/ediff-customizations)
  :config
  (let ((ediff-last-windows)
        (ediff-frame))
    (advice-add 'ediff :around #'tg/add-record-ediff-opened-files)
    (defun tg/record-ediff-opened-files ()
          (add-to-list 'ediff-opened-files buffer-file-name))
    (defun tg/add-record-ediff-opened-files (old-fun &rest args)
      (let ((ediff-opened-files '()))
        (add-hook 'find-file-hook 'tg/record-ediff-opened-files)
        (apply old-fun args)
        (remove-hook 'find-file-hook 'tg/record-ediff-opened-files)))
    (defun tg/ediff-customizations ()
      (setq truncate-lines t))
    (defun tg/ediff-open ()
      (setq ediff-last-windows (current-window-configuration))
      (select-frame-set-input-focus (setq ediff-frame (make-frame)))
      (when (featurep 'dimmer) (dimmer-mode -1))
      (defun tg/ediff-cleanup ()
        (when (featurep 'dimmer) (dimmer-mode t))
        (when ediff-frame
          (delete-frame ediff-frame)
          (setq ediff-frame nil)))
      (defun tg/kill-ediff-restore ()
        (dolist (file ediff-opened-files)
          (kill-buffer (get-file-buffer file)))
        (ediff-kill-buffer-carefully "*ediff-errors*")
        (ediff-kill-buffer-carefully "*ediff-diff*")
        (ediff-kill-buffer-carefully "*ediff-fine-diff*")
        (ediff-kill-buffer-carefully "*Ediff Registry*")
        (ediff-kill-buffer-carefully "*Ediff Control Panel*")
        (set-window-configuration ediff-last-windows)))))
(use-package consult-ls-git
  :after consult
  :commands consult-ls-git
  :config
  (when tg/using-wsl
    (defun tg/process-file-convert-path (func &rest args)
      (let ((rest (cddddr args)))
        (apply func `(,@(seq-subseq args 0 4)
                      ,(car rest)
                      ,(wsl-path-convert-to-win (cadr rest))
                      ,@(cddr rest)))))
    (defun tg/consult-ls-git--execute-git-command (func &rest args)
      (advice-add 'process-file :around #'tg/process-file-convert-path)
      (unwind-protect
          (apply func args)
        (advice-remove 'process-file #'tg/process-file-convert-path)))
    (advice-add 'consult-ls-git--execute-git-command :around #'tg/consult-ls-git--execute-git-command)))
(use-package consult-git-log-grep
  :commands consult-git-log-grep
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup/Persistence
(use-package bookmark
  :ensure nil
  :defer t
  :custom
  (bookmark-save-flag 1))
(use-package recentf
  :ensure nil
  :custom
  (recentf-keep            nil) ;'(file-remote-p file-readable-p))
  (recentf-max-menu-items  25)
  (recentf-max-saved-items 100)
  :config
  ;; https://www.reddit.com/r/emacs/comments/enhbfi/is_there_some_way_to_suppress_the_wrote_recentf/
  (defun tg/suppress-messages (func &rest args)
    "Suppress message output from FUNC."
    (cl-flet ((silence (&rest args1) (ignore)))
      (advice-add 'message :around #'silence)
      (unwind-protect
          (apply func args)
        (advice-remove 'message #'silence))))
  (advice-add 'recentf-cleanup :around #'tg/suppress-messages)
  (run-at-time "30 min" (* 30 60) 'recentf-save-list)
  (recentf-mode t))
(use-package dashboard
  :after diminish
  :diminish dashboard-mode
  :custom
  ;; Setting dashboard-items will cause other packages to load. Projects will
  ;; load projectile. Bookmarks will load tramp if there are remote bookmarks.
  (dashboard-banner-logo-title        "Emacs Dashboard")
  (dashboard-projects-switch-function 'projectile-switch-project-by-name)
  (dashboard-projects-backend         'projectile)
  (dashboard-icon-type                'all-the-icons)
  (dashboard-set-file-icons           t)
  (dashboard-items  (rassq-delete-all 0
                                      `((recents   . 20)
                                        (projects  . ,tg/project-recentf-max-load)
                                        (bookmarks . 10))))
  :config
  (dashboard-setup-startup-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
(use-package deadgrep
  :commands deadgrep
  :config
  ;; override visit file because comes back as windows format
  (defun deadgrep-visit-result ()
    (interactive)
    (deadgrep--visit-result
     (lambda (fname)
       (find-file (wsl-path-convert-to-linux fname)))))
  (defun deadgrep-visit-result-other-window ()
    (interactive)
    (deadgrep--visit-result
     (lambda (fname)
       (find-file-other-window (wsl-path-convert-to-linux fname))))))
(use-package visual-regexp
  :bind ([remap query-replace] . vr/query-replace)
  :commands (vr/replace))
(use-package ctrlf
  :custom
  (ctrlf-auto-recenter t)
  :bind
  (([remap isearch-forward]  . ctrlf-forward-default)
   ([remap isearch-backward] . ctrlf-backward-default)
   :map ctrlf-minibuffer-mode-map
   ("<left>"  . exit-minibuffer)
   ("<right>" . exit-minibuffer))
  :config
  (defun tg/exit-ctrlf-when-not-minibuf (w)
    (remove-hook 'window-selection-change-functions 'tg/exit-ctrlf-when-not-minibuf t)
    (when (and (tg/ctrlf-active-p)
               (not (eq w (selected-window)))
               (frame-focus-state))
      (exit-minibuffer)))
  (add-function :after after-focus-change-function
                (defun tg/exit-ctrlf-after-regain-focus ()
                  (when (tg/ctrlf-active-p)
                    (add-hook 'window-selection-change-functions
                              'tg/exit-ctrlf-when-not-minibuf nil t))))
  (ctrlf-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display
(use-package hl-line :ensure nil)
(use-package ef-themes
  :pin gnu
  :config
  (defun tg/ef-themes-custom-faces ()
    (ef-themes-with-colors
     (custom-set-faces
      `(default ((,c :foreground ,yellow-warmer)))
      `(font-lock-comment-face ((,c :foreground ,green-faint))))))
  (add-hook 'ef-themes-post-load-hook #'tg/ef-themes-custom-faces)
  (ef-themes-select 'ef-dark))
(use-package doom-modeline
  :after (all-the-icons)
  :custom
  (doom-modeline-minor-modes            t)
  (doom-modeline-height                 20)
  (doom-modeline-buffer-file-name-style 'file-name)
  :hook
  (after-init . doom-modeline-mode))
(use-package posframe)
(use-package which-key-posframe
  :after (which-key posframe)
  :custom
  (which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (which-key-posframe-border-width 4)
  (which-key-min-display-lines 2)
  :config
  (which-key-posframe-mode))
(use-package vertico-posframe
  :after (vertico posframe)
  :custom
  (vertico-posframe-parameters   '((left-fringe  . 8)
                                   (right-fringe . 8)))
  (vertico-posframe-border-width 2)
  :custom-face
  (vertico-posframe-border ((t (:background "#FF79CF")))))
(use-package paren
  :ensure nil
  :custom
  (show-paren-delay 0)
  :config
  (show-paren-mode 1))
(use-package dimmer
  :custom
  (dimmer-fraction .1)
  (dimmer-use-colorspace 'RGB)
  (dimmer-watch-frame-focus-events nil)
  :config
  (dimmer-configure-posframe)
  (dimmer-configure-magit)
  (dimmer-configure-which-key)
  (dimmer-configure-hydra)
  ;;(add-to-list 'dimmer-exclusion-regexp-list "^ \\*lsp-ui-doc.*\\*$")
  ;; FIXME needed to make ctrlf and lsp-ui-doc work
  ;; https://github.com/gonewest818/dimmer.el/issues/65
  (defun tg/ctrlf-active-p () (and (featurep 'ctrlf) ctrlf--active-p))
  ;; (defun tg/ediff-control-active-p (buf) (and (featurep 'ediff) (get-buffer-window "*Ediff Control Panel*")))
  (defun dimmer-config-change-handler ()
    (run-at-time 0.2 nil #'dimmer-process-all))
  ;; Need to refresh dimmer after the ui frame closes when you've clicked it or the buffer will remain dark
  (advice-add 'lsp-ui-doc--hide-frame :around
              (lambda (old-fun &rest args)
                (apply old-fun args)
                (dimmer-process-all)))
  (with-no-warnings
    ;; (add-to-list 'dimmer-buffer-exclusion-predicates #'tg/ediff-control-active-p)
    (add-to-list 'dimmer-prevent-dimming-predicates #'tg/ctrlf-active-p)
    ;; FIXME doesn't get called until after popup goes away
    (add-to-list 'dimmer-prevent-dimming-predicates #'lsp-ui-doc--visible-p))
   (dimmer-mode))
(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (let ((inst-name "install-fonts"))
    (when (and (not (file-directory-p tg/fonts-path))
               (not (member "all-the-icons" (font-family-list)))
               (window-system))
      ;; https://emacs.stackexchange.com/questions/14669/sort-of-autoreply-for-specific-messages-in-minibuffer/14676#14676
      (advice-add 'all-the-icons-install-fonts
                  :around
                  (lambda (old-fun &rest args)
                    (cl-letf (((symbol-function 'read-directory-name)
                               (lambda (prompt dummy) tg/fonts-path)))
                      (apply old-fun args)))
                  '((name . inst-name)))
      (all-the-icons-install-fonts t)
      (advice-remove 'all-the-icons-install-fonts inst-name))))
(use-package all-the-icons-dired
  :after all-the-icons
  :hook
  (dired-mode . all-the-icons-dired-mode))
(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setopt cursor-type                   'bar
        ;;truncate-lines               t
        visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(visual-line-mode)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode t)

(when (find-font (font-spec :name "Hack"))
  (set-frame-font "Hack 11"))

(let ((minibuf-face-remapping nil))
  (defun tg/minibuf-update-mapping (face)
    (when minibuf-face-remapping
      (face-remap-remove-relative minibuf-face-remapping)
      (setq minibuf-face-remapping nil))
    (setq minibuf-face-remapping
          (face-remap-add-relative 'default face))
    (force-window-update (current-buffer))))

(defun tg/minibuf-selected-or-deselected (w)
  (when (minibuffer-window)
    (if (and (eq w (selected-window))
             ;; selected if posframe visible
             (or (string-match-p "Minibuf" (buffer-name))
                  (cl-find-if
                   (lambda (frame)
                     (and (frame-visible-p frame)
                          (frame-parameter frame 'posframe-buffer)))
                   (frame-list))))
       (tg/minibuf-update-mapping nil)
     (with-selected-window (minibuffer-window)
       (tg/minibuf-update-mapping tg/highlight-face)))))

(add-hook 'minibuffer-setup-hook
          (defun tg/minibuf-customizations ()
            (add-hook 'window-selection-change-functions
                      'tg/minibuf-selected-or-deselected nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard/Coding
(keymap-global-set "<remap> <dabbrev-expand>" 'hippie-expand)
(keymap-global-set "<remap> <kill-buffer>"    'kill-current-buffer)
(keymap-global-set "C-z"                      'switch-to-minibuffer)
(keymap-global-unset "C-x C-z")
(keymap-global-unset "C-x C-c")

(set-charset-priority          'unicode)
(prefer-coding-system          'utf-8-unix)
(set-default-coding-systems    'utf-8-unix)
(set-terminal-coding-system    'utf-8-unix)
(set-keyboard-coding-system    'utf-8-unix)
(set-file-name-coding-system   'utf-8-unix)
(set-clipboard-coding-system   'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
(set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8-unix))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
(use-package whitespace
  :ensure nil
  :defer t
  :custom
  (whitespace-style '(face trailing))
  :config
  (customize-set-variable 'whitespace-trailing-regexp (concat "\\b.*?" whitespace-trailing-regexp)))
(use-package visual-fill-column
  :disabled
  :custom
  (visual-fill-column-center-text t)
  :hook
  (text-mode . tg/visual-fill-column-mode-hook)
  :init
  ;; some modes, like nxml, derive from text
  (defun tg/visual-fill-column-mode-hook()
    (when (eq major-mode 'text-mode)
      (visual-fill-column-mode))))

(delete-selection-mode 1)
(setopt eval-expression-print-level  nil
        eval-expression-print-length nil
        sentence-end-double-space    nil
        indent-tabs-mode             nil
        abbrev-mode                  t
        require-final-newline        t
        truncate-string-ellipsis     "…"
        fill-column                  110
        tab-width                    4)
(add-hook 'before-save-hook
          (lambda ()
            (interactive)
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-max))
                (delete-blank-lines)))))
(add-hook 'text-mode-hook
          (lambda ()
            (electric-indent-local-mode -1)
            (auto-fill-mode t)))

;; remove EOL \r and \r\n on yank; emacs adds \r in dos mode
(defun tg/insert-for-yank (orig-fun &rest args)
  (funcall orig-fun
           (if t ;;(memq buffer-file-coding-system
               ;;      '(utf-8-unix undecided-unix dos ascii-dos undecided-dos utf-8-dos))
               (replace-regexp-in-string
                (concat '(?\r ?\$))
                ""
                (string-replace "\r\n" "\n" (car args)))
               (car args))))
(advice-add 'insert-for-yank :around #'tg/insert-for-yank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major modes
(use-package yaml-mode
  :bind (:map yaml-mode-map
              (("C-m" . newline-and-indent)))
  :mode ("\\.yml\\'" . yaml-mode))
(use-package powershell
  ;;:mode ("\\.ps1\\'" . powershell-mode)
  )
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :custom
  (rustic-analyzer-command (if (or tg/using-wsl tg/using-windows)
                               (f-join tg/win-deps-path "cargo/bin/rust-analyzer.exe")
                             (f-join tg/home-path ".cargo/bin/rust-analyzer")))
  (rustic-lsp-setup-p nil)
  (rustic-cargo-use-last-stored-arguments t)
  :config
  (setq rustic-format-on-save nil)
  (advice-add #'rustic-flycheck-get-cargo-targets
            :around
            (lambda (old-fun &rest args)
              (apply old-fun (cons (wsl-path-convert-to-win (car args)) (cdr args))))))

(defun tg/nxml-mode-customizations ()
  (setq-local nxml-child-indent 4))
(defun tg/prog-mode-customizations ()
  (auto-fill-mode)
  (hl-line-mode)
  (display-line-numbers-mode)
  (whitespace-mode)
  (setq-local ;;fill-column                     110
              comment-auto-fill-only-comments t
              display-line-numbers            'relative
              display-line-numbers-width      (length (number-to-string
                                                       (line-number-at-pos (point-max))))))
(dolist (hook '(prog-mode-hook nxml-mode-hook sgml-mode-hook yaml-mode-hook conf-mode-hook))
  (add-hook hook 'tg/prog-mode-customizations))
(add-hook 'nxml-mode-hook 'tg/nxml-mode-customizations)

(setq auto-mode-alist
      (append
       '(;;("\\.java\\'"      . java-mode)
         ("\\.js\\'"        . js-mode)
         ("\\.xml\\'"       . nxml-mode)
         ("\\.sh\\'"        . shell-script-mode)
         ("Dockerfile.*\\'" . dockerfile-mode)
         ;;("\\.rs\\'"        . rustic-mode)
         )
       auto-mode-alist))
(setq major-mode-remap-alist
      '((dockerfile-mode . dockerfile-ts-mode)
        ;;(rustic-mode     . rust-ts-mode)
        ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDE

;; I may want to use projectile even without language servers
(use-package projectile
  :defer t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (:map projectile-command-map
        ("." . (lambda ()
                 (interactive)
                 (setq projectile-switch-project-action 'tg/start-ide)
                 (consult-projectile-switch-project)))
        ("p" . (lambda ()
                 (interactive)
                 (setq projectile-switch-project-action '(lambda () (tg/start-ide t)))
                 (consult-projectile-switch-project))))
  :custom
  (projectile-track-known-projects-automatically nil)
  ;; Hybrid is needed to use .gitignore, but this causes it to look for tr
  ;; https://github.com/bbatsov/projectile/issues/1302
  ;; FIXME: but native is needed for TRAMP to find file!
  (projectile-indexing-method (if tg/using-windows 'hybrid 'alien))
  (projectile-git-submodule-command nil)
  ;;(projectile-enable-caching nil) ; or use C-u C-c p f for finding files
  :config
  (setq projectile-switch-project-action '(lambda () (tg/start-ide t)))
  (projectile-mode 1))
(use-package consult-projectile
  :after (consult projectile)
  :init
  ;; https://gitlab.com/OlMon/consult-projectile/-/issues/13
  (setq consult-projectile-use-projectile-switch-project t))

(when tg/lsp-modes (load "~/.emacs.d/ide.el"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package doom-themes
;;   :disabled
;;   :after all-the-icons
;;   :config
;;   (let ((theme 'doom-dark+))
;;     (load-theme theme t)
;;     (setq custom--inhibit-theme-enable nil)
;;     (custom-theme-set-faces
;;      theme
;;      '(default ((t (:foreground "#F58700" :background "#131313")))))
;;     ;;(setq doom-themes-treemacs-theme "doom-colors")
;;     ;;(doom-themes-treemacs-config)
;;     (doom-themes-visual-bell-config)))
;; (use-package modus-themes
;;   :disabled
;;   :ensure nil
;;   :config
;;   (defun tg/modus-themes-custom-faces ()
;;     (modus-themes-with-colors
;;       (custom-set-faces
;;        `(default ((,c :foreground ,rust))))))
;;   (add-hook 'modus-themes-post-load-hook #'tg/modus-themes-custom-faces)
;;   (load-theme 'modus-vivendi :no-confirm))
;; (use-package spaceline
;;   :disabled
;;   :config
;;   (use-package spaceline-config
;;     :custom
;;     (powerline-default-separator 'wave))
;;   (spaceline-emacs-theme))
;; (use-package spaceline-all-the-icons
;;   :disabled
;;   :after (spaceline all-the-icons)
;;   :config
;;   (spaceline-all-the-icons-theme)
;;   (spaceline-toggle-all-the-icons-minor-modes-off)
;;   (spaceline-toggle-all-the-icons-mode-icon-on)
;;   (spaceline-toggle-all-the-icons-buffer-size-off)
;;   (spaceline-toggle-all-the-icons-vc-status-on))
;; (use-package powerline :disabled)
;; (use-package smart-mode-line-powerline-theme
;;   :disabled
;;   :after (powerline smart-mode-line)
;;   :config
;;   (add-hook 'after-init-hook 'sml/setup)
;;   (sml/apply-theme 'powerline))
;; (use-package smart-mode-line
;;   :disabled
;;   :custom
;;   (sml/no-confirm-load-theme t))
