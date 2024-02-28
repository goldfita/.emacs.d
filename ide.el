;;; -*- coding: utf-8; lexical-binding: t -*-

;; Use '(:client-port xxx) to specify Emacs listens on xxx.
;; Use '(:server-port xxx) to specify Emacs connects to xxx.
;; Use '() to specify Emacs connects to tg/one-ls-port.
(setq tg/lsp-servers '(("jdtls"   . ())
                       ("xmlls"   . ())
                       ("ts-ls"   . ())
                       ("bash-ls" . ())
                       ("rust-analyzer" . ()))
      tg/one-ls-host (getenv "LS_IP")
      tg/one-ls-port 60000
      tg/one-ls-init "EONELS\n\n\n\n"
      tg/npm-path    (f-join tg/win-deps-path "nodejs/npm"))


(use-package lsp-mode
  :hook
  (lsp-after-apply-edits . tg/lsp-save-buffer-after-rename)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-file-watch-threshold         5000)
  (lsp-headerline-breadcrumb-enable nil)
  ;;(lsp-enable-file-watchers         nil)
  (lsp-auto-register-remote-clients nil)
  (lsp-rust-rls-server-command      "rust-analyzer")
  (lsp-disabled-clients             '(semgrep-ls))
  :init
  ;; FIXME: https://github.com/emacs-lsp/lsp-java/issues/438
  (setq c-basic-offset 4)
  :config
  ;;(add-to-list 'lsp-file-watch-ignored-files "[/\\\\]pom\\.xml\\'")
  (defun tg/lsp-save-buffer-after-rename (operation)
    (when (eq operation 'rename)
      (save-buffer)))
  
  (when tg/using-wsl
    (setq lsp--uri-file-prefix "file:///")
    (defun lsp--path-to-uri-1 (path)
      (concat lsp--uri-file-prefix
              (--> path
                   (expand-file-name it)
                   (or (file-remote-p it 'localname t) it)
                   (url-hexify-string (wsl-path-convert-to-win it)
                                      lsp--url-path-allowed-chars))))

    (defun tg/lsp--uri-to-path-1 (orig-fun &rest args)
      (let ((fname (apply orig-fun args)))
        (wsl-path-convert-to-linux
         (or (and (eq (elt fname 0) ?\/) (substring fname 1)) fname))))
    (advice-add 'lsp--uri-to-path-1 :around #'tg/lsp--uri-to-path-1)
    
    (defun tg/make-lsp--workspace (orig-fun &rest args)
      (apply orig-fun
             (plist-put args :root
                        (wsl-path-convert-to-win (plist-get args :root)))))
    (advice-add 'make-lsp--workspace :around #'tg/make-lsp--workspace))

  ;; FIXME https://github.com/emacs-lsp/lsp-mode/issues/921
  ;; Open Windows Defender Firewall and go to Advanced Settings -> Inbound
  ;; Rules. Add a new rule on public network (Advanced tab) that opens all
  ;; ports (Protocols and Ports) between the host and guest IP (Scope tab). If
  ;; windows asks you to allow an application access to a network, you'll need
  ;; to go to 'Allow an app or feature through Windows Defender Firewall' and
  ;; delete the application it was asking about. Use emacs echo server example
  ;; and telnet to test connection.
  (setq lsp--tcp-server-wait-seconds 15)

  (defun tg/lsp-client-connection ()
    `(:connect
      ,(lambda (filter sentinel name environment-fn _workspace)
         (let* ((null-proc (make-process :name (format "null-proc-%s*" name) :noquery t))
                (port (plist-get (cdr (assoc name tg/lsp-servers)) :client-port))
                (init-msg (concat name ":" (system-name) ":" (number-to-string port) tg/one-ls-init))
                (retries 0)
                (tcp-client-connection)
                (lsp-client)
                (tcp-server
                 (make-network-process
                  :name (format "*tcp-server-%s*" name)
                  :family 'ipv4
                  :host "0.0.0.0"
                  :service port
                  :sentinel (lambda (proc _string)
                              (lsp-log "Language server %s is connected." name)
                              (setf tcp-client-connection proc))
                  :server 't
                  :noquery t)))
           (unwind-protect
               (progn
                 (setq lsp-client
                       (open-network-stream "lsp-client" nil tg/one-ls-host tg/one-ls-port))
                 (process-send-string lsp-client init-msg)
                 (while (and (not tcp-client-connection) (< retries (* 2 lsp--tcp-server-wait-seconds)))
                   (lsp--info "Waiting for connection for %s, retries: %s" name retries)
                   (sleep-for 0.500)
                   (cl-incf retries)))
             (delete-process lsp-client))
           
           (unless tcp-client-connection
             (condition-case nil (delete-process tcp-server) (error))
             (condition-case nil (delete-process null-proc) (error))
             (error "Failed to create connection to %s on port %s" name port))
           (lsp--info "Successfully connected to %s" name)
           (set-process-query-on-exit-flag tcp-client-connection nil)
           (set-process-query-on-exit-flag tcp-server nil)
           (set-process-filter tcp-client-connection filter)
           (set-process-sentinel tcp-client-connection
                                 (lambda (proc _string)
                                   (when (eq (process-status proc) 'closed)
                                     (delete-process null-proc)
                                     (delete-process tcp-server))
                                   (funcall sentinel proc _string)))
           (cons tcp-client-connection null-proc)))
      :test? ,(lambda () t)))

  (defun tg/lsp-server-connection ()
    `(:connect
      ,(lambda (filter sentinel name environment-fn _workspace)
         (let* ((null-proc (make-process :name (format "null-proc-%s*" name) :noquery t :sentinel sentinel))
                (server-port (plist-get (cdr (assoc name tg/lsp-servers)) :server-port))
                (port (or server-port tg/one-ls-port))
                (init-msg (concat name "::" (if server-port (number-to-string port) "") tg/one-ls-init))
                (tcp-proc (lsp--open-network-stream tg/one-ls-host port (concat name "::tcp"))))
           (process-send-string tcp-proc init-msg)
           (set-process-query-on-exit-flag null-proc nil)
           (set-process-query-on-exit-flag tcp-proc nil)
           (set-process-filter tcp-proc filter)
           (set-process-sentinel tcp-proc
                                 (lambda (proc _string)
                                   (when (eq (process-status proc) 'closed)
                                     (delete-process null-proc))))
           (cons tcp-proc null-proc)))
      :test? ,(lambda () t)))

  ;; https://emacs.stackexchange.com/questions/79412/setf-not-expanding
  (eval-when-compile
    (defun tg/lsp-register-client (orig-fun &rest args)
      (let* ((client (car args))
             (server-id (symbol-name (lsp--client-server-id client)))
             (server (assoc server-id tg/lsp-servers))
             (client-port (plist-get (cdr server) :client-port))
             (new-client (copy-lsp--client client)))
        ;;(message "%s %s %s" server server-id client-port)
        (when server
          (setf (lsp--client-new-connection new-client)
                (if client-port
                    (tg/lsp-client-connection)
                  (tg/lsp-server-connection))))
        (funcall orig-fun new-client))))
  (advice-add 'lsp-register-client :around #'tg/lsp-register-client))

(use-package lsp-java
  :after (lsp-mode dap-mode treemacs)
  :custom
  (lsp-java-workspace-dir (f-join tg/win-deps-path "ls-workspace"))
  (lsp-java-configuration-runtimes
   `[(:name "JavaSE-1.8"
			:path ,(wsl-path-convert-to-win (getenv "JAVA_HOME")))
      (:name "JavaSE-21"
             :path ,(wsl-path-convert-to-win (f-join tg/win-deps-path "ls-jdk"))
             :default t)
      (:name "JavaSE-17"
             :path ,(wsl-path-convert-to-win (f-join tg/win-deps-path "ls-jdk-17")))])
  (lsp-java-configuration-maven-user-settings tg/mvn-settings-file-path)
  :init
  (use-package request :defer t)
  :config
  ;; FIXME: https://github.com/emacs-lsp/lsp-treemacs/issues/109
  (defun tg/lsp-f-ancestor-of-patch (path-args)
    (mapcar (lambda (path) (downcase path)) path-args))
  (when tg/using-windows
    (advice-add 'lsp-f-ancestor-of? :filter-args #'tg/lsp-f-ancestor-of-patch)
    (advice-add 'lsp-f-same? :filter-args #'tg/lsp-f-ancestor-of-patch))
  ;;;;;;;;;;
  )

(use-package lsp-javascript
  :ensure nil
  :after (lsp-mode dap-mode treemacs)
  :config
  (defun lsp-clients-typescript-server-path ()
    (wsl-path-convert-to-win (f-join tg/npm-path "node_modules/typescript/lib")))
  (cl-defun lsp--npm-dependency-path (&key package path &allow-other-keys)
    (f-join tg/npm-path package path)))

(use-package lsp-xml
  :ensure nil
  :after (lsp-mode dap-mode treemacs)
  :custom
  (lsp-xml-server-work-dir (wsl-path-convert-to-win (f-join tg/win-deps-path ".lsp4xml"))))

(use-package consult-lsp
  :after (consult lsp-mode))
(use-package lsp-ui
  :after lsp-mode
  :if (display-graphic-p))
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;; IDE related
(use-package dap-mode
  :after lsp-mode
  :bind
  (:map dap-mode-map
        (("<f7>"   . dap-step-in)
         ("<f8>"   . dap-next)
         ("<f9>"   . dap-continue)
         ("S-<f8>" . dap-step-out)))
  :config
  ;; so we don't get a message about safe variables for .dir-locals.el
  (put 'dap-debug-template-configurations 'safe-local-variable (lambda (_) t))
  (dap-auto-configure-mode))
(use-package dap-java
  :after (lsp-java dap-mode)
  :ensure nil)
(use-package dap-chrome
  :disabled
  :after (lsp-mode dap-mode)
  :ensure)
(use-package treemacs
  :custom
  ;; Use linux python on WSL. Windows python can't see treemac's .py files.
  (treemacs-python-executable         (if tg/using-windows
                                          (f-join tg/win-deps-path "python/python.exe")
                                        "python3"))
  (treemacs-show-hidden-files         nil)
  (treemacs-width-is-initially-locked nil)
  (treemacs-width                     40)
  :bind
  ("C-c t" . treemacs-select-window)
  :hook
  (treemacs-mode . tg/start-ide)
  :config
  (treemacs-project-follow-mode 1)
  (treemacs-follow-mode 0))
(use-package treemacs-all-the-icons
  :after treemacs)
(use-package treemacs-projectile
  :after (treemacs projectile))
(use-package treemacs-magit
  :after (treemacs magit))
(use-package yasnippet :defer t)
(use-package flycheck :defer t)
(use-package consult-flycheck
  :after (consult flycheck))
(use-package centaur-tabs
  :after all-the-icons
  :custom
  (centaur-tabs-style               "slant")
  (centaur-tabs-set-bar             'under)
  (centaur-tabs-set-icons           t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-new-tab-button t)
  (x-underline-at-descent-line      t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>"  . centaur-tabs-forward)
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project))

(defun tg/toggle-tabs (&optional w)
  (when (and (featurep 'projectile)
             (featurep 'centaur-tabs)
             (centaur-tabs-mode-on-p)
             (not (eq (buffer-name) "*Bookmark List*")))
    (if (and (projectile-project-root)
             (buffer-file-name)
             (not (tg/special-buffer-p)))
        (centaur-tabs-local-mode 0)
      (centaur-tabs-local-mode 1))))

(defun tg/project-recentf-open-all (proj-buf)
  (let* ((abs-names (seq-map (lambda (n)
                                (expand-file-name n (projectile-project-root)))
                              (projectile-recently-active-files)))
         (exist-names (seq-filter 'file-exists-p abs-names))
         (recentf-names (seq-take exist-names tg/project-recentf-max-load))
         (last-buf))
    ;;(message "---recent names %s %s %s" recentf-names proj-buf (projectile-project-root))
    (if (not recentf-names)
        (unless proj-buf (projectile-dired))
      (dolist (fname (reverse recentf-names))
        ;;(message "%s" (expand-file-name fname (projectile-project-root)))
        (setq last-buf (find-file-noselect (expand-file-name fname (projectile-project-root)))))
      ;;(message "---last buf %s %s %s" last-buf (get-buffer-window proj-buf) major-mode)
      (when (eq major-mode 'treemacs-mode) (other-window -1))
      (when-let ((proj-buf)
                 (proj-win (get-buffer-window proj-buf)))
        (set-window-buffer proj-win last-buf))
      (switch-to-buffer last-buf nil t))
    last-buf))

(defun tg/start-lsp-on-buffer (buf)
  (when (and buf (not (and (boundp 'lsp-mode) lsp-mode)))
    (dolist (mode tg/lsp-modes)
      (let ((ext-re (car (rassoc mode auto-mode-alist)))
            (buf-file (buffer-file-name buf)))
        ;;(message "---start on buffer %s %s %s %s" buf buf-file ext-re mode)
        (when (and ext-re buf-file (string-match-p ext-re buf-file))
          (funcall (symbol-function mode)))))))
  
(defun tg/most-recent-buffer-in-project (bufs)
  (when-let ((prj-root (projectile-project-root))
             (buf (car bufs)))
    (if-let ((fname (buffer-file-name buf))
             (_match? (eq 0 (string-match prj-root fname))))
        buf
      (tg/most-recent-buffer-in-project (cdr bufs)))))

(let ((lsp-proj-path '())
      (loaded))
  (defun tg/start-lsp ()
    (let ((inhibit-message t))
      (message "[tg/start-lsp] mode=%s  file=%s  project root=%s  project paths=%s"
               major-mode (buffer-file-name) (projectile-project-root) lsp-proj-path))
    (when (member (projectile-project-root) lsp-proj-path)
      (lsp)))

  (defun tg/post-init-ide (start-lsp? proj-root)
    (when start-lsp?
      (add-to-list 'lsp-proj-path (or (projectile-project-root) proj-root)))
    (let ((inhibit-message t))
      (message "[tg/post-init-ide] start=%s  mode=%s  file=%s  buffer=%s  project=%s"
               start-lsp? major-mode (buffer-file-name) (current-buffer) projectile-project-name))
    (tg/start-lsp-on-buffer
     (tg/project-recentf-open-all
      (tg/most-recent-buffer-in-project (buffer-list))))
    (treemacs-add-and-display-current-project-exclusively))
      
  (defun tg/start-ide (&optional start-lsp?)
    (let ((proj-root (when start-lsp? (projectile-project-root projectile-project-name))))
      (if loaded
          (unless (eq 'treemacs-mode major-mode)
            (tg/post-init-ide start-lsp? proj-root))
        (setq loaded t)
        (if (and lsp-proj-path
                 (member (projectile-project-root) lsp-proj-path))
            (switch-to-buffer (tg/most-recent-buffer-in-project (buffer-list)))
          (let ((inhibit-message t))
            (message "[tg/start-ide] start=%s  mode=%s  project paths=%s  project root=%s"
                     start-lsp? major-mode lsp-proj-path proj-root))
          (run-with-timer 0 nil (lambda () (tg/post-init-ide start-lsp? proj-root)))
          (unless (and (featurep 'treemacs)
                       ;;(eq 'visible (treemacs-current-visibility))
                       )
            (treemacs)
            (other-window -1))
          (unless (and (featurep 'projectile) projectile-mode) (projectile-mode))
          (centaur-tabs-mode t)
          (add-hook 'after-change-major-mode-hook 'tg/toggle-tabs)
          (add-hook 'window-state-change-functions 'tg/toggle-tabs nil t))))))

(cl-flet ((get-hook-symb (s) (intern (concat (symbol-name s) "-hook"))))
  (dolist (s tg/lsp-modes)
    (add-hook (get-hook-symb s) 'tg/start-lsp)))
