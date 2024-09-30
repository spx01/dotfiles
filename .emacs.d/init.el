(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package icons
  :ensure t
  :config
  (define-icon tab-bar-new nil
    `((image "tabs/new.xpm"
             :height (1.0 . em)
             :margin ,tab-bar-button-margin
             :ascent center)
      ;; (emoji "➕")
      ;; (symbol "＋")
      (text " + "))
    "Icon for creating a new tab."
    :version "29.1"
    :help-echo "New tab")

  (define-icon tab-bar-close nil
    `((image "tabs/close.xpm"
             :height (1.0 . em)
             :margin ,tab-bar-button-margin
             :ascent center)
      ;; (emoji " ❌")
      ;; (symbol "✕") ;; "ⓧ"
      (text " x"))
    "Icon for closing the clicked tab."
    :version "29.1"
    :help-echo "Click to close tab"))

(use-package emacs
  :ensure nil
  :demand t

  :config
  (let ((backup-dir "~/.emacs.tmp/backups")
        (auto-saves-dir "~/.emacs.tmp/auto-saves"))
    (dolist (dir (list backup-dir auto-saves-dir))
      (when (not (file-directory-p dir))
        (make-directory dir t)))
    (setq backup-directory-alist `(("." . ,backup-dir))
                  auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
                  auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
                  tramp-backup-directory-alist `((".*" . ,backup-dir))
                  tramp-auto-save-directory auto-saves-dir))
  (setq backup-by-copying t    ; Don't delink hardlinks
        delete-old-versions t  ; Clean up the backups
        version-control t      ; Use version numbers on backups,
        kept-new-versions 2    ; keep some new versions
        kept-old-versions 2)   ; and some old ones, too

  (setq help-window-select t)

  (setq load-prefer-newer t)

  (setq mode-require-final-newline 'visit-save)

  (add-hook 'text-mode-hook 'display-line-numbers-mode)
  (add-hook 'conf-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (setq display-line-numbers-type 'relative)

  (setq initial-buffer-choice t)

  ;; (setq-default show-trailing-whitespace t)

  (setq-default compilation-scroll-output 'first-error)

  (blink-cursor-mode 0)
  (recentf-mode 1)
  (electric-pair-mode 1)
  (global-whitespace-mode 1)

  (tool-bar-mode -1)
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice nil)

  (setq tab-always-indent 'complete)

  (setq enable-recursive-minibuffers t)

  (setq indent-tabs-mode nil)

  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-use-plists t)


  (defun ctabfix ()
    (when (equal tab-always-indent 'complete)
      (keymap-set c-mode-base-map "<tab>" #'completion-at-point)))
  (add-hook 'c-mode-hook #'ctabfix)
  (add-hook 'c++-mode-hook #'ctabfix)

  (defun my-backward-kill-word ()
    "If there are only whitespaces between the cursor and the previous word, delete only the whitespaces.
Otherwise, delete the previous word."
    (interactive)
    (let ((pos (point)))
      (skip-syntax-backward " ")
      (if (= pos (point))
          ;; No whitespaces were skipped, so kill the previous word.
          (backward-kill-word 1)
        ;; Delete the whitespace characters.
        (delete-region (point) pos))))

  :bind (:map global-map
    ("C-z" . nil)
    ("C-<tab>" . other-window)
    ("C-x C-r" . restart-emacs)
    ("M-z" . zap-up-to-char)
    ("C-x C-b" . ibuffer)
    ("M-DEL" . my-backward-kill-word)
    ("C-x 3" . (lambda () (interactive) (split-window-right) (other-window 1)))
    ("C-x 2" . (lambda () (interactive) (split-window-below) (other-window 1)))))

(use-package whitespace
  :config
  (setq whitespace-style '(face tabs lines-tail trailing)))

(use-package package
  :ensure nil
  :init
  (setq package-native-compile t)
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-archive-priorities '(("melpa" . 100))))

(defun anchor-buffer ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package nerd-icons-ibuffer
  :ensure nil
  :config
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure nil
  :config
  (nerd-icons-completion-mode))

(use-package consult-flycheck
  :ensure t
  :bind (:map global-map
              ("M-g f" . consult-flycheck)))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.??
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package ialign
  :ensure t)

(use-package visual-regexp
  :ensure t
  :bind (:map global-map
              ("C-c r" . vr/replace)
              ("C-c q" . vr/query-replace)
              ;; TODO: multiple cursors
              ;; ("C-c m" . vr/mc-mark)
              ))

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))
(put 'narrow-to-region 'disabled nil)

(defalias 'keymap-5lines
   (kmacro "C-a M-: ( f u n c a l l SPC f o r m a t - k e y m a p ) <return> <return>"))

(use-package avy
  :ensure t
  :bind (:map global-map
              ("C-'" . avy-goto-char)))

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  ;; (setq spacious-padding-subtle-mode-line
  ;;    `( :mode-line-active 'default
  ;;          :mode-line-inactive vertical-border))
  (spacious-padding-mode 1))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package expand-region
  :ensure t
  :bind (:map global-map
              ("C-=" . er/expand-region)))

(use-package corfu
  :ensure t
  :after orderless
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  :config
  (keymap-set corfu-map "M-q" #'corfu-quick-complete)
  (keymap-set corfu-map "C-q" #'corfu-quick-insert)
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(use-package flycheck
  :ensure t
  :config
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-check-syntax-automatically '(save)))

(use-package lsp-mode
  ;; :quelpa
  ;; (lsp-mode
  ;;  :fetcher github
  ;;  :repo emacs-lsp/lsp-mode
  ;;  :commit "a478e03cd1a5dc84ad496234fd57241ff9dca57a")
  :ensure t
  :init
  (setq lsp-use-plists t)
  :config
  (setq lsp-enable-snippet nil)
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  )

(use-package lsp-ui
  :ensure t)

(use-package consult-lsp
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.tmp/undotree")))
  (global-undo-tree-mode 1))

(use-package multiple-cursors
  :ensure t
  :bind (:map global-map
              ("C-S-c C-S-c" . 'mc/edit-lines)
              ("C->" . 'mc/mark-next-like-this)
              ("C-<" . 'mc/mark-previous-like-this)
              ("C-c C-<" . 'mc/mark-all-like-this)))


(use-package magit
  :ensure t)

;; (use-package keycast
;;   :config
;;   (keycast-mode-line-mode 1)
;;   (set-face-attribute 'keycast-key nil :background "dark slate blue"))

(use-package transpose-frame
  :ensure t)

(use-package clang-format+
  :ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-default-state 'emacs))

(use-package goto-chg
  :ensure t)

(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :after projectile
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  (setq-local completion-at-point-functions
              (mapcar #'cape-company-to-capf
                      (list #'company-files #'company-keywords #'company-dabbrev)))
  )

;; TODO: embark
(use-package embark
  :ensure t)

(use-package embark-consult
  :ensure t)

(use-package wgrep
  :ensure t)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
