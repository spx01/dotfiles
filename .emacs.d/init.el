(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(wombat))
 '(desktop-save-mode nil)
 '(display-line-numbers-type 'relative)
 '(package-selected-packages
   '(keycast magit consult-flycheck multiple-cursors undo-tree lsp-treemacs flycheck corfu expand-region rg spacious-padding el-fetch avy editorconfig visual-regexp ialign projectile zig-mode orderless consult nerd-icons nerd-icons-completion nerd-icons-dired vertico nerd-icons-ibuffer marginalia))
 '(safe-local-variable-values
   '((eval setq format-keymap
	   (lambda nil
	     (ialign
	      (point)
	      (save-excursion
		(forward-line 5)
		(left-char 1)
		(point))
	      "[A-Z/].*?,\\*?/?\\( *\\)" 1 1 t)))))
 '(save-place-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Mononoki Nerd Font Mono" :foundry "UKWN" :slant normal :weight regular :height 158 :width normal))))
 '(fringe ((t :background "#242424")))
 '(header-line ((t :box (:line-width 4 :color "#303030" :style nil))))
 '(header-line-highlight ((t :box (:color "#f6f3e8"))))
 '(keycast-key ((t (:background "dark slate blue"))))
 '(line-number ((t :background "#242424")))
 '(mode-line ((t :box (:line-width 6 :color "#444444" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "#444444" :style nil))))
 '(mode-line-highlight ((t :box (:color "#f6f3e8"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#444444" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "grey85" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey75" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#242424" :foreground "#242424")))
 '(window-divider ((t (:background "#242424" :foreground "#242424"))))
 '(window-divider-first-pixel ((t (:background "#242424" :foreground "#242424"))))
 '(window-divider-last-pixel ((t (:background "#242424" :foreground "#242424")))))

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

  (setq tab-always-indent 'complete)

  (setq enable-recursive-minibuffers t)

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

;; TODO: remove
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; (use-package editorconfig
;;   :ensure t
;;   :config
;;   (editorconfig-mode 1))

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
  (global-corfu-mode))

(use-package flycheck
  :ensure t
  :config
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-check-syntax-automatically '(save)))

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-enable-snippet nil)
  ;; (add-hook 'c-mode-hook #'lsp)
  )

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

(use-package keycast
  :config
  (keycast-mode-line-mode 1)
  (set-face-attribute 'keycast-key nil :background "dark slate blue"))
