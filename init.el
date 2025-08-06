;; -*- lexical-binding: t; -*-

(setq ed/config-dir    "~/.config/emacs/"
      ed/cache-dir     "~/.local/share/emacs/"
      ed/fixed-font    "Input Mono 10"
      ed/var-font      "Noto Sans 12"
      ed/evil-leader   "SPC"
      ed/global-leader "C-c")

(setq scroll-margin 10
      scroll-conservatively 101  ;; rolagem suave
      split-width-threshold nil) ;; split horizontal por padrão

(setopt use-short-answers t)
(electric-pair-mode 1)
(visual-line-mode 1)

;; -----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; -----------------------------------------------------------------------------

(setq user-emacs-directory (expand-file-name ed/cache-dir))
(use-package no-littering)

(setq url-history-file (no-littering-expand-var-file-name "url-history")
      custom-file (no-littering-expand-etc-file-name "custom.el"))

(setq backup-directory-alist
      `(("." . ,(no-littering-expand-var-file-name "backups/")))
      auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; -----------------------------------------------------------------------------

(tool-bar-mode     -1) ;; sem barra de ferramentas
(menu-bar-mode     -1) ;; sem barra de menus
(scroll-bar-mode   -1) ;; sem barra de rolagem
(blink-cursor-mode -1) ;; não gosto de cursor piscando
(set-fringe-mode    5) ;; pequenas margens

(set-face-attribute 'default nil :font ed/fixed-font)
(set-face-attribute 'fixed-pitch nil :font ed/fixed-font)
(set-face-attribute 'variable-pitch nil :font ed/var-font)

(setq display-line-numbers-type 'relative)
(defun ed/text-visual-setup ()
  "Configurações visuais em buffers de texto"
  (hl-line-mode 1)
  (display-line-numbers-mode 1))

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode #'ed/text-visual-setup))

(use-package catppuccin-theme
  :config (load-theme 'catppuccin :no-confirm))

;; -----------------------------------------------------------------------------

(setq display-time-format "%H:%M"
      display-time-default-load-average nil)
(column-number-mode 1) ;; número da coluna na modeline
(display-time-mode  1) ;; horário na modeline

(use-package minions
  :custom (minions-prominent-modes '(flymake-mode))
  :config (minions-mode 1))

;; -----------------------------------------------------------------------------

(use-package which-key
  :defer 0
  :config (which-key-mode 1)
  (setq which-key-idle-delay 0.3))

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t) ;; Y => y$
  (setq evil-respect-visual-line-mode t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :config (evilnc-default-hotkeys t)
  :bind (:map evil-normal-state-map
              ("gc" . evilnc-comment-or-uncomment-lines)))

(use-package general
  :after evil
  :config
  (general-create-definer ed/leader-bind
    :states '(emacs insert normal)
    :prefix ed/evil-leader
    :global-prefix ed/global-leader)
  (ed/leader-bind
    "."  #'find-file
    ":"  '(execute-extended-command :wk "M-x")
    "w"  '(save-buffer :wk "write")
    "b"  #'switch-to-buffer
    "c"  #'compile
    "g"  #'recompile
    "h"  #'evil-window-left
    "l"  #'evil-window-right
    "k"  #'evil-window-up
    "j"  #'evil-window-down)

;; -----------------------------------------------------------------------------

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(defun ed/minibuffer-del (arg)
  "Se o conteúdo do minibuffer for um caminho, apaga até a pasta pai do arquivo,
    do contrário, apaga normalmente (i.e. um caractere só)"
  ;; Peguei isso emprestado da configuração de um amigo
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-delete-char arg)))

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              :map minibuffer-local-map
              ("M-h" . backward-kill-word)
              ("<backspace>" . ed/minibuffer-del))
  :init
  (setq vertico-cycle t)
  (vertico-mode))
(use-package marginalia :init (marginalia-mode))

(use-package corfu
  :demand t
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode 1)
  :config
  (keymap-unset corfu-map "RET")
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold 3))

;; -----------------------------------------------------------------------------

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-lAh --group-directories-first --sort=extension"
        delete-by-moving-to-trash t
        dired-kill-when-opening-new-dired-buffer t)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file
    " " nil))
(general-define-key :states 'normal "-" #'dired-jump) ;; estilo `vim-vinegar'

;; Interface mais colorida com `diredfl'
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package vterm
  :config (ed/leader-bind "t" #'vterm-other-window :wk "terminal"))

(use-package magit
  :init (setq magit-section-visibility-indicator nil)
  :config (ed/leader-bind "g" '(magit-status :wk "git")))

;; -----------------------------------------------------------------------------

(require 'treesit)
(customize-set-variable 'treesit-font-lock-level 3)
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.24.1")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")))

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)))

(push "~/.local/bin" exec-path) ;; alguns servidores LSP estão aqui

;; -----------------------------------------------------------------------------

(use-package compile
 :hook (compilation-filter . ansi-color-compilation-filter)
 :custom (ansi-color-bold-is-bright 't))
(setq compilation-scroll-output t)

(use-package project
  :ensure nil
  :config
  (setopt xref-search-program 'ripgrep)
  (ed/leader-bind
    "p" (general-simulate-key "C-x p" :which-key "project")))

;; -----------------------------------------------------------------------------

(setq gc-cons-threshold (* 20 1000 1000))
