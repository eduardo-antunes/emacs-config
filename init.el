;; -*- lexical-binding: t; -*-

(setq ed/config-dir    "~/.config/emacs/"   ;; diretório de configuração
      ed/cache-dir     "~/.cache/emacs/"    ;; diretório de dados
      ed/fixed-font    "Source Code Pro 10" ;; fonte monoespaçada
      ed/fixed-serif   "IBM Plex Mono 10"   ;; fonte monoespaçada (serifas)
      ed/variable-font "Cantarell 12"       ;; fonte variável
      ed/leader        "SPC"                ;; tecla líder (`evil')
      ed/global-leader "C-c")               ;; tecla líder (global)

;; Indentação padrão com quatro espaços
(setq-default tab-width 4
              indent-tabs-mode nil
              tab-always-indent 'complete)

(setq scroll-margin 10           ;; espaço vertical a partir do cursor
      scroll-conservatively 101  ;; rolamento suave
      split-width-threshold nil) ;; abre janelas em split por padrão

(setopt use-short-answers t) ;; respostas curtas
(electric-pair-mode 1) ;; delimitadores balanceados

(require 'package)
;; Pacotes são baixados de três fontes: o GNU ELPA, o NON-GNU ELPA e, depois
;; da execução da linha abaixo, também o MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
;; Mantém as listas de pacotes sempre atualizadas
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t) ;; garante a instalação de todos os pacotes

;; Diretório para dados de pacotes em `ed/cache-dir'
(setq user-emacs-directory (expand-file-name ed/cache-dir))

;; Pacote `no-litering' organiza bastante esse diretório
(use-package no-littering)

;; Arquivos de backup e salvamento automático
(setq backup-directory-alist
      `(("." . ,(no-littering-expand-var-file-name "backups/")))
      auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Outros arquivos
(setq url-history-file
      (no-littering-expand-var-file-name "url-history")
      custom-file
      (no-littering-expand-etc-file-name "custom.el"))

;; Configuração de fontes
(set-face-attribute 'default nil           :font ed/fixed-font)
(set-face-attribute 'fixed-pitch nil       :font ed/fixed-font)
(set-face-attribute 'fixed-pitch-serif nil :font ed/fixed-serif)
(set-face-attribute 'variable-pitch nil    :font ed/variable-font)

(blink-cursor-mode -1) ;; não gosto de cursor piscando
(set-fringe-mode 0)    ;; sem margens desnecessárias

;; Eu uso os temas nativos `modus' para o org, principalmente
(setq modus-themes-org-blocks 'gray-background ;; fundo cinza em blocos de código
      modus-themes-italic-constructs t         ;; uso generoso do itálico
      modus-themes-mixed-fonts t)              ;; fontes monoespaçadas e variáveis

;; Para programação, eu prefiro os temas do pacote `doom-themes'
(use-package doom-themes
  :config
  (setq doom-themes-enable-italic t  ;; permite itálico
        doom-themes-enable-bold nil) ;; não curto muito negrito
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(defun ed/text-visual-setup ()
  "Configurações visuais em buffers de texto"
  (hl-line-mode 1)               ;; destaque para a linha atual
  (display-line-numbers-mode 1)) ;; linhas numeradas

;; Executa as configurações visuais acima
(setq display-line-numbers-type 'relative)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode #'ed/text-visual-setup))

;; Mais informações na modeline
(column-number-mode 1)
(setq display-time-format "%H:%M"
      display-time-default-load-average nil)
(display-time-mode 1)

;; O `minions' resume bem os modos menores
(use-package minions
  :custom (minions-prominent-modes '(flymake-mode))
  :config (minions-mode 1))

(defun ed/open-config ()
  "Abre o arquivo de configuração principal"
  (interactive)
  (find-file (expand-file-name "config.org" ed/config-dir)))

(use-package general
  :after evil
  :config
  ;; Define um utilitário para a criação de atalhos prefixados pelo líder
  (general-create-definer ed/leader-bind
    :states '(emacs insert normal)
    :prefix ed/leader
    :global-prefix ed/global-leader)

  ;; Define alguns atalhos essenciais
  (ed/leader-bind
    "."  #'find-file
    ":"  '(execute-extended-command :wk "M-x")
    "w"  '(save-buffer :wk "write")
    "b"  #'switch-to-buffer
    "c"  #'compile
    "g"  #'recompile

    ;; Movimentos do vim mais práticos
    "h" #'evil-window-left
    "l" #'evil-window-right
    "k" #'evil-window-up
    "j" #'evil-window-down

    ;; Acesso rápido à arquivos e aplicações
    "o"  '(:ignore t :wk "open")
    "oc" '(ed/open-config :wk "config.org")))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; ESC como no vim

(use-package evil
  :init
  (setq evil-want-integration t    ;; integrações opcionais
        evil-want-keybinding nil   ;; atalhos extras, colide com o `evil-collection'
        evil-want-C-u-scroll t     ;; C-u sobe meia página
        evil-want-Y-yank-to-eol t  ;; Y copia até o fim da linha (estilo neovim)
        evil-split-window-below t  ;; abre splits para baixo
        evil-vsplit-window-right t ;; abre vsplits para a direita

        ;; A modeline já traz um indicador do estado do `evil'
        evil-insert-state-message nil
        evil-visual-state-message nil
        evil-replace-state-message nil)
  :custom
  (evil-undo-system 'undo-redo) ;; C-r funciona
  :config
  (evil-mode 1)
  ;; Equivalência filosoficamente agradável entre C-g e ESC
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :config (evilnc-default-hotkeys t)
  :bind (:map evil-normal-state-map
              ("gc" . evilnc-comment-or-uncomment-lines)))

(use-package which-key
  :defer 0
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3))

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

(use-package marginalia :init (marginalia-mode)) ;; minibuffer mais informativo

(use-package corfu
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold 3)
  (global-corfu-mode 1))

(defun ed/org-visual-setup ()
  "Configurações visuais do modo org"
  (org-indent-mode)       ;; indentação semântica das seções
  (visual-line-mode 1))   ;; linhas visuais
(add-hook 'org-mode-hook #'ed/org-visual-setup)

(setq org-startup-folded 'content
      org-ellipsis "_")

;; Essas configurações de indentação garantem que código fique livremente
;; indentado, sem a constante interferência do org
(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

;; A biblioteca nativa `org-tempo' fornece atalhos para a inserção rápida
;; dos delimitadores de blocos, o que é bastante prático
(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-modules 'org-tempo)
  ;; Atalhos para emacs-lisp e python
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; O modo `electric-pair' é muito útil, mas acaba interferindo na inserção
;; de delimitadores configurada acima ao tentar completar parênteses angulares.
;; As linha abaixo resolvem essa questão
(defun ed/electric-no-angle-brackets ()
  (setq-local electric-pair-inhibit-predicate
              `(lambda (ch)
                 (if (char-equal ch ?<) t (,electric-pair-inhibit-predicate ch)))))
(add-hook 'org-mode-hook #'ed/electric-no-angle-brackets)

(defun ed/ask-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "config.org" ed/config-dir))
    (if (y-or-n-p "Gerar arquivos de emacs-lisp? ")(org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'ed/ask-tangle-config)))

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-lAh --group-directories-first --sort=extension"
        delete-by-moving-to-trash t
        dired-kill-when-opening-new-dired-buffer t)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h"   #'dired-up-directory
    "l"   #'dired-find-file
    " "   nil))
(general-define-key :states 'normal "-" #'dired-jump) ;; estilo `vim-vinegar'

;; Interface mais colorida com `diredfl'
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package vterm
  :config
  (ed/leader-bind "oT" '(vterm :wk "terminal-fs")
                  "ot" '(vterm-other-window :wk "terminal")))

(use-package magit
  :config (ed/leader-bind "og" '(magit-status :wk "git")))

(push "~/.local/bin" exec-path) ;; alguns servidores LSP estão aqui
(use-package highlight-indent-guides ;; guias de indentação
  :hook (prog-mode . highlight-indent-guides-mode))

;; Colorização sintática melhor com o `tree-sitter.el'
(use-package tree-sitter
  :hook (prog-mode . tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter)

(setq compilation-scroll-output t)

(defun ed/clang-setup ()
  "Configurações para C e C++"
  (c-set-style "cc-mode")) ;; usa as configurações padrão de indentação
(add-hook 'c-mode-hook #'ed/clang-setup)
(add-hook 'c++-mode-hook #'ed/clang-setup)

 (setq gc-cons-threshold (* 20 1000 1000))
