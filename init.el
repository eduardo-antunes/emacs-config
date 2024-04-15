;; -*- lexical-binding: t; -*-
(setq ed/config-dir    "~/.config/emacs/"   ;; diretório de configuração
      ed/cache-dir     "~/.cache/emacs/"    ;; diretório de dados
      ed/fixed-font    "Inconsolata LGC 10" ;; fonte monoespaçada
      ed/fixed-serif   "IBM Plex Mono 10"   ;; fonte monoespaçada (serifas)
      ed/variable-font "Cantarell 12")      ;; fonte variável

;; Indentação com 4 espaços por padrão
(setq-default tab-width 4
              indent-tabs-mode nil)

(setq scroll-margin 10           ;; espaço vertical a partir do cursor
      scroll-conservatively 101  ;; rolamento suave
      split-width-threshold nil) ;; abre janelas em split vertical por padrão

(setopt use-short-answers t) ;; respostas curtas
(electric-pair-mode 1) ;; delimitadores balanceados

(require 'package)
;; Adiciona o MELPA aos repositórios de pacotes
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
;; Mantém as listas de pacotes sempre atualizadas
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t) ;; garante a instalação de todos os pacotes

(setq user-emacs-directory (expand-file-name ed/cache-dir))
(use-package no-littering) ;; move muita coisa para o diretório acima

;; Alguns arquivos específicos usados pelo emacs
(setq url-history-file (no-littering-expand-var-file-name "url-history")
      custom-file (no-littering-expand-etc-file-name "custom.el"))

;; Diretórios próprios para backups e salvamentos automáticos
(setq backup-directory-alist
      `(("." . ,(no-littering-expand-var-file-name "backups/")))
      auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(defvar-keymap ed/personal-map
  :doc "Prefixo para atalhos personalisados"
  "p" #'previous-buffer
  "=" #'indent-region)
(keymap-set global-map "C-z" ed/personal-map)

;; Alterações em atalhos nativos
(keymap-set global-map "M-z" #'zap-up-to-char)
(setq kill-whole-line t) ;; C-k mata a linha inteira

(use-package which-key
  :defer 0
  :config (which-key-mode 1)
  (setq which-key-idle-delay 0.3))

(setq modus-themes-org-blocks 'gray-background ;; fundo cinza em blocos de código
      modus-themes-italic-constructs t         ;; uso generoso do itálico
      modus-themes-mixed-fonts t)              ;; fontes monoespaçadas e variáveis

(use-package doom-themes
  :config
  (setq doom-themes-enable-italic t  ;; permite itálico
        doom-themes-enable-bold nil) ;; não curto muito negrito
  (load-theme 'doom-one t)
  ;; Comentários em itálico (o `doom-one' não tem isso)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (doom-themes-org-config)) ;; configurações extras para org

(setq display-time-format "%H:%M"
      display-time-default-load-average nil)

(blink-cursor-mode -1) ;; não gosto de cursor piscando
(set-fringe-mode    0) ;; sem margens desnecessárias
(column-number-mode 1) ;; número da coluna na modeline
(display-time-mode  1) ;; horário na modeline

(setq display-line-numbers-type 'relative)
(defun ed/text-visual-setup ()
  "Configurações visuais em buffers de texto"
  (hl-line-mode 1)               ;; destaque para a linha atual
  (display-line-numbers-mode 1)) ;; linhas numeradas

;; Executa as configurações visuais acima
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode #'ed/text-visual-setup))

;; O `minions' resume bem os modos menores na modeline
(use-package minions
  :custom (minions-prominent-modes '(flymake-mode))
  :config (minions-mode 1))

(set-face-attribute 'default nil :font ed/fixed-font)
(set-face-attribute 'fixed-pitch nil :font ed/fixed-font)
(set-face-attribute 'fixed-pitch-serif nil :font ed/fixed-serif)
(set-face-attribute 'variable-pitch nil :font ed/variable-font)

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
  :bind (:map minibuffer-local-map
              ("M-h" . backward-kill-word)
              ("<backspace>" . ed/minibuffer-del))
  :init
  (setq vertico-cycle t)
  (vertico-mode))

(use-package marginalia :init (marginalia-mode)) ;; minibuffer mais informativo

(use-package consult
  :config
  (setq consult-preview-key "M-.") ;; previews automáticas me deixam desorientado
  (keymap-set ed/personal-map "b" #'consult-buffer)
  (keymap-set ed/personal-map "s" #'consult-line)
  (keymap-set ed/personal-map "r" #'consult-ripgrep)
  (keymap-set ed/personal-map "d" #'consult-flymake))

(use-package company
  :config
  (setq-default company-format-margin-function nil)
  (add-hook 'prog-mode-hook #'company-mode))

(defun ed/org-visual-setup ()
  "Configurações visuais do modo org"
  (org-indent-mode)
  (visual-line-mode)
  ;; Símbolos bonitinhos para certos elementos do org
  (setq prettify-symbols-alist
        '(("#+begin_src" . ?λ)
          ("#+end_src"   . ?λ)))
  (prettify-symbols-mode))

(add-hook 'org-mode-hook #'ed/org-visual-setup)

(setq org-startup-folded 'content ;; corpo dos documentos vem escondido ("folded")
      org-hide-emphasis-markers t ;; delimitadores de formatação ficam ocultos
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
  "Desativa o balanceamento de <> pelo eletric-pair-mode"
  (setq-local electric-pair-inhibit-predicate
              `(lambda (ch)
                 (if (char-equal ch ?<) t (,electric-pair-inhibit-predicate ch)))))
(add-hook 'org-mode-hook #'ed/electric-no-angle-brackets)

(defun ed/ask-tangle-config ()
  "Pergunta se os arquivos de emacs-lisp (init.el e early-init.el)
devem ser gerados ao salvar config.org"
  (when (string-equal (buffer-file-name)
                      (expand-file-name "config.org" ed/config-dir))
    (if (y-or-n-p "Gerar arquivos de emacs-lisp? ")(org-babel-tangle))))

(add-hook 'org-mode-hook
          ;; Função é executada sempre que arquivos org são salvos
          (lambda () (add-hook 'after-save-hook #'ed/ask-tangle-config)))

(setq dired-listing-switches "-lAh --group-directories-first"
      delete-by-moving-to-trash t)

(use-package vterm
  :config
  (keymap-set ed/personal-map "T" #'vterm)
  (keymap-set ed/personal-map "t" #'vterm-other-window))

(use-package magit
  :config
  (keymap-set ed/personal-map "g" #'magit-status)
  (keymap-set project-prefix-map "g" #'magit-project-status))

(push "~/.local/bin" exec-path) ;; alguns servidores LSP estão aqui
(use-package highlight-indent-guides ;; guias de indentação
  :hook (prog-mode . highlight-indent-guides-mode))

;; Colorização sintática melhor com o `tree-sitter.el'
(use-package tree-sitter
  :init (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter)

(setq compilation-scroll-output t)

(defun ed/clang-setup ()
  "Configurações para C e C++"
  (c-set-style "user"))

(add-hook 'c-mode-hook #'ed/clang-setup)
(add-hook 'c++-mode-hook #'ed/clang-setup)

 (setq gc-cons-threshold (* 20 1000 1000))
