;; -*- lexical-binding: t; -*-

(defun ed/enable-redisplays ()
  "Reativa redesenhos da janela após o seu desligamento"
  (setq-default inhibit-redisplay nil
                inhibit-message nil)
  (redisplay))

(setq gc-cons-threshold most-positive-fixnum) ;; previne coletas de lixo na inicialização
(setq-default inhibit-redisplay t ;; desativa redesenhos na janela
              inhibit-message t)
;; Após a janela ter sido aberta, é necessário permitir redesenhos novamente
(add-hook 'window-setup-hook #'ed/enable-redisplays)

;; A função `load-file' pode causar redesenhos indesejados. Por isso, desativo-a
;; aqui para religá-la após a abertura da janela
(define-advice load-file (:override (file) silence)
  (load-file nil 'nomessage))
(define-advice startup--load-user-init-file
    (:before (&rest _) remove-load-file-silence)
  (advice-remove #'load-file #'load-file@silence))

;; Remove elementos indesejados da interface. É bom removê-los aqui, antes que a
;; janela seja aberta, pois assim eles não são sequer desenhados
(setq inhibit-startup-message t
      ring-bell-function 'ignore)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
;; Desnecessário, mas gosto de também definir o título da janela aqui
(setq frame-title-format "%b - GNU emacs")

;; Previne o carregamento do modo Lisp Interaction ao abrir o emacs
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message "Bem-vindo ao GNU emacs!") ;; :)

;; Suprime erros/avisos da compilação nativa e redireciona o seu cache para
;; um diretório melhor (`~/.cache/emacs/var')
(setq native-comp-async-report-warnings-errors nil)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" "~/.cache/emacs/"))))
