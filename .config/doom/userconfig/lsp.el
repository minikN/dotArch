;;; ~/.doom.d/userconfig/lsp.el -*- lexical-binding: t; -*-

;; Generic settings
(after! lsp-mode
  (setq lsp-auto-guess-root nil
        lsp-file-watch-threshold 10000))

(after! lsp-ui
  (setq lsp-ui-peek-list-width 100
        lsp-ui-peek-fontify 'always
        lsp-ui-doc-position 'top
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-max-height 30
        lsp-ui-doc-max-width 90
        lsp-ui-doc-border "white"
        lsp-ui-imenu-enable nil))

;; Key bindings
(map! :leader
      :desc "Show documentation" "c K" 'lsp-ui-doc-show)

;; Docker
(defun db/get-docker-path (path-mappings project-root)
  "Returns the path inside a docker container
  for a given file using lsp-docker-path-mappings"
  (let ((current-project  ()))
    (dolist (project path-mappings)
      (if (string= (concat (car project) "/") project-root)
          (push (cons (cdr project) (car project)) current-project)))
    (concat (car (car current-project)) "/" (file-relative-name buffer-file-name project-root))))

;;; Configure and initialize lsp-docker with the settings above.
(use-package! lsp-docker
  :config

  ;;;; sh, bash, zsh
  (require 'lsp-bash nil t)
  (lsp-docker-register-client :priority 10
                              :server-id 'bash-ls
                              :docker-server-id 'bashls-docker
                              :docker-image-id "lsp-docker-shell"
                              :docker-container-name "lsp-docker-shell"
                              :server-command "bash-language-server start"
                              :path-mappings '(("/home/demis/.local/share/git/dotArch" . "/projects/dotArch")))


  ;;;; php
  (require 'lsp-php nil t)
  (lsp-docker-register-client :priority 10
                              :server-id 'iph
                              :docker-server-id 'phpls-docker
                              :docker-image-id "lsp-docker-php"
                              :docker-container-name "lsp-docker-php"
                              :server-command "intelephense --stdio"
                              :path-mappings '(("/home/demis/.local/share/git/laravel" . "/projects/laravel")))


  ;;;; dockerfile
  (require 'lsp-dockerfile nil t)
  (lsp-docker-register-client :priority 10
                              :server-id 'dockerfile-ls
                              :docker-server-id 'dockerfilels-docker
                              :docker-image-id "lsp-docker-dockerfile"
                              :docker-container-name "lsp-docker-dockerfile"
                              :server-command "docker-langserver --stdio"
                              :path-mappings '(("/home/demis/.local/share/git/lsp-main-dev" . "/projects/lsp-main-dev"))))

;; After lsp-docker, lets configure some new flycheck checkers and enable them.
(after! lsp-docker
  ;;; sh-mode (sh, bash, zsh)
  (load! "checkers/sh"))
