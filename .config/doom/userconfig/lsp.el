;;; ~/.doom.d/userconfig/lsp.el -*- lexical-binding: t; -*-

(setq lsp-ui-peek-list-width 100)

;; Docker
;;; PHP
(require 'lsp-docker)

(lsp-docker-init-clients
  :docker-image-id "lsp-docker-php"
  :docker-container-name "lsp-docker-php"
  :client-packages '(lsp-php)
  :client-configs (list
   (list :server-id 'iph :docker-server-id 'phpls-docker :server-command "intelephense --stdio"))

  ;; Projects
  :path-mappings '(
                   ("/mnt/c/Users/deb/code/phoeni" . "/projects")))
