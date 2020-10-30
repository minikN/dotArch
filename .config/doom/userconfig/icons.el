;;; ../../.local/share/git/dotArch/.config/doom/userconfig/icons.el -*- lexical-binding: t; -*-

(after! all-the-icons
  (add-to-list 'all-the-icons-icon-alist '("^*.conf$" all-the-icons-fileicon "config" :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist '("^web.config$" all-the-icons-fileicon "config" :face all-the-icons-green))
  (add-to-list 'all-the-icons-icon-alist '("^phpunit.xml$" all-the-icons-fileicon "phpunit" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist '("^composer.lock$" all-the-icons-fileicon "composer" :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist '("^composer.json$" all-the-icons-fileicon "composer" :face all-the-icons-yellow)))
