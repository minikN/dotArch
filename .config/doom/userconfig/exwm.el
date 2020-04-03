;;; ~/.doom.d/userconfig/exwm.el -*- lexical-binding: t; -*-

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section).
;(server-start)

;; Load EXWM.
(require 'exwm)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 10)

;; Use class names for all windows except Java and GIMP
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; Disable the default key map
(define-key exwm-mode-map (kbd "C-c") nil)

;; Global key bindings
(setq exwm-input-global-keys
      `(
        ;; Bind "s-0 -> s-9" to workspaces.
        ([?\s-1] . (lambda () (interactive) (exwm-workspace-switch 0)))
        ([?\s-2] . (lambda () (interactive) (exwm-workspace-switch 1)))
        ([?\s-3] . (lambda () (interactive) (exwm-workspace-switch 2)))
        ([?\s-4] . (lambda () (interactive) (exwm-workspace-switch 3)))
        ([?\s-5] . (lambda () (interactive) (exwm-workspace-switch 4)))
        ([?\s-6] . (lambda () (interactive) (exwm-workspace-switch 5)))
        ([?\s-7] . (lambda () (interactive) (exwm-workspace-switch 6)))
        ([?\s-8] . (lambda () (interactive) (exwm-workspace-switch 7)))
        ([?\s-9] . (lambda () (interactive) (exwm-workspace-switch 8)))
        ([?\s-0] . (lambda () (interactive) (exwm-workspace-switch 9)))

        ;; Launch an application
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))

        ;; Enter line mode and redirect input to emacs
        ([?\s-n] . (lambda () (interactive)
                     (exwm-reset)
                     (setq exwm-input-line-mode-passthrough t)))

        ;; Only enter line mode
        ([?\s-N] . (lambda () (interactive)
                     (exwm-reset)
                     (setq exwm-input-line-mode-passthrough nil)))

        ;; enter char-mode again
        ([?\s-i] . exwm-input-release-keyboard)
        ([?\s-f] . exwm-layout-toggle-fullscreen)
        ([?\s-t] . exwm-floating-toggle-floating)
        ([?\s-m] . exwm-layout-toggle-mode-line)
        ([?\s-w] . exwm-workspace-move-window)
        ))

;; Set s-c and s-v to C-s and C-v in X application
(setq exwm-input-simulation-keys
      '(([?\s-c] . [C-c])
        ([\?s-v] . [C-v])))

;; set char-mode to default
(setq exwm-manage-configurations '((t char-mode t)))

;; Enable EXWM
(exwm-enable)
