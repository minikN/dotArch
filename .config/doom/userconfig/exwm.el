;;; ~/.doom.d/userconfig/exwm.el -*- lexical-binding: t; -*-

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section).
                                        ;(server-start)

;; Start applications in the background.
(defun db/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(use-package! counsel
  :config
  (setq counsel-linux-app-format-function 'counsel-linux-app-format-function-name-only))

;; Load EXWM.
(use-package! exwm
  :config
  ;; Set the initial number of workspaces (they can also be created later).
  (setq exwm-workspace-number 10)

  ;; Per application settings
  (setq exwm-manage-configurations
        '(
          ((equal exwm-class-name "Blueman-manager")
           floating t
           floating-mode-line nil
           width 0.5
           height 0.5)

          ((equal exwm-class-name "Pavucontrol")
           floating t
           floating-mode-line nil
           width 0.5
           height 0.5)

          ((equal exwm-class-name "Steam") workspace 6)
          ((equal exwm-class-name "discord") workspace 5)
          ))

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook
            (lambda ()
              ;; Start polybar
              (db/start-panel)

              ;; Launch apps that will run in the background
              (db/run-in-background "pasystray")
              (db/run-in-background "blueman-applet")))

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

  ;; Configure windows as they're created
  ;;(add-hook 'exwm-manage-finish-hook #'db/configure-window-by-class)

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

          ;; NOT NEEDED IN MASTER/STACK LAYOUT
          ;; Move focus between windows (vim and arrow keys)
          ;; ([s-left] . windmove-left)
          ;; ([?\s-h]  . windmove-left)
          ;; ([s-right] . windmove-right)
          ;; ([?\s-l]   . windmove-right)
          ;; ([s-up] . windmove-up)
          ;; ([?\s-k] . windmove-up)
          ;; ([s-down] . windmove-down)
          ;; ([?\s-j] . windmove-down)

          ;; ;; Move windows
          ;; ([M-s-left] . windmove-swap-states-left)
          ;; ([M-s-right] . windmove-swap-states-right)
          ;; ([M-s-up] . windmove-swap-states-up)
          ;; ([M-s-down] . windmove-swap-states-down)
          ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))
          ;; Master/Stack layout
          ;;; Arrange the windows if needed
          ([?\s-a] . edwina-arrange)

          ;;; Create a new window
          ([?\s-w] . edwina-clone-window)

          ;;; delete the current window
          ([?\s-d] . edwina-delete-window)

          ;;; Move down the hierarchy
          ([?\s-e] . edwina-select-next-window) ;; move focus
          ([?\s-E] . edwina-swap-next-window) ;; move window

          ;;; Move down the hierarchy
          ([?\s-q] . edwina-select-previous-window) ;; move focus
          ([?\s-Q] . edwina-swap-previous-window) ;; move window

          ;;; Swap current window with master
          ([?\s-s] . edwina-zoom)

          ;; Launch applications
          ([?\s- ] . counsel-linux-app)

          ;; Launch terminal
          ([s-return] . ansi-term)

          ;; Enter passwords
          ([?\s-p] . ivy-pass)

          ;; char/line-mode stuff
          ([?\s-i] . exwm-input-release-keyboard)

          ;;; Enter line mode and redirect input to emacs
          ([?\s-n] . (lambda () (interactive)
                       (exwm-reset)
                       (setq exwm-input-line-mode-passthrough t)))

          ;;; Only enter line mode
          ([?\s-N] . (lambda () (interactive)
                       (exwm-reset)
                       (setq exwm-input-line-mode-passthrough nil)))

          ;;; Kill a window
          ([?\s-D] . (lambda () (interactive)
                       (kill-buffer (current-buffer))))

          ;; full-screen / floating
          ([?\s-f] . exwm-layout-toggle-fullscreen)
          ([?\s-F] . exwm-floating-toggle-floating)

          ;; mode-line / move window
          ([?\s-m] . exwm-layout-toggle-mode-line)
          ([?\s-M] . exwm-workspace-move-window)

          ;; Media keys
          ([XF86PowerOff] . db/power-menu)
          ([XF86Sleep]    . db/power-menu)
          ))

  ;; Set s-c and s-v to C-s and C-v in X application
  (setq exwm-input-simulation-keys
        '(([?\s-c] . [C-c])
          ([\?s-v] . [C-v])))

  ;; set char-mode to default
                                        ;(setq exwm-manage-configurations '((t char-mode t)))

  (push '((lambda (buffer-name action)
            (and (string= buffer-name "*Messages*")
                 (> (exwm-workspace--count) 2)))
          (lambda (buffer alist)
            (with-selected-frame (elt exwm-workspace--list 2)
              (display-buffer-same-window buffer alist))))
        display-buffer-alist)
  ;; Enable EXWM
  (exwm-enable))
