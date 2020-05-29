;;; ~/git/dotArch/.config/doom/userconfig/acpi.el -*- lexical-binding: t; -*-

(defun db/power-menu ()
  "Interactive menu for shutdown, reboot or sleep."
  (interactive)
  (let ((actions '("Shutdown" "Reboot" "Sleep")))
    (setq action (ivy-completing-read "What do you want to do?" actions ))
    (if (y-or-n-p (concat "Execute " action "? Unsaved progress will be lost. "))
        (let ((default-directory "/sudo::"))
          (cond ((equal action "Shutdown")
                 (shell-command "systemctl poweroff"))
                ((equal action "Reboot")
                 (shell-command "systemctl reboot"))
                ((equal action "Sleep")
                 (shell-command "systemctl suspend")))))))
