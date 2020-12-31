;;; ~/.doom.d/userconfig/exwm-polybar.el -*- lexical-binding: t; -*-

(defvar db/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun db/kill-panel ()
  "Kill the polybar panel"
  (interactive)
  (when db/polybar-process
    (ignore-errors
      (kill-process db/polybar-process)))
  (setq db/polybar-process nil))

(defun db/start-panel ()
  "Start the polybar panel"
  (interactive)
  (db/kill-panel)
  (setq db/polybar-process (start-process-shell-command "polybar" "*polybar*" "polybar -c=/home/demis/.config/polybar/bar-single.ini single")))

;(advice-add 'enable-theme :after #'db/start-panel)

(setq WORKSPACE_1 ""
      WORKSPACE_2 ""
      WORKSPACE_3 ""
      WORKSPACE_4 ""
      WORKSPACE_5 ""
      WORKSPACE_6 ""
      WORKSPACE_7 "")

;; Setting workspaces for polybar
(defun dw/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 (concat "%{F" THEME_YELLOW "}" WORKSPACE_1 " WWW%{F-}   %{F" THEME_B6 "}" WORKSPACE_2 "%{F-} TERM   %{F" THEME_B6 "}" WORKSPACE_3 "%{F-} CODE   %{F" THEME_B6 "}" WORKSPACE_4 "%{F-} AGENDA   %{F" THEME_B6 "}" WORKSPACE_5 "%{F-} MUSIC   %{F" THEME_B6 "}" WORKSPACE_6 "%{F-} CHAT   %{F" THEME_B6 "}" WORKSPACE_7 "%{F-} GAMES"))
    (1 (concat "%{F" THEME_B6 "}" WORKSPACE_1 "%{F-} WWW   %{F" THEME_YELLOW "}" WORKSPACE_2 " TERM%{F-}   %{F" THEME_B6 "}" WORKSPACE_3 "%{F-} CODE   %{F" THEME_B6 "}" WORKSPACE_4 "%{F-} AGENDA   %{F" THEME_B6 "}" WORKSPACE_5 "%{F-} MUSIC   %{F" THEME_B6 "}" WORKSPACE_6 "%{F-} CHAT   %{F" THEME_B6 "}" WORKSPACE_7 "%{F-} GAMES"))
    (2 (concat "%{F" THEME_B6 "}" WORKSPACE_1 "%{F-} WWW   %{F" THEME_B6 "}" WORKSPACE_2 "%{F-} TERM   %{F" THEME_YELLOW "}" WORKSPACE_3 " CODE%{F-}   %{F" THEME_B6 "}" WORKSPACE_4 "%{F-} AGENDA   %{F" THEME_B6 "}" WORKSPACE_5 "%{F-} MUSIC   %{F" THEME_B6 "}" WORKSPACE_6 "%{F-} CHAT   %{F" THEME_B6 "}" WORKSPACE_7 "%{F-} GAMES"))
    (3 (concat "%{F" THEME_B6 "}" WORKSPACE_1 "%{F-} WWW   %{F" THEME_B6 "}" WORKSPACE_2 "%{F-} TERM   %{F" THEME_B6 "}" WORKSPACE_3 "%{F-} CODE   %{F" THEME_YELLOW "}" WORKSPACE_4 " AGENDA%{F-}   %{F" THEME_B6 "}" WORKSPACE_5 "%{F-} MUSIC   %{F" THEME_B6 "}" WORKSPACE_6 "%{F-} CHAT   %{F" THEME_B6 "}" WORKSPACE_7 "%{F-} GAMES"))
    (4 (concat "%{F" THEME_B6 "}" WORKSPACE_1 "%{F-} WWW   %{F" THEME_B6 "}" WORKSPACE_2 "%{F-} TERM   %{F" THEME_B6 "}" WORKSPACE_3 "%{F-} CODE   %{F" THEME_B6 "}" WORKSPACE_4 "%{F-} AGENDA   %{F" THEME_YELLOW "}" WORKSPACE_5 " MUSIC%{F-}   %{F" THEME_B6 "}" WORKSPACE_6 "%{F-} CHAT   %{F" THEME_B6 "}" WORKSPACE_7 "%{F-} GAMES"))
    (5 (concat "%{F" THEME_B6 "}" WORKSPACE_1 "%{F-} WWW   %{F" THEME_B6 "}" WORKSPACE_2 "%{F-} TERM   %{F" THEME_B6 "}" WORKSPACE_3 "%{F-} CODE   %{F" THEME_B6 "}" WORKSPACE_4 "%{F-} AGENDA   %{F" THEME_B6 "}" WORKSPACE_5 "%{F-} MUSIC   %{F" THEME_YELLOW "}" WORKSPACE_6 " CHAT%{F-}   %{F" THEME_B6 "}" WORKSPACE_7 "%{F-} GAMES"))
    (6 (concat "%{F" THEME_B6 "}" WORKSPACE_1 "%{F-} WWW   %{F" THEME_B6 "}" WORKSPACE_2 "%{F-} TERM   %{F" THEME_B6 "}" WORKSPACE_3 "%{F-} CODE   %{F" THEME_B6 "}" WORKSPACE_4 "%{F-} AGENDA   %{F" THEME_B6 "}" WORKSPACE_5 "%{F-} MUSIC   %{F" THEME_B6 "}" WORKSPACE_6 "%{F-} CHAT   %{F" THEME_YELLOW "}" WORKSPACE_7 " GAMES%{F-}"))))

;; Hook for Polybar to update workspaces
(defun dw/send-polybar-hook (name number)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" name number)))

;; Send the hook above to polybar
(defun dw/update-polybar-exwm ()
  (dw/send-polybar-hook "exwm" 1))

;; Send the hook every time a workspace changes.
(add-hook 'exwm-workspace-switch-hook #'dw/update-polybar-exwm)
