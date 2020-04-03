;;; ~/.doom.d/userconfig/windows.el -*- lexical-binding: t; -*-

(defun split-and-follow-h ()
  "Split a window horizontally and follow it."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-v ()
  "Split a window vertically and follow it."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-and-follow-h)
(global-set-key (kbd "C-x 3") 'split-and-follow-v)
