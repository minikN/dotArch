;;; ~/.doom.d/userconfig/exwm.el -*- lexical-binding: t; -*-

;; Load ivy-posframe.
(require 'ivy-posframe)

;; Functions ivy-posframe should be used.
(setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-posframe-display-at-point)
        (complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-display-function-fallback)
        (t               . ivy-posframe-display)))
(ivy-posframe-mode 1)
