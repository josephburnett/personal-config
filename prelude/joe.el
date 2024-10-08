;; Mouse
(xterm-mouse-mode)
(defun up-slightly () (interactive) (scroll-up 1))
(defun down-slightly () (interactive) (scroll-down 1))
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key (kbd "<mouse-5>") 'up-slightly)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
