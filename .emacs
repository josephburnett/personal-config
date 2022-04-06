
; https://gist.github.com/belak/ca1c9ae75e53324ee16e2e5289a9c4bc
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure and load use-package
(setq use-package-always-ensure t)

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))


;; BASIC STUFF ;;

; Appearance
(menu-bar-mode -1)

; Keep buffers in sync with files
(global-auto-revert-mode t)

; No sounds please
(setq visible-bell 1)

; Line numbers
(global-linum-mode t)
(setq linum-format "%d  ")
;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; Mouse
(xterm-mouse-mode)
(defun up-slightly () (interactive) (scroll-up 1))
(defun down-slightly () (interactive) (scroll-down 1))
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key (kbd "<mouse-5>") 'up-slightly)

; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
; http://snarfed.org/gnu_emacs_backup_files
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "-f"
	       ((org-agenda-span 7)
		(org-deadline-warning-days 0)))
       (tags-todo "PROMISE" nil)
       (tags-todo "URGENT" nil)
       (tags-todo "IMPORTANT" nil))
      nil)
     ("o" "Time-Flies"
      ((agenda "TODO=\"DONE\""
	       ((org-agenda-span 7)
		(org-agenda-prefix-format "[x]")
		(org-agenda-todo-keyword-format "")
		(org-agenda-entry-types
		 '(:scheduled))
		(org-deadline-warning-days 0))))
      nil
      ("~/org/tf-log"))))
 '(org-agenda-files
   '("~/org/mobile.org" "~/org/backlog.org" "~/org/schedule.org" "~/org/log.org"))
 '(org-agenda-prefix-format
   '((agenda . " %-6e ")
     (todo . " %-6e ")
     (tags . " %-6e ")
     (search . " %-6e")))
 '(org-agenda-remove-tags nil)
 '(org-agenda-tags-column 0)
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(org-priority-faces
   '((65 :background "blue" :foreground "white" :weight bold)
     (66 :background "blue" :foreground "white" :weight normal)
     (67 :background "blue" :foreground "white" :weight normal)))
 '(org-tag-faces
   '(("IMPORTANT" :foreground "purple")
     ("URGENT" :foreground "red")
     ("PROMISE" :foreground "cyan")))
 '(org-tags-column 0)
 '(package-selected-packages
   '(clojure-mode cider-eval-sexp-fu lua-mode markdown-preview-mode protobuf-mode cider go-guru)))
; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

; Navigation
(global-set-key (kbd "C-c C-c") 'goto-line)


;; ORG MODE

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/log.org" "Log") "* TODO %? %^g\n  SCHEDULED: %t\n  %i\n  %U\n"
	 :prepend t)
        ("l" "Log" entry (file+headline "~/org/log.org" "Log") "* TODO %? %^g\n  %i\n  %U\n"
	 :prepend t)
	("d" "Done" entry (file+headline "~/org/log.org" "Log") "* DONE %? \n  SCHEDULED: %t\n  %i\n  CLOSED: %U\n  %U\n"
	 :prepend t)
	("n" "Notes" entry (file "~/org/notes.org") "* %?\n  %i\n  %U\n"
	 :prepend t)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-targets '(("log.org" :maxlevel . 1)
			   ("backlog.org" :maxlevel . 1)))
(setq org-log-done 'time)
(global-set-key "\C-cf" 'org-gcal-fetch)
(global-set-key "\C-ck" 'org-gcal-delete-at-point)
(setq org-highest-priority ?A)
(setq org-default-priority ?C)
(setq org-lowest-priority ?C)


;; WINDOW AND BUFFER MANAGEMENT ;;

; New shell
(defun bash ()
  "Open bash in an ansi-term."
  (interactive)
  (ansi-term "/bin/bash"))
(define-key ctl-x-4-map (kbd "4") 'bash)

; IBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 40 40 :left :elide) " "
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))
(fset 'show-ibuffer
   "\C-x\C-b")
(global-set-key (kbd "C-c b") 'show-ibuffer)

(defun switch-to-other-buffer ()
  "Switch to other buffer."
  (interactive)
  (switch-to-buffer (other-buffer (get-buffer "*Ibuffer*"))))
(global-set-key (kbd "C-c o") 'switch-to-other-buffer)

; Neotree
(require 'neotree)
(global-set-key (kbd "C-c n") 'neotree-toggle)

; Window order and orientation
; http://www.emacswiki.org/emacs/TransposeWindows
(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(define-key ctl-x-4-map (kbd "t") 'transpose-windows)
; http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))
(define-key ctl-x-4-map (kbd "y") 'window-toggle-split-direction)

; Rename file and buffer
; http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;; CUSTOM EDITING FUNCTIONS ;;

(defun delete-horizontal-space-across-lines ()
  "Like delete-horizontal-space but works across newlines."
  (interactive)
  (let ((regexp-forward "[ \t\n]+"))
    (re-search-forward regexp-forward nil t)
    (replace-match "" nil nil))
  (let ((regexp-backward "\\([^ \t\n]\\)[ \t\n]*"))
    (re-search-backward regexp-backward nil t)
    (replace-match "\\1" nil nil)))
(global-set-key (kbd "M-\\") 'delete-horizontal-space-across-lines)


;; LANGUAGE CONFIGURATION ;;

; Callow
(add-to-list 'auto-mode-alist '("\\.clw\\'" . clojure-mode))

; Golang

; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l"))
(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; OTHER ;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-tag ((t (:foreground "white" :weight bold)))))
