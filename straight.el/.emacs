; Bootstrap straight.el
; https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

; Golang
; https://geeksocket.in/posts/emacs-lsp-go/
;
; M-. ...................... jump to definition
; M-. ...................... jump back
; M-? ...................... find references
; M-x lsp-rename ........... rename a symbol
;
(straight-use-package 'go-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'lsp-deferred)

; Preferences
(menu-bar-mode -1)
(global-auto-revert-mode t)
(setq visible-bell 1)
(global-linum-mode t)
(setq linum-format "%d  ")

; Mouse
(xterm-mouse-mode)
(defun up-slightly () (interactive) (scroll-up 1))
(defun down-slightly () (interactive) (scroll-down 1))
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key (kbd "<mouse-5>") 'up-slightly)

; Autosave / Theme / Org agenda
(make-directory "~/.emacs.d/autosaves/" t)
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(custom-enabled-themes '(manoj-dark))
 '(org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "-f"
	       ((org-agenda-span 7)
		(org-deadline-warning-days 0)
		(org-agenda-sorting-strategy '(priority-down effort-up))))
       (tags-todo "PROMISE" nil)
       (tags-todo "URGENT" nil)
       (tags-todo "IMPORTANT" nil))
      nil)))
 '(org-agenda-files '("~/org/notes.org" "~/org/log.org"))
 '(org-agenda-prefix-format
   '((agenda . " %-6e| ")
     (todo . " %-6e| ")
     (tags . " %-6e| ")
     (search . " %-6e| ")))
 '(org-agenda-remove-tags nil)
 '(org-agenda-tags-column 0)
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(org-priority-faces
   '((65 :background "black" :foreground "white" :weight bold)
     (66 :background "black" :foreground "white" :weight normal)
     (67 :background "black" :foreground "white" :weight normal)
     (68 :background "black" :foreground "white" :weight normal)
     (69 :background "black" :foreground "white" :weight normal)
     (70 :background "black" :foreground "white" :weight normal)))
 '(org-tag-faces
   '(("IMPORTANT" :foreground "purple")
     ("URGENT" :foreground "red")
     ("PROMISE" :foreground "cyan")))
 '(org-tags-column 0))

;; ORG MODE

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/org/log.org") "* TODO %?\n  SCHEDULED: %t\n  %i\n  %U\n" :prepend t)
	("d" "Done" entry (file "~/org/log.org") "* DONE %?\n  SCHEDULED: %t\n  %i\n  CLOSED: %U\n  %U\n" :prepend t)
        ("n" "Note" entry (file "~/org/notes.org") "* %?\n  %i\n  %U\n" :prepend t)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-targets '(("log.org" :maxlevel . 2)
			   ("backlog.org" :maxlevel . 1)))
(setq org-log-done 'time)
(global-set-key "\C-cf" 'org-gcal-fetch)
(global-set-key "\C-ck" 'org-gcal-delete-at-point)
(setq org-highest-priority ?A)
(setq org-default-priority ?F)
(setq org-lowest-priority ?F)
(setq org-reverse-note-order t)
(setq org-link-search-must-match-exact-headline nil)

;; BUFFER MANAGEMENT

(defun switch-to-other-buffer ()
  "Switch to other buffer."
  (interactive)
  (switch-to-buffer (other-buffer (get-buffer "*Ibuffer*"))))
(global-set-key (kbd "C-c o") 'switch-to-other-buffer)
(fset 'show-ibuffer
   "\C-x\C-b")
(global-set-key (kbd "C-c b") 'show-ibuffer)

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
