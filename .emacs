(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


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
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(org-agenda-files
   (quote
    ("~/org/mobile.org" "~/org/backlog.org" "~/org/schedule.org" "~/org/log.org")))
 '(org-tags-column 100)
 '(package-selected-packages
   (quote
    (clojure-mode cider-eval-sexp-fu lua-mode markdown-preview-mode protobuf-mode cider go-guru company-go))))
; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

; Navigation
(global-set-key (kbd "C-c C-c") 'goto-line)


;; ORG MODE

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/log.org" "Log") "* TODO %?\n  %i\n  %U\n"
	 :prepend t
	 :jump-to-captured t)
	("d" "Done" entry (file+headline "~/org/log.org" "Log") "* DONE %? \n  %i\n  CLOSED: %U\n  %U\n"
	 :prepend t
	 :jump-to-captured t)
	("n" "Notes" entry (file "~/org/notes.org") "* %?\n  %i\n  %U\n" :prepend t)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-targets '(("log.org" :maxlevel . 1)
			   ("backlog.org" :maxlevel . 1)))
(setq org-log-done 'time)
(global-set-key "\C-cf" 'org-gcal-fetch)
(global-set-key "\C-cs" 'org-gcal-sync)
(global-set-key "\C-ck" 'org-gcal-delete-at-point)
(load "~/org-gcal-secret.el")
(load "~/org-gcal.el/org-generic-id.el")
(load "~/org-gcal.el/org-gcal.el")

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


;; LANGUAGE CONFIGURATION

; Callow
(add-to-list 'auto-mode-alist '("\\.clw\\'" . clojure-mode))

; Golang
(require 'go-guru)                                   ; load guru

;; https://johnsogg.github.io/emacs-golang
;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
	   "go build -v && go test -v && go vet"))

  ;; ;; guru settings
  ;; (go-guru-hl-identifier-mode)                    ; highlight identifiers

  ;; ;; Key bindings specific to go-mode
  ;; (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  ;; (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  ;; (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  ;; (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  ;; (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  ;; (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; Misc go stuff
  ;; (auto-complete-mode 1))                         ; Enable auto-complete mode
  )

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;;https://github.com/nsf/gocode/tree/master/emacs-company
(require 'company)                                   ; load company mode
(require 'company-go)                                ; load company mode go backend
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

