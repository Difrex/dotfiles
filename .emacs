
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;;; Code:

;; Packages
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


;; define system
(defun system-is-linux()
	(string-equal system-type "gnu/linux"))

;; Start Emacs as server
(when (system-is-linux)
	(require 'server)
	(unless (server-running-p)
		(server-start)))

;; Dired
(require 'dired)
(setq dired-recursive-deletes 'top)

;; Imenu
(require 'imenu)
(setq imenu-autorescan t)
(setq imenu-use-popup-menu t)

(global-set-key (kbd "<f4>") 'imenu)

;; Display name of teh buffer
(setq frame-title-format "GNU Emacs: %b")

;; Disable startup screen
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t)

;; Show-paren-mode settings
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Electric-modes settings
(electric-pair-mode 1)
(electric-indent-mode 1)

;; Delete selection
(delete-selection-mode t)


(add-hook 'after-init-hook #'global-flycheck-mode)
;; (require 'autopair)
;; (autopair-global-mode)

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")

;; Disable GUI
(tooltip-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)
;; (setq redisplay-dont-pause t)
(setq ring-bell-function 'ignore)
;; Set cursor
;; (setq cursor-type 'bar)
;; (setq blink-cursor-mode 1)
;; (setq blink-cursor-interval 0.8)


(setq cursor-type 'bar)
(beacon-mode 1)

;; Scrolling
(setq scroll-step               1)
(setq scroll-margin            10)
(setq scroll-conservatively 10000)

;; Fill column indicator
(require 'fill-column-indicator)
(setq-default fill-column 99)
(setq fci-rule-width 1)
(setq fci-rule-color "#696969")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; Powerline
(setq powerline-default-separator 'slant)

;; Battery
(add-hook 'after-init-hook #'fancy-battery-mode)
(fancy-battery-mode 1)

;; Buffers selectin and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "<f2>") 'bs-show)

(setenv "GOPATH" "/home/d_zheleztsov/.local")

;; Linum plugin
(require 'linum)
(line-number-mode t)
(global-linum-mode t)
(column-number-mode t)
(setq linum-format " %d")

;; Fringe settings
(fringe-mode '(4 . 0))
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Display file size in mode-line
(setq display-time-24hr-format t)
(display-time-mode             t)
(size-indication-mode          t)

;; Indent settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default standart-indent 4)
(setq-default lisp-body-indent 4)
(global-set-key (kbd "RET") 'newline-and-indent)
(add-hook 'go-mode-hook (lambda () (my-setup-indent 2)))

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; EOF newlines
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; Highlight search results
(setq search-highlight t)
(setq query-replace-highlight t)

;; Fonts
(add-to-list 'default-frame-alist '(font . "Hack 15"))
(set-face-attribute 'default t :font "Hack 15")

;; Undo & Redo
(global-unset-key "\C-z")
(global-set-key "\C-z" 'advertised-undo)

;; Comment block
(defun comment-dwim-line (&optional arg)
    "Replacement for comment-dwim"
    (interactive "*P")
    (comment-normalize-vars)
    (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
            (comment-or-uncomment-region (line-beginning-position) (line-end-position))
        (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)

;; Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Close all buffers
(defun close-all-buffers ()
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

;; Calendar localization
(setq calendar-week-start-day 1
      calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май" 
                                 "Июнь" "Июль" "Август" "Сентябрь"
                                 "Октябрь" "Ноябрь" "Декабрь"])

;; Projectile + Neotree
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)

(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
        (neotree-toggle)
        (if project-dir
                (if (neo-global--window-exists-p)
                        (progn
                            (neotree-dir project-dir)
                            (neotree-find file-name)))
            (message "Could not find git project root."))))

(global-set-key [f8] 'neotree-project-dir)

;; Autocomplete
(defun ac-init()
	(require 'auto-complete-config)
	(ac-config-default)
	(setq ac-auto-start 1)
	(setq ac-auto-show-menu t)
	(global-auto-complete-mode t)
	(add-to-list 'ac-modes 'lisp-mode)
	(add-to-list 'ac-sources 'ac-source-variables)
	(add-to-list 'ac-sources 'ac-source-functions)
	(add-to-list 'ac-sources 'ac-source-dictionary))
(ac-init)

;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
	auto-complete-mode (lambda ()
						   (if (not (minibufferp (current-buffer)))
								   (auto-complete-mode 1))
						   ))
(real-global-auto-complete-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (ample)))
 '(custom-safe-themes
   (quote
    ("938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (org-capture . helm-org-completing-read-tags)
     (org-set-tags . helm-org-completing-read-tags)
     (ffap-alternate-file)
     (tmm-menubar)
     (find-file)
     (execute-extended-command)
     (find-file-read-only . ido))))
 '(minimap-mode t)
 '(minimap-window-location (quote right))
 '(package-selected-packages
   (quote
    (helm-swoop ac-helm go-complete go-snippets ansible magit ample-theme spaceline minimap micgoline jedi projectile-speedbar flymake-python-pyflakes flymake-cursor flymake-go flycheck spacemacs-theme powerline helm helm-pydoc helm-go-package auto-complete go-projectile go-mode projectile neotree fill-column-indicator fancy-battery beacon))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:background "black")))))

;; Fonts
(add-to-list 'default-frame-alist '(font . "Hack 15"))
(set-face-attribute 'default t :font "Hack 15")

;; Undo & Redo
(global-unset-key "\C-z")
(global-set-key "\C-z" 'advertised-undo)

; Comment block
(defun comment-dwim-line (&optional arg)
	"Replacement for comment-dwim"
	(interactive "*P")
	(comment-normalize-vars)
	(if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
			(comment-or-uncomment-region (line-beginning-position) (line-end-position))
		(comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)

;; Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

; Close all buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Calendar localization
(setq calendar-week-start-day 1
      calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май" 
                                 "Июнь" "Июль" "Август" "Сентябрь"
                                 "Октябрь" "Ноябрь" "Декабрь"])

;; golang
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/golang-mode")
(require 'go-mode)
(require 'go-complete)
(add-hook 'completion-at-point-functions 'go-complete-at-point)


;; Powerline
(require 'powerline)
(powerline-default-theme)

(require 'spaceline-config)
(spaceline-emacs-theme)

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; HELM
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)


(require 'helm-swoop)

;; Change keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direction. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)


;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)
;; (yas-reload-all)
;; (add-hook 'prog-mode-hook #'yas-minor-mode)

