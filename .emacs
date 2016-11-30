;; define system
(defun system-is-linux()
	(string-equal system-type "gnu/linux"))

;; Start Emacs as server
(when (system-is-linux)
	(require 'server)
	(unless (server-running-p)
		(server-start)))

;; Perl DE
;; (add-to-list 'load-path "~/.emacs.d/pde")
;; (load "pde-load")

;; Username
(setq user-mail-address	""
	  user-full-name	"")


;; Dired
(require 'dired)
(setq dired-recursive-deletes 'top)

;; Imenu
(require 'imenu)
(setq imenu-autorescan t)
(setq imenu-use-popup-menu nil)

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
(blink-cursor-mode -1)
(setq use-dialog-box nil)
;; (setq redisplay-dont-pause t)
(setq ring-bell-function 'ignore)

;; Disable backup/autosave
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-name t)

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

;; Line wrapping
(setq word-wrap t)
(global-visual-line-mode t)

;; Start window size
(when (window-system)
	(set-frame-size (selected-frame) 100 50))

;; IDO plugin
(require 'ido)
(ido-mode t)
(icomplete-mode t)
(ido-everywhere t)
(setq ido-virtual-buffers t)
(setq ido-enable-flex-matchng t)

;; Buffers selectin and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "<f2>") 'bs-show)

;; Indent settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default standart-indent 4)
(setq-default lisp-body-indent 4)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Scrolling
(setq scroll-step               1)
(setq scroll-margin            10)
(setq scroll-conservatively 10000)

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; EOF newlines
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; Highlight search results
(setq search-highlight t)
(setq query-replace-highlight t)

;; Delete trailing whitespaces, format buffer and untabify when save buffer
;; (defun format-current-buffer()
;; 	(indent-region (point-min) (point-max)))
;; (defun untabify-current-buffer()
;; 	(if (not indent-tabs-mode)
;; 			(untabify (point-min) (point-max)))
;; 	nil)
;; (add-to-list 'write-file-functions 'format-current-buffer)
;; (add-to-list 'write-file-functions 'untabify-current-buffer)
;; (add-to-list 'write-file-functions 'delete-trailing-whitespace)

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

;; Fonts
(add-to-list 'default-frame-alist '(font . "Hack 15"))
(set-face-attribute 'default t :font "Hack 15")

;; Bookmarks
(require 'bookmark)
(setq bookmark-save-flag t)
(when (file-exists-p (concat user-emacs-directory "bookmarks"))
	(bookmark-load bookmark-default-file t))
(global-set-key (kbd "<f3>") 'bookmark-set)
(global-set-key (kbd "<f5>") 'bookmark-jump)
(global-set-key (kbd "<f6>") 'bookmark-bmenu-list)
(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))

;; Packages
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Neotree
(add-to-list 'load-path "~/.emacs.d/plugins/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

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

;; Move block of code
;;;;;;;;;;;;;;;;;;;;;
(defun move-region (start end n)
	"Move the currnt region up or down"
	(interactive "r\np")
	(let ((line-text (delete-and-extract-region start end)))
		(forward-line n)
		(let ((start (point)))
			(insert line-text)
			(setq deactivate-mark nil)
			(set-mark start))))

(defun move-region-down (start end n)
	"Move the current line down by N lines"
	(interactive "r\np")
	(move-region start end (if (null n) 1 n)))

(defun move-region-up (start end n)
	"Move the current line up by N lines"
	(interactive "r\np")
	(move-region start end (if (null n) -1 (- n))))

(global-set-key (kbd "M-s-<up>") 'move-region-up)
(global-set-key (kbd "M-s-<down>") 'move-region-down)

;; Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; (add-to-list 'load-path "~/.emacs.d/elpa/fill-column-indicator-20151030.1233/")
;; (require 'fill-column-indicator)
;; (define-globalized-minor-mode
;; 	global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode t)


;; SPEC mode
(autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
                              auto-mode-alist))

;; SASS mode
;; SPEC mode
(setq auto-mode-alist (append '(("\\.scss" . sass-mode))
                               auto-mode-alist))

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

;; org-mode agenda
(setq org-agenda-files (list "~/projects/tasks.org" ))

;; golang
(add-to-list 'load-path "/usr/share/emacs/site-lisp/golang-mode")
(require 'go-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; CUSTOM VARIABLES IN BOTTOM ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(ansi-term-color-vector
   [unspecified "#424242" "#EF9A9A" "#C5E1A5" "#FFEE58" "#64B5F6" "#E1BEE7" "#80DEEA" "#E0E0E0"])
 '(beacon-color "#ec4780")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (ample)))
 '(custom-safe-themes
   (quote
    ("4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(evil-emacs-state-cursor (quote ("#E57373" bar)))
 '(evil-insert-state-cursor (quote ("#E57373" hbar)))
 '(evil-normal-state-cursor (quote ("#FFEE58" box)))
 '(evil-visual-state-cursor (quote ("#C5E1A5" box)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(jabber-account-list (quote (("difrex@jabber.ru" (:password . "vf2yjdfh")))))
 '(magit-diff-use-overlays nil)
 '(minimap-buffer-name " map")
 '(minimap-highlight-line nil)
 '(minimap-minimum-width 15)
 '(minimap-mode t)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location (quote right))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(red "#ffffff")
 '(session-use-package t nil (session))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tabbar-background-color "#353535")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:background "black"))))
 '(minimap-font-face ((t (:height 30 :family "Hack")))))
