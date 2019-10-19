;; Edwin's init.el
;; start date: 6/08/2019
;; last modified: 10/04/2019
;; My personal init.el to learn more about emacs.
;; --------------------------------------------------------------------------------
;; My approach to writting this file: any new features should be under the super key
;; and some other combination. Any function that are similiar will be replace for example
;; since swiper is just a better search I replaced the default search key-binding "C-s" to now invoke swiper instead of incremental search.
;; --------------------------------------------------------------------------------
;; Currently working/thinking on: I haven't put this file into an .org, but may consider in the furture.
;; I also want to group all setting that set keys together to be more logically grouped. Adding more comments
;; for beginners and myself. add git support to upload files. Do something about "C-x b" change buffers which
;; is still horizontal and I am not sure how I want to approach a fix on this.
;; --------------------------------------------------------------------------------

;;***** BASIC SETTING CHANGES *****

;; Continous PDF
(setq doc-view-continuous t)
;; Show fringes
(setq visual-line-fringe-indicators t)
;; Remove the start screen and basic emacs settings
(setq inhibit-startup-message t)
;; Instead of an annoying sound a visual action is better I feel
(setq visible-bell t)
;; Remove tool bar less clutter
(tool-bar-mode -1)
;; keep menu bar
(menu-bar-mode 1)
;; Remove the scroll bar
(scroll-bar-mode -1)
;; Add line number and column numbers to be displayed on emacs bar
(line-number-mode 1)
(column-number-mode 1)
;; Add line numbers to be displayed on the left side
(global-display-line-numbers-mode 1)
;; display time standard AM/PM
(display-time-mode 1)
;;set line to always be on the screen so words sick together
(global-visual-line-mode 1)
(setq octave-indent-comment nil)
;; electric pair mode to add delimiters as they popup
(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-style 'mixed)
;; change color of numix highlighting original setting gtk_selection_bg_color, gtk_selection_fg_color
(set-face-attribute 'region nil :background "#cae1ff")
;; change electric indent behavior
(setq-default electric-indent-inhibit t)
;; set tabs to true to work with smart tabs for sure
(setq indent-tabs-mode t)
;; tab width default
(setq tab-width 4)
;; change deleting tabs by making it remove the whole tab
(setq backward-delete-char-untabify-method 'hungry)
;; spotting trailing spaces
(setq whitespace-style '(face tabs tab-mark trailing))
(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode)

;; ***** BUFFER *****

;; Setting C-x k to kill the current buffer when pressed
(defun kill-current-buffer ()
  "Sets C-x k to kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
;; Setting the key to kill the buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; Setting ibuffer to be the default buffer and opens in other window
;;(defalias 'list-buffers 'ibuffer)

;; ***** CUSTOM FUNCTIONS *****

;; When opening a new window the cursor will be active in that new window
;; these are custom function for following the cursor.
(defun split-follow-horizontal ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-follow-horizontal)

(defun split-follow-vertical ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-follow-vertical)

;; Copy the file path to kill
(defun copy-file-path-to-kill ()
  (interactive)
  (insert (eval-expression 'buffer-file-name))
  (move-beginning-of-line 1)
  (kill-line 1)
  (open-line 1))
(global-set-key (kbd "C-c n") 'copy-file-path-to-kill)

;; ***** SETTING UP MELPA and checking PACKAGES *****

;; Setting up MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; For making sure packages are installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ***** PACKAGES BELOW *****

;; magit for git interaction
(use-package magit
  :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

(use-package smart-tabs-mode
  :ensure t)
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'python 'nxml)

;; Adding web mode for web development
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil))
(add-hook 'web-mode-hook  'my-web-mode-hook)


;; Adding emmet mode for completion
(use-package emmet-mode
  :ensure t)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(setq emmet-self-closing-tag-style " /")

;; Adding sudo edit for editing in sudo
(use-package sudo-edit
  :ensure t)

;; Adding a color theme that keeps things simple and organizes the color scheme
(use-package habamax-theme
  :ensure t
  :config
  (load-theme 'habamax t))

;; Adding dashboard to basically customize the startup screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)))
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-footer nil))

;; Adding avy this allows for jumping around to characters
;; only on the current screen just press "M-S"
;; then enter the letter you would like to go to.
;; recent change just M-s easier to press
(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

;; Adding swiper
(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

;; trying out helm for a little while to see what I think
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (setq helm-autoresize-max-height 0
	helm-autoresize-min-height 40
	helm-M-x-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentq-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-split-window-in-side-p t))
(helm-mode 1)
(helm-autoresize-mode 1)

;; Adding switch-window
(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  :bind
  ([remap other-window] . switch-window))

;; Used to try packages
(use-package try
  :ensure t)

;; Which key is used for auto completes of commands e.g. C-x (forgot command?)
;; after waiting a couple of second a menu will pop-up
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Adding Beacon which just shows cursor a little more disticly when switching windows
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;;|------------------------------------------------------------------------------|
;;|										 |
;;|			     Extra added variables				 |
;;|										 |
;;|------------------------------------------------------------------------------|
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "SRC" :family "hack")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (habamax-custom)))
 '(custom-safe-themes
   (quote
    ("ade856d9263050e485bb594687315bb3b04a9accb938a6be1aca7e6fed3b5bd5" default)))
 '(package-selected-packages
   (quote
    (which-key web-mode use-package try switch-window swiper sudo-edit smart-tabs-mode magit helm habamax-theme emmet-mode dashboard beacon avy))))
;; Edwin's init.el
;; start date: 6/08/2019
;; last modified: 10/04/2019
;; My personal init.el to learn more about emacs.
;; --------------------------------------------------------------------------------
;; My approach to writting this file: any new features should be under the super key
;; and some other combination. Any function that are similiar will be replace for example
;; since swiper is just a better search I replaced the default search key-binding "C-s" to now invoke swiper instead of incremental search.
;; --------------------------------------------------------------------------------
;; Currently working/thinking on: I haven't put this file into an .org, but may consider in the furture.
;; I also want to group all setting that set keys together to be more logically grouped. Adding more comments
;; for beginners and myself. add git support to upload files. Do something about "C-x b" change buffers which
;; is still horizontal and I am not sure how I want to approach a fix on this.
;; --------------------------------------------------------------------------------


;;***** BASIC SETTING CHANGES *****

;; Continous PDF
(setq doc-view-continuous t)
;; Show fringes
(setq visual-line-fringe-indicators t)
;; Remove the start screen and basic emacs settings
(setq inhibit-startup-message t)
;; Instead of an annoying sound a visual action is better I feel
(setq visible-bell t)
;; Remove tool bar less clutter
(tool-bar-mode -1)
;; keep menu bar
(menu-bar-mode 1)
;; Remove the scroll bar
(scroll-bar-mode -1)
;; Add line number and column numbers to be displayed on emacs bar
(line-number-mode 1)
(column-number-mode 1)
;; Add line numbers to be displayed on the left side
(global-display-line-numbers-mode 1)
;; display time standard AM/PM
(display-time-mode 1)
;;set line to always be on the screen so words sick together
(global-visual-line-mode 1)
;; electric pair mode to add delimiters as they popup
(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-style 'mixed)
;; change color of numix highlighting original setting gtk_selection_bg_color, gtk_selection_fg_color
(set-face-attribute 'region nil :background "#cae1ff")

;; ***** Text editing settings *****

;; set .m files to be in octave mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; change electric indent behavior
(setq-default electric-indent-inhibit t)
;; set tabs to true to work with smart tabs for sure
(setq indent-tabs-mode t)
;; tab width default
(setq tab-width 4)
;; change deleting tabs by making it remove the whole tab
(setq backward-delete-char-untabify-method 'hungry)
;; spotting trailing spaces
(setq whitespace-style '(face tabs tab-mark trailing))
(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode)

;; ***** BUFFER *****

;; Setting C-x k to kill the current buffer when pressed
(defun kill-current-buffer ()
  "Sets C-x k to kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
;; Setting the key to kill the buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; Setting ibuffer to be the default buffer and opens in other window
;;(defalias 'list-buffers 'ibuffer)

;; ***** CUSTOM FUNCTIONS *****

;; When opening a new window the cursor will be active in that new window
;; these are custom function for following the cursor.
(defun split-follow-horizontal ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-follow-horizontal)

(defun split-follow-vertical ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-follow-vertical)

;; Copy the file path to kill
(defun copy-file-path-to-kill ()
  (interactive)
  (insert (buffer-file-name))
  (move-beginning-of-line 1)
  (kill-line 1)
  (open-line 1))
(global-set-key (kbd "C-c n") 'copy-file-path-to-kill)


;; ***** SETTING UP MELPA and checking PACKAGES *****

;; Setting up MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; For making sure packages are installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ***** PACKAGES BELOW *****

;; magit for git interaction
(use-package magit
  :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

(use-package smart-tabs-mode
  :ensure t)
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'python 'nxml)

;; Adding web mode for web development
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Adding emmet mode for completion
(use-package emmet-mode
  :ensure t)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(setq emmet-self-closing-tag-style " /")

;; Adding sudo edit for editing in sudo
(use-package sudo-edit
  :ensure t)

;; Adding a color theme that keeps things simple and organizes the color scheme
(use-package habamax-theme
  :ensure t
  :config
  (load-theme 'habamax t))

;; Adding dashboard to basically customize the startup screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)))
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-footer nil))

;; Adding avy this allows for jumping around to characters
;; only on the current screen just press "M-S"
;; then enter the letter you would like to go to.
;; recent change just M-s easier to press
(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

;; Adding swiper
(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

;; trying out helm for a little while to see what I think
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 40
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentq-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-split-window-in-side-p t))
(helm-mode 1)
(helm-autoresize-mode 1)

;; Adding switch-window
(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  :bind
  ([remap other-window] . switch-window))

;; Used to try packages
(use-package try
  :ensure t)

;; Which key is used for auto completes of commands e.g. C-x (forgot command?)
;; after waiting a couple of second a menu will pop-up
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Adding Beacon which just shows cursor a little more disticly when switching windows
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;;|------------------------------------------------------------------------------|
;;|                                                                              |
;;|                          Extra added variables                               |
;;|                                                                              |
;;|------------------------------------------------------------------------------|
