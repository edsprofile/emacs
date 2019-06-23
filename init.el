;; Edwin's init.el
;; start date: 6/08/2019
;; last modified: 6/16/2019
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
;; Recent changes:
;; - Change: ibuffer from opening in other window to open in current window.
;; - Change: removed using ansi-term and other stuff I want to make thing simpler again.
;; - Change: Removed all code that was commented out.

;; Remove the start screen and basic emacs settings
(setq inhibit-startup-message t)
;; Instead of an annoying sound a visual action is better I feel
(setq visible-bell t)
;; Remove tool bar less clutter
(tool-bar-mode -1)
;; Remove menu bar
(menu-bar-mode -1)
;; Remove the scroll bar
(scroll-bar-mode -1)
;; Add line number and column numbers to be displayed on emacs bar
(line-number-mode 1)
(column-number-mode 1)
;; Add line numbers to be displayed on the left side
(global-display-line-numbers-mode 1)
;; display time standard AM/PM
(display-time-mode 1)
;; Remove tabs
(setq indent-tabs-mode nil)
;;set line to always be on the screen
(setq visual-line-mode t)
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

;; ***** SETTING UP MELPA and checking PACKAGES *****

;; Setting up MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; For making sure packages are installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ***** PACKAGES BELOW *****

;; Adding a powerline for better emacs bar
(use-package powerline
  :ensure t)
(powerline-default-theme)

;; Adding use-ttf to keep font consistent on different machines
;; Currently I have a need for this but may get removed once
;; I do not have to switch machinces so often.
(use-package use-ttf
  :ensure t)
(setq use-tff-default-ttf-font-name "Ubuntu mono")

;; Adding sudo edit for editing in sudo
(use-package sudo-edit
  :ensure t)

;; Adding dmenu to easier see launchable applications
(use-package dmenu
  :ensure t
  :bind
    ("s-d" . 'dmenu))

;; Adding popup-kill-ring to show the kill ring
(use-package popup-kill-ring
  :ensure t
  :bind
  ("M-y" . popup-kill-ring))

;; ***** EXWM *****

;; Adding EXWM an emacs windows manager for those long work days
;; for this to work you need the proper file e.g. /usr/share/xsessions/emacs.desktop
;; EXWM may need to get its own section
(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (dotimes (i 10)
      (exwm-input-set-key (kbd (format "s-%d" i))
                          `(lambda ()
                             (interactive)
                             (exwm-workspace-switch-create ,i))))

  (dolist (k '(XF86AudioLowerVolume
               XF86AudioRaiseVolume))
    (cl-pushnew k exwm-input-prefix-keys))
  (exwm-enable))
;; This enables the system tray for EXWM so is in included here
(require 'exwm-systemtray)
(exwm-systemtray-enable)
;; Configure deleting workspaces and swapping in case of mistake
(exwm-input-set-key (kbd "s-r") 'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-k") 'exwm-workspace-delete)
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-swap)
;; Adding locking to to the screen for when in exwm using slock
(defun exwm-slock ()
  (interactive)
  (start-process "slock" nil "slock"))
(exwm-input-set-key (kbd "s-l") 'exwm-slock)

;; Adding laptop functionality for exwm
(defconst volumeModifier "4")
(defun audio/raise-volume ()
  (interactive)
  (start-process "raise-volume" nil "pulsemixer" "--change-volume" (concat "+" volumeModifier)))

(defun audio/lower-volume ()
  (interactive)
  (start-process "lower-volume" nil "pulsemixer" "--change-volume" (concat "-" volumeModifier)))

(global-set-key (kbd "<XF86AudioRaiseVolume>") 'audio/raise-volume)
(global-set-key (kbd "<XF86AudioLowerVolume>") 'audio/lower-volume)

;; Adding a color theme that keeps things simple and organizes the color scheme
(use-package habamax-theme
  :ensure t
  :config
  (setq habamax-theme-variable-heading-heights t)
  (load-theme 'habamax t))

;; Adding dashboard to basically customize the startup screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)))
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-footer nil))

;; Adding auto-complete it will search through all buffer
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

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
	helm-imenu-fuzzy-match t))
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

;; Org mode Bullets changed the astrisks to fonts
;; kind of distracting so I removed it for now but it is nice
;; so I am debating with keeping it or not.
;; (use-package org-bullets
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

;; Adding Beacon which just shows cursor a little more disticly when switching windows
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; linux kernel C programming settings
(setq c-default-style "linux"
      c-basic-offset 8)

;;|------------------------------------------------------------------------------|
;;|                                                                              |
;;|                          Extra added variables                               |
;;|                                                                              |
;;|------------------------------------------------------------------------------|
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (beacon which-key try switch-window helm swiper avy company dashboard habamax-theme exwm xelb use-ttf use-package sudo-edit powerline popup-kill-ring dmenu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
