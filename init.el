;; Edwin's init.el
;; start date: 6/08/2019
;; last modified: 6/09/2019
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

;; - Reason: The reason for this was it just feels more like what emacs would logically do.

;; Remove the start screen and basic emacs settings
(setq inhibit-startup-message t)
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
;; Debating if I should add time I think I would need a better for it but I like the current bar.
;; (setq display-time-24hr-format t)
;; (setq display-time-format "%H:%M - %d %B %Y")
;; (setq display-time-mode 1)
;; Allow buffer listing and matching in the minibuffer when using e.g. C-x b
(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;; Buffer stuff
;; Setting C-x k to kill the current buffer when pressed
(defun kill-current-buffer ()
  "Sets C-x k to kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
;; Setting the key to kill the buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; Setting ibuffer to be the default buffer and opens in other window
(defalias 'list-buffers 'ibuffer)

;; ***** CUSTOM FUNCTION *****

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

;; Adding use-ttf to keep font consistent on different machines
(use-package use-ttf
  :ensure t)
(setq use-tff-default-ttf-font-name "Ubuntu mono")

;; Adding sudo edit for editing in sudo
(use-package sudo-edit
  :ensure t)

;; Adding vertical ido just easier to look at a vertical list than a horizontal one
(use-package ido-vertical-mode
  :ensure t)
(ido-vertical-mode 1)

;; Adding dmenu to easier see launchable applications in Exwm
(use-package dmenu
  :ensure t
  :bind
    ("s-d" . 'dmenu))

;; Adding a shell emulator this really isn't a package
;; so I will probably move it somewhere else in the furture.
(defvar bash-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list bash-shell)))
(ad-activate 'ansi-term)
(global-set-key (kbd "s-e") 'ansi-term)

;; Adding popup-kill-ring to show the kill ring
(use-package popup-kill-ring
  :ensure t
  :bind
  ("M-y" . popup-kill-ring))

;; Adding EXWM an emacs windows manager for those long work days
;; for this to work you need the proper file e.g. /usr/share/xsessions/emacs.desktop
;; EXWM may need to get its own section
(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (exwm-config-default)
  (exwm-config-ido)
  (dotimes (i 10)
      (exwm-input-set-key (kbd (format "s-%d" i))
                          `(lambda ()
                             (interactive)
                             (exwm-workspace-switch-create ,i))))
  ;; I tried to set up the media key for this but could not figure it out
  ;; if anyone figues out let me know I also tried another package called desktop enviroment
  ;; it worked better but again not fully.
  (dolist (k '(XF86AudioLowerVolume
               XF86AudioRaiseVolume
               XF86PowerOff
               XF86AudioMute
               XF86AudioPlay
               XF86AudioStop
               XF86AudioPrev
               XF86AudioNext
               XF86ScreenSaver
               XF68Back
               XF86Forward
               Scroll_Lock
               print))
    (cl-pushnew k exwm-input-prefix-keys))
  (exwm-input-set-key (kbd "s-i")
                        (lambda (command)
                          (interactive (list (read-shell-command "$ ")))
                          (start-process-shell-command command nil command))))
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
  (setq dashboard-startup-banner 'official)
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

;; Adding ivy, swiper and counsel
;; Adding counsel will bring in ivy and swiper
;; Swiper allows for searching through the file simliar to isearch but better
;; Counsel allows for searching through M-x functions better by bringing up a menu and matching
;; Counsel also does the same for files.
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

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
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

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


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "DAMA" :family "Ubuntu mono")))))


;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (better-shell use-ttf desktop-environment desktop-enviroment fancy-battery ido-vertical-mode dmenu which-key use-package try switch-window s org-bullets dashboard dash counsel company beacon avy))))
