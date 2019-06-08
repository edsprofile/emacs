;; Edwin's init.el
;; start date: 6/08/2019
;; last modified: 6/08/2019
;; My personal init.el to learn more about emacs.

;; Remove the start screen and basic emacs settings
(setq inhibit-startup-message t)
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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
(defalias 'list-buffers 'ibuffer-other-window)

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

;; Adding ivy, swiper and counsel
;; adding counsel will bring in ivy and swiper
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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "SRC" :family "Hack")))))

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel which-key use-package try switch-window s powerline org-bullets dash beacon))))
