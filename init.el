;; Edwin's init.el
;; start date: 6/08/2019
;; last modified: 10/31/2019
;; My personal init.el to learn more about Emacs.
;; --------------------------------------------------------------------------------
;; My approach to writing this file: any new features should be under the super key
;; and some other combination. Any function that are similar will be replace for example
;; helm just has about everything and I just discovered it has helm-occur which makes helm swoop redundant.
;; --------------------------------------------------------------------------------
;; Currently working/thinking on: I haven't put this file into an .org, but may consider in the future.
;; I also want to group all setting that set keys together to be more logically grouped. Adding more comments
;; for beginners and myself. add git support to upload files. Do something about "C-x b" change buffers which
;; is still horizontal and I am not sure how I want to approach a fix on this.
;; --------------------------------------------------------------------------------

;;***** BASIC SETTING CHANGES *****

;; Continuous PDF
(setq doc-view-continuous t)
;; Show fringes
(setq visual-line-fringe-indicators t)
;; Remove the start screen and basic Emacs settings
(setq inhibit-startup-message t)
;; Instead of an annoying sound a visual action is better I feel
(setq visible-bell t)
;; Remove tool bar less clutter
(tool-bar-mode -1)
;; Remove the scroll bar
(scroll-bar-mode -1)
;; Add line number and column numbers to be displayed on Emacs bar
(line-number-mode 1)
(column-number-mode 1)
;; Add line numbers to be displayed on the left side
(global-display-line-numbers-mode 1)
;; display time standard AM/PM
(display-time-mode 1)
;;set line to always be on the screen so words sick together
(global-visual-line-mode -1)
(show-paren-mode 1)
(setq show-paren-style 'mixed)
;; set the abbrev-file-name
(setq abbrev-file-name "~/.emacs.d/lisp/my-abbrev.el")
;; set abbrevs to be global
(setq-default abbrev-mode t)
;; make bookmark-save-flagmark save every time it is changed
(setq bookmark-save-flag 1)
;; make abbrevs save on exit
(setq save-abbrevs 'silently)
;; auto fill for text files
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default fill-column 80)
;; change tabs to spaces
(setq-default indent-tabs-mode nil)
;; Setting octave mode for .m files
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; scroll one line at a time
(setq scroll-step 1)
;; do not recenter the cursor
(setq scroll-conservatively 1000)
;; auto pair mode globally matches delimiters
(electric-pair-mode 1)
;; to be able to undo window configuration
(winner-mode 1)
;; change where backups are saved
(setq backup-directory-alist `(("." . "~/.backup-saves")))
(setq backup-by-copying t)

;; ***** ORG MODE *****
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))

;; ***** BUFFER *****

;; Setting C-x k to kill the current buffer when pressed
(defun kill-current-buffer ()
  "Sets C-x k to kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
;; Setting the key to kill the buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)


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
  (kill-new (buffer-file-name)))
(global-set-key (kbd "C-c C-n") 'copy-file-path-to-kill)

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

;; ***** SPELLING HOOKS and SPELLING STUFF*****

(defun turn-on-flyspell()
  (flyspell-mode 1))

(add-hook 'text-mode-hook 'turn-on-flyspell)

(use-package langtool
  :ensure t)
(setq langtool-language-tool-jar "/opt/LanguageTool-4.7/languagetool-commandline.jar"
      langtool-disabled-rules '("WHITESPACE_RULE"
                                "EN_UNPAIRED_BRACKETS"
                                "COMMA_PARENTHESIS_WHITESPACE"
                                "EN_QUOTES"))

;; ***** WEB DEVELOPMENT *****

;; for html validation
;; Function to run Tidy HTML parser on buffer
;; NOTE: this requires external Tidy program
;; tidy -f /tmp/tidy-errs -q -i -wrap 72 -c
 (defun tidy-buffer ()
  "Run Tidy HTML parser on current buffer."
  (interactive)
  (if (get-buffer "tidy-errs") (kill-buffer "tidy-errs"))
  (shell-command (concat "tidy -f /tmp/tidy-errs -q -i -wrap " (buffer-file-name)))
  (kill-buffer "*Shell Command Output*")
  (find-file-other-window "/tmp/tidy-errs")
  (other-window 1)
  (delete-file "/tmp/tidy-errs")
  (message "buffer tidy'ed"))
(global-set-key (kbd "C-x t") 'tidy-buffer)

;;Adding web mode for web development
(use-package web-mode
  :ensure t
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode)))
)
(defun my-web-mode-hook ()
  "Hook for Web mode."
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-css-colorization t)
  (setq truncate-lines t)
  (setq web-mode-enable-auto-indention nil))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Adding emmet mode for completion
(use-package emmet-mode
  :ensure t)
(add-hook 'web-mode-hook 'emmet-mode)
(setq emmet-self-closing-tag-style " /")


;; ***** PACKAGES BELOW *****

;; restclient for web development
(use-package restclient
  :ensure t)

;; slime for lisp
(use-package slime
  :ensure t)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(use-package helm-slime
  :ensure t)

;; magit for git interaction
(use-package magit
  :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

;; Adding dashboard to basically customize the startup screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 20)
                          (bookmarks . 5)
                          (projects . 5)))
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

;; Helm awesome package that helps searching for many things
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-s") 'helm-occur)
  (setq helm-autoresize-max-height 0
	helm-autoresize-min-height 40
	helm-split-window-in-side-p t))
(helm-mode 1)
(helm-autoresize-mode 1)

;; to display function, variable and dependency tags when searching through c files
(with-eval-after-load 'helm-semantic
      (push '(c-mode . semantic-format-tag-summarize) helm-semantic-display-style)
      (push '(c++-mode . semantic-format-tag-summarize) helm-semantic-display-style))

(use-package projectile
  :ensure t)
(use-package helm-projectile
  :ensure t)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

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
;;|                            Extra added variables                             |
;;|                                                                              |
;;|------------------------------------------------------------------------------|
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (adwaita)))
 '(package-selected-packages
   (quote
    (restclient flyspell-correct-helm slime helm-projectile which-key use-package try switch-window projectile powerline magit emmet-mode dmenu dashboard beacon avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
