;; Edwin's init.el
;; start date: 6/08/2019
;; last modified: 2/29/2020
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
;; Currently using Emacs 28

;;***** BASIC SETTING CHANGES *****
;; Remove the start screen and basic Emacs settings
(setq inhibit-startup-message t)
;; Instead of an annoying sound a visual action is better I feel
(setq visible-bell t)
;; Remove tool bar less clutter
(tool-bar-mode -1)
;; Remove the scroll bar
(scroll-bar-mode 1)
;; Add line number and column numbers to be displayed on Emacs bar
(line-number-mode 1)
(column-number-mode 1)
;; Add line numbers to be displayed on the left side
;; (global-display-line-numbers-mode 1)
;; display time standard AM/PM
(display-time-mode 1)
;;set line to always be on the screen so words sick together
(global-visual-line-mode 1)
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
;; being able to scroll the screen better
;; got from http://pragmaticemacs.com/emacs/scrolling-and-moving-by-line/
;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; ***** OTHER HOOKS *****
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'js-mode-hook
          (lambda ()
            (setq js-switch-indent-offset 4)))

;; ***** ORG MODE *****
;; Org mode changes
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %b %d %Y>" . "<%D %a %b %d %Y %I:%M%p>"))
;; Other org settings
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)
(add-hook 'org-mode-hook 'org-indent-mode)

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

;; move lines up or down got this from
;; https://krsoninikhil.github.io/2018/12/15/easy-moving-from-vscode-to-emacs/

;; (defun move-line-down ()
;;    (interactive)
;;    (let ((col (current-column)))
;;      (save-excursion
;;        (forward-line)
;;        (transpose-lines 1))
;;      (forward-line)
;;      (move-to-column col)))
;; (global-set-key (kbd "C-M-n") 'move-line-down)

;; (defun move-line-up ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines -1))
;;     (forward-line -2)
;;     (move-to-column col)))
;; (global-set-key (kbd "C-M-p") 'move-line-up)

(defun duplicate-line ()
  (interactive)
  (let ((col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (move-to-column col)))
(global-set-key (kbd "C-M-d") 'duplicate-line)

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
(require 'json)
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

(defun turn-on-flyspell ()
  (flyspell-mode 1))

(defun turn-off-flyspell ()
  (flyspell-mode 0))

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'sgml-mode-hook 'turn-off-flyspell)

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
(global-set-key (kbd "C-x t b") 'tidy-buffer)

;;Adding web mode for web development
(use-package web-mode
  :ensure t
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode)))

(defun my-web-mode-hook ()
  "Hook for Web mode."
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-indention nil))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Adding emmet mode for completion
(use-package emmet-mode
  :ensure t
  :bind*
  (("C-RET" . emmet-expand-line)
   ("C-(" . emmet-prev-edit-point)
   ("C-)" . emmet-next-edit-point))
  :hook
  ((sgml-mode . emmet-mode)
   (css-mode . emmet-mode)
   (web-mode . emmet-mode))
  :init
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-self-closing-tag-style " /"))


;; ***** PACKAGES BELOW *****
(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 1)
  (setq writeroom-mode-line-toggle-position 'mode-line-format))

(use-package auctex
  :defer t
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t))

;; Trying treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t d"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; Python config
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

(use-package py-autopep8
  :ensure t)

(use-package elpy
  :ensure t
  :config
  (setq elpy-eldoc-show-current-function nil))
(elpy-enable)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)

;; (use-package flycheck
;;   :ensure t)
;; (when (load "flycheck" t t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package ein
  :ensure t)

;; learning org mode
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; move text
(use-package move-text
  :ensure t
  :bind
  (("C-M-p" . move-text-up)
   ("C-M-n" . move-text-down)))

;; vterm
(use-package vterm
  :ensure t
  :load-path "~/.emacs.d/emacs-libvterm/")

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
;; only on the current screen just press "M-s"
;; then enter the letter you would like to go to.
;; recent change just M-s easier to press
(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char-2))

;; Helm awesome package that helps searching for many things
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-c f f") 'helm-find-files)
  (global-set-key (kbd "C-c h b") 'helm-buffers-list)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-b") 'helm-mini)
  (global-set-key (kbd "C-s") 'helm-occur)
  (global-set-key (kbd "C-c h f") 'helm-find)
  (global-set-key (kbd "C-c h g") 'helm-google-suggest)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)
  (setq helm-split-window-in-side-p t))

(helm-mode 1)
(helm-autoresize-mode 1)

;; to display function, variable and dependency tags when searching through c files
(with-eval-after-load 'helm-semantic
      (push '(c-mode . semantic-format-tag-summarize) helm-semantic-display-style)
      (push '(c++-mode . semantic-format-tag-summarize) helm-semantic-display-style))

(use-package projectile
  :ensure t
  :config
  (global-set-key (kbd "C-c p h") 'helm-projectile)
  (global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
  (global-set-key (kbd "C-c p b") 'helm-projectile-switch-to-buffer)
  (global-set-key (kbd "C-c p p") 'helm-projectile-switch-project)
  (global-set-key (kbd "C-c p g") 'helm-projectile-grep)
  (setq projectile-switch-project-action 'helm-projectile-find-file))

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
 '(custom-enabled-themes '(adwaita))
 '(ein:output-area-inlined-images t)
 '(eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
 '(elpy-modules
   '(elpy-module-company elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults))
 '(elpy-rpc-python-command "python3")
 '(elpy-rpc-virtualenv-path 'default)
 '(global-company-mode t)
 '(global-eldoc-mode nil)
 '(package-selected-packages
   '(which-key web-mode use-package try switch-window sudo-edit smooth-scrolling restclient py-autopep8 org-bullets move-text magit langtool impatient-mode helm-swoop helm-slime helm-projectile ess emmet-mode dashboard beacon))
 '(python-shell-interpreter "python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
