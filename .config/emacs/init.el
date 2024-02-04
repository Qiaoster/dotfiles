;;Package Management
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;UsePackage
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package impatient-mode)
(defun markdown-html (buffer)
    (princ (with-current-buffer buffer
      (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer)))

(use-package minimap)
(use-package tree-sitter-langs)
(use-package tree-sitter
  :hook (python-mode  . tree-sitter-hl-mode)
  :config  (global-tree-sitter-mode))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun efs/lsp-mode-setup()
  (setq lsp-headerline-breadcrumbtysegments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package treemacs)
(use-package lsp-treemacs
  :after lsp)
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'right)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package ccls)
(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :config
  (setq python-indent-level 4))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-function] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode)
  :config
  (setq which-key-idle-delay 1.5))

(use-package hydra)
(defhydra hydra-text-scale nil
  "scale text"
  ("n" text-scale-increase "in")
  ("e" text-scale-decrease "out")
  ("i" nil "finished" :exit t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/")
    (setq projectile-project-search-path '("~/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch))

(defun org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-image-actual-width 400)
  
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  
  (setq org-todo-keywords
	'((sequence "TODO(t)" "InProgress(p)" "|" "Done(d!)")))
  
  (setq org-agenda-files
	'("~/.config/CommandTower.org"
	  "~/UWA2023-Sem1/AUTO4508_Robots/notes.org"
	  "~/UWA2023-Sem1/CITS4401_Software/notes.org"
	  "~/UWA2023-Sem1/CITS4402_CompVision/notes.org"
	  "~/UWA2023-Sem1/CITS5508_ML/notes.org")))

(use-package org-journal
  :ensure t
  :defer t
  :init
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/journal/"
	org-journal-date-format "%A, %d %B %Y"))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;;key bindings
;;IDE Attempts
;; (defun edit-code-layout()
;;   "set current frame to edit code layout"
;;   (interactive)
;;   (delete-other-windows)
;;   (split-window-horizontally)
;;   (split-window-vertically)
;;   ;(other-window 1)
  
;;   (let* ((frame-width (frame-width))
;; 	 (desired-width (/ frame-width 4)))
;;     (enlarge-window-horizontally (- desired-width (window-width)))
;;     (call-interactively 'lsp)
;;     (call-interactively 'flymake-show-buffer-diagnostics))
;;   )

;; (defun IDE ()
;;   (interactive)
;;   (let ()
;;     (let ((gdb-process (start-process "gdb" "*gdb*" "gdb" "-i=mi" "-ex" "file exe")))
;;       (process-send-string gdb-process "y\n"))
;;     (split-window-horizontally)
;;     (other-window 1)
;;     (gdb-display-locals-buffer)
;;     (other-window -1)))
    

;; ;; compile current project with make file in current folder
;; (defun run-with-gdb ()
;;   "Run GDB on the specified executable"
;;   (interactive)
;;   (gud-gdb (concat "gdb -i=mi -ex \"file exe\"")))

;; (global-set-key (kbd "M-C-d") 'run-with-gdb)

(defun save-and-run ()
  "save current buffer and run \"./make\" command on current directory"
  (interactive)
  (save-buffer)
  (let ((split-width-threshold nil)
	(split-height-threshold 0))
    (save-window-excursion
     (async-shell-command "./make"))))

(global-set-key (kbd "M-m") 'save-and-run)

;; compile and run current c main
(global-set-key (kbd "M-r")
		(lambda () (interactive)
		  (save-buffer)
		  (eshell-command "tcc -run -Wunsupported -Wall -Werror main.c ")));;(shell-command "tcc -run -L/usr/local/lib -leyesim ctest.c")))
(global-set-key (kbd "M-a") 'beginning-of-visual-line)
(global-set-key (kbd "C-a") 'back-to-indentation)

(defun open-next-line ()
  "go to end of line and make a newline"
  (interactive)
  (end-of-visual-line)
  (newline)
  (c-indent-line-or-region))
(global-set-key (kbd "C-o") 'open-next-line)
(defun open-previous-line ()
  (interactive)
  (previous-line)
  (open-next-line))
(global-set-key (kbd "M-o") 'open-previous-line)

(defun shift-line-up ()
  (interactive)
  (kill-whole-line)
  (previous-line)
  (yank)
  (previous-line)
  (c-indent-line-or-region))
(global-set-key (kbd "M-C-u") 'shift-line-up)

(defun shift-line-down ()
  (interactive)
  (kill-whole-line)
  (next-line)
  (yank)
  (previous-line)
  (c-indent-line-or-region))
(setq inhibit(global-set-key (kbd "M-C-e") 'shift-line-down)

;;Init
-startup-message t)
(find-file "~/.config/emacs/init.el")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode t)
(fringe-mode 0)
(tooltip-mode -1)
(setq visible-bell t)
(setq-default indent-tabs-mode t)
(setq-default c-basic-offset 4)
(setq-default c-indentation-style 'cc-mode)
(setq-default gdb-many-windows t)
(desktop-save-mode 1)
;;Line Numbers
(setq column-number-mode t)
(global-display-line-numbers-mode t)
;;(setq display-line-numbers-type 'default)

(setq display-time-day-and-date t)
(display-time)
(setq display-time-24hr-format t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		treemacs-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;Appearance
(use-package all-the-icons)
(use-package doom-themes)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package rainbow-mode)
(rainbow-mode 1)

(load-theme 'doom-solarized-dark-high-contrast t)
(set-face-attribute 'default nil :font "Fira Code" :height 200)
(set-frame-font "Fira Code 20")
(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (set-frame-font "Fira Code 20")
	    ))


;;Generated stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-mode impatient-mode markdown-preview-mode markdown-soma grip-mode minimap ccls evil-nerd-commenter lsp-treemacs treemacs lsp-ui company-box company python-mode tree-sitter-langs tree-sitter org-journal magit counsel-projectile projectile hydra all-the-icons doom-themes helpful counsel ivy-rich which-key rainbow-delimiters doom-modeline ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
