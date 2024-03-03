;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Zimu Zhang"
      user-mail-address "zimuzhang1@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 20 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 20))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark-high-contrast)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 't)
(display-time-mode t)
(global-visual-line-mode t)
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))
(setq auto-save-default t
      make-backup-file t)
(add-hook 'evil-insert-state-exit-hook
           (lambda ()
             (call-interactively #'save-buffer)))
(setq confirm-kill-emacs nil)
(add-hook! 'evil-insert-state-exit-hook
  (lambda ()
    (call-interactively #'save-buffer)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(add-hook! 'org-mode-hook (set (make-local-variable 'tab-width) 8))
(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords '((sequence "TODO" "STRT" "WAIT" "LOOP" "|" "DONE" "DROP")))
  (setq org-todo-keyword-faces '(("TODO" . +org-todo-cancel)
                                 ("STRT" . +org-todo-project)
                                 ("WAIT" . +org-todo-onhold)
                                 ("LOOP" . +org-todo-project))))
;;(font-lock-add-keywords 'org-mode '(("(*\?)" 0 error t)))

(after! org-journal
  (setq org-journal-dir "~/org/journal/"
        org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STRT\"|TODO=\"WAIT\"|TODO=\"LOOP\""))

(after! org-agenda
  (setq org-agenda-files '("~/UWA2024_01/"
                           "~/org/journal/"
                           "~/org/"))
  (setq org-agenda-include-diary t))

(after! org-clock
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

(use-package! org-archive
  :after org
  :config
  (setq org-archive-location "archive.org::datetree/"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(after! projectile (setq projectile-project-root-files-bottom-up
                         (remove ".git"
                                 projectile-project-root-files-bottom-up)))

(defun save-and-run ()
  "save current buffer and run \"./make\" command on current directory"
  (interactive)
  (save-buffer)
  (let ((split-width-threshold nil)
	(split-height-threshold 0))
    (save-window-excursion
      (async-shell-command "./make"))))

(global-set-key (kbd "M-m") 'save-and-run)

(let ((perhost "~/.config/doom/perhost.el"))
  (when (file-exists-p perhost)
    (load-file perhost)))
