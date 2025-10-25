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
(setq doom-font (font-spec :family "Monaspace Neon" :size 16 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Monaspace Argon" :size 16))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-fairy-floss)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setenv "SHELL" "/bin/bash")
(setq display-line-numbers-type 't)
(setq ispell-dictionary "en_AU-large")
(display-time-mode t)
(global-visual-line-mode t)
(good-scroll-mode 1)
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'interpreter-mode-alist '("dash" . sh-mode))
(setq auto-save-default t
      make-backup-file t)
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (when (and (buffer-file-name)  ; Only save actual file buffers
                       (not (string-match-p "\\*.*\\*" (buffer-name)))) ; Skip special buffers
              (call-interactively #'save-buffer))))
(setq confirm-kill-emacs nil)
;;(add-hook! 'evil-insert-state-exit-hook
;;  (lambda ()
;;    (call-interactively #'save-buffer)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(add-hook! 'org-mode-hook (set (make-local-variable 'tab-width) 8))
(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords '((sequence "TODO" "LOOP" "WAIT" "|" "DONE" "DROP")))
  (setq org-todo-keyword-faces '(("TODO" . +org-todo-cancel)
                                 ;; ("STRT" . +org-todo-project)
                                 ("WAIT" . +org-todo-onhold)
                                 ("DROP" . +org-todo-onhold)
                                 ("LOOP" . +org-todo-project))))
;;(font-lock-add-keywords 'org-mode '(("(*\?)" 0 error t)))
(use-package! qml-ts-mode
  :after lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(qml-ts-mode . "qml-ts"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("qmlls" "-E"))
                    :activation-fn (lsp-activate-on "qml-ts")
                    :server-id 'qmlls))
  (add-hook 'qml-ts-mode-hook (lambda ()
                                (setq-local electric-indent-chars '(?\n ?\( ?\) ?{ ?} ?\[ ?\] ?\; ?,))
                                (lsp-deferred))))

(use-package! treesit-auto
  :config
  (global-treesit-auto-mode))

(after! org-journal
  (setq org-journal-dir "~/org/journal/"
        org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STRT\"|TODO=\"WAIT\"|TODO=\"LOOP\""))

(after! org-agenda
  (setq org-agenda-files '("~/UWA2024-Sem2/"
                           "~/UWA2024-Sem2/CITS5501_Software_Testing_and_Quality_Assurance/"
                           "~/UWA2024-Sem2/GENG5512_MPE_Engineering_Research_Project_part2/"
                           "~/UWA2024-Sem2/CITS5552_Software_Engineering_Design_Project_part2/"
                           "~/org/journal/"
                           "~/org/"))
  (setq org-agenda-include-diary t))

(after! org-clock
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))
(add-hook 'org-mode-hook (lambda ()
  (add-hook 'before-save-hook 'org-update-all-dblocks nil 'local)))
(setq org-duration-format 'h:mm)
(setq wl-copy-process nil)
(setq org-hierarchical-todo-statistics nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe
                                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil
    (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

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
  "save current buffer and \"make\" from current directory"
  (interactive)
  (save-buffer)
  ;; Kill running async processes
  (when (get-buffer "*Async Shell Command*")
    (let ((proc (get-buffer-process "*Async Shell Command*")))
      (when proc
        (delete-process proc))))
  (let ((split-width-threshold nil)
	(split-height-threshold 0)
        (file-ext (file-name-extension (buffer-file-name))))

    (save-window-excursion
      (cond
       ((file-exists-p "make")
        (async-shell-command "./make"))

       ((string= file-ext "c")
        (async-shell-command (format "tcc -run %s" (buffer-file-name))))

       ((string= file-ext "go")
        (async-shell-command (format "go run %s" (buffer-file-name))))

       ((string= file-ext "zig")
        (async-shell-command (format "zig run %s" (buffer-file-name))))

       (t
        (message "Unsupported file type: %s" file-ext))))))

(global-set-key (kbd "M-m") 'save-and-run)
(global-set-key (kbd "M-/") 'comment-line)
(global-set-key (kbd "C-SPC") 'set-mark-command)

(let ((perhost "~/.config/doom/perhost.el"))
  (when (file-exists-p perhost)
    (load-file perhost)))
