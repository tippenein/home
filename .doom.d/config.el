;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Brady Ouren"
      user-mail-address "brady.ouren@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;
(setq doom-theme 'doom-material)
;; 'doom-one
;; 'doom-one-light
;; 'doom-acario-light

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


(map! :leader
      "d" #'dired)

(map! :leader
      "v e" #'evil-multiedit-match-all)

(map! :leader
      "e l" #'flycheck-list-errors)

;; Rust
(map! :leader
      "r c" #'rustic-compile)
(map! :leader
      "r v" #'rustic-cargo-run)

;; macros
(map! :leader
      "z s" #'kmacro-start-macro)
(map! :leader
      "z e" #'kmacro-end-macro)
(map! :leader
      "z p" #'kmacro-end-and-call-macro)
(map! :leader
      "z z" #'call-last-kbd-macro)

;; git
(map! :leader
      "g p" #'magit-push)

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))
(map! :leader
      "p K" #'doom/kill-other-buffers)

;; fuck go
;; go mode
;; (defun my-go-mode-hook ()
;;   ; Call Gofmt before saving
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   ; Customize compile command to run go build
;;   (if (not (string-match "go" compile-command))
;;       (set (make-local-variable 'compile-command)
;;            "go build -v && go test -v"))
;;   ; Godef jump key binding
;; )
;; (add-hook 'go-mode-hook 'my-go-mode-hook)

(setq haskell-stylish-on-save t)
;;
(map! :leader
      "A" #'projectile-ripgrep)

;; SURROUND
(after! evil-surround
  (let ((pairs '((?j "(" . ")")
                 (?k "[" . "]")
                 (?l "{" . "}")
                 (?m "'" . "'")
                 (?n "\"" . "\""))))
    (prependq! evil-surround-pairs-alist pairs)
    (prependq! evil-embrace-evil-surround-keys (mapcar #'car pairs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
