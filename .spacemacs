;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(racket
     go
     python
     helm
     auto-completion
     syntax-checking
     better-defaults
     c-c++
     emacs-lisp
     (git :variables
          git-gutter-use-fringe t)
     (org :variables org-projectile-file ".todo.org")
     sql
     react
     yaml
     purescript
     rust
     lsp
     (haskell :variables
              haskell-enable-ghc-mod-support nil)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
    ripgrep
    magithub
    )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   ;; (dotspacemacs-delete-orphan-packages t)
   ))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."

  (setq-default
   ;; This setq-default sexp is an exhaustive list of all the supported
   ;; spacemacs settings.
   dotspacemacs-elpa-https t
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(bookmarks)
   ;; js2-basic-offset 2
   ;; js-indent-level 2
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list
   dotspacemacs-themes '(
                         ;; material
                         ;; leuven
                         sanityinc-solarized-light
                         sanityinc-solarized-dark
                         ;; monokai
                         )
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Inconsolata"
                               :size 20
                               :weight normal
                               :width normal
                              )
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 95
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 80
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("rg" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil

   dotspacemacs-frame-title-format "%a - emacs"
   )
  ;; User initialization goes here
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  ;; (global-linum-mode)
  (add-to-list 'exec-path "~/.local/bin/")
  (find-file "~/.todo.org")
  ;; (push '("ensime" . "melpa-stable") package-pinned-packages)
  ;; (setq x-select-enable-clipboard t)
  (setq helm-grep-ag-command "rg --vimgrep --no-heading --smart-case")
  ;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;; movement
  (define-key evil-normal-state-map (kbd "SPC x a a") 'align-regexp)
  (define-key evil-normal-state-map (kbd "SPC x a A") 'align)
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  (define-key evil-normal-state-map (kbd "SPC , a") 'projectile-ripgrep)
  (define-key evil-normal-state-map (kbd "SPC , d") 'dired)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "RET") 'spacemacs/evil-search-clear-highlight)

  (define-key evil-normal-state-map (kbd "SPC o b") 'helm-buffers-list)
  (define-key evil-normal-state-map (kbd "SPC g c") 'magit-commit)
  ;; rust
  (define-key evil-normal-state-map (kbd "SPC r c") 'rust-compile)
  (define-key evil-normal-state-map (kbd "SPC r v") 'rust-run)
  ;; purescript
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation)))
  (define-key evil-normal-state-map (kbd "SPC , p m") 'purescript-mode)
  ;; (setq psc-ide-purs-executable "node_modules/.bin/purs")
  (setq psc-ide-use-npm-bin t)
  ;; haskell
  (define-key evil-normal-state-map (kbd "SPC , h n") 'ghc-goto-next-error)
  (define-key evil-normal-state-map (kbd "SPC , h p") 'ghc-goto-prev-error)
  (define-key evil-normal-state-map (kbd "SPC , h h") 'haskell-hoogle)
  (define-key evil-normal-state-map (kbd "SPC , h m") 'haskell-mode)
  ;; (add-hook 'haskell-mode-hook
  ;;           (lambda ()
  ;;             (company-mode)
  ;;             (flycheck-mode)
  ;;             (turn-on-haskell-indentation)))
  ;; (custom-set-variables
  ;;  '(haskell-stylish-on-save t))
  ;; macros
  (define-key evil-normal-state-map (kbd "SPC , q s") 'kmacro-start-macro)
  (define-key evil-normal-state-map (kbd "SPC , q e") 'kmacro-end-macro)
  (define-key evil-normal-state-map (kbd "SPC , q p") 'kmacro-end-and-call-macro)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'call-last-kbd-macro)
  ;; bookmarks
  (define-key evil-normal-state-map (kbd "SPC , b j") 'bookmark-jump)
  (define-key evil-normal-state-map (kbd "SPC , b s") 'bookmark-set)
  ;; misc

  (defun turn-off-evil-auto-indent ()
    (setq-local evil-auto-indent nil))

  (add-hook 'haskell-mode-hook #'turn-off-evil-auto-indent)
  (add-hook 'purescript-mode-hook #'turn-off-evil-auto-indent)
  (add-to-list 'spacemacs-indent-sensitive-modes 'purescript-mode)
  (add-to-list 'spacemacs-indent-sensitive-modes 'haskell-mode)
  ;; (setq projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s")
  ;; (setq x-select-enable-clipboard t)
  ;; (setq tags-revert-without-query 1)
)

;; ;let spacemacs load crap or whatever
;; (setq custom-file "/dev/null")
;; (load custom-file)
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 ;; '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (racket-mode faceup company-quickhelp godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc flycheck-golangci-lint company-go go-mode yapfify stickyfunc-enhance sphinx-doc pytest pyenv-mode py-isort poetry pippel pipenv pyvenv pip-requirements lsp-python-ms lsp-pyright live-py-mode importmagic epc ctable concurrent helm-pydoc helm-gtags helm-cscope xcscope ggtags dap-mode bui cython-mode counsel-gtags counsel swiper ivy company-anaconda blacken anaconda-mode pythonic lsp-ui lsp-treemacs lsp-origami origami helm-lsp ccls yasnippet-snippets yaml-mode ws-butler writeroom-mode winum which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unfill undo-tree treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toml-mode toc-org symon symbol-overlay string-inflection sql-indent spaceline-all-the-icons smeargle ron-mode rjsx-mode ripgrep restart-emacs rainbow-delimiters racer psci psc-ide prettier-js popwin pcre2el password-generator paradox overseer orgit org-superstar org-rich-yank org-projectile org-present org-pomodoro org-mime org-download org-cliplink org-brain open-junk-file npm-mode nodejs-repl nameless mwim move-text magithub magit-svn magit-section magit-gitflow macrostep lsp-haskell lorem-ipsum livid-mode link-hint json-navigator json-mode js2-refactor js-doc indent-guide hybrid-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-rtags helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-ls-git helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate google-c-style golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link fuzzy forge font-lock+ flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-haskell flycheck-elsa flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu emr elisp-slime-nav editorconfig dumb-jump dotenv-mode disaster dired-quick-sort diminish devdocs define-word dante cpp-auto-include company-ycmd company-rtags company-cabal company-c-headers column-enforce-mode color-theme-sanityinc-solarized cmm-mode clean-aindent-mode centered-cursor-mode cargo auto-yasnippet auto-highlight-symbol auto-compile attrap aggressive-indent ace-link ace-jump-helm-line ac-ispell)))
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
