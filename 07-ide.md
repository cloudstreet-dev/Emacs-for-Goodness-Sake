# Chapter 7: Emacs as an IDE
## Who Needs Visual Studio When You Have Actual Magic?

Here's a secret: Emacs was doing IDE features before "IDE" was even a term. Syntax highlighting? Emacs had it. Integrated debugging? Check. Version control? Emacs users were using VC-mode while others were still typing git commands. The difference is that Emacs doesn't just bolt on features—it absorbs them, improves them, and makes them feel like they were always meant to be there.

This chapter will turn your Emacs into a development environment so powerful, your VS Code friends will accuse you of witchcraft.

### LSP: The Modern Magic

Language Server Protocol changed everything. Now Emacs can have the same intelligent code completion, refactoring, and analysis as any modern IDE:

```elisp
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (go-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-idle-delay 0.6
        lsp-log-io nil
        lsp-prefer-capf t
        lsp-enable-symbol-highlighting t
        lsp-enable-on-type-formatting nil
        lsp-signature-auto-activate t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.6
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t))
```

Now you have:
- **Hover documentation**: `K` or mouse hover
- **Find definitions**: `M-.`
- **Find references**: `M-?`
- **Rename symbol**: `C-c l r r`
- **Format buffer**: `C-c l = =`
- **Code actions**: `C-c l a a`

🚸 **IDE Refugee Note**: This is the same LSP your VS Code uses. Same servers, same intelligence, but with Emacs keybindings and integration.

### Projectile: Project Management Done Right

```elisp
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien))

;; Integration with consult
(use-package consult-projectile
  :ensure t
  :bind ("C-c p f" . consult-projectile-find-file))
```

Essential projectile commands:
- `C-c p f` - Find file in project
- `C-c p p` - Switch project
- `C-c p g` - Grep in project
- `C-c p c` - Compile project
- `C-c p t` - Test project
- `C-c p r` - Replace in project

### Magit: Git So Good It Hurts

Magit isn't just a git interface—it's better than git itself:

```elisp
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Show changes in fringe
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; Git time machine
(use-package git-timemachine
  :ensure t
  :bind ("C-c g t" . git-timemachine))
```

The Magit workflow:
1. `C-x g` - Open Magit status
2. `s` - Stage changes (or `S` for all)
3. `c c` - Commit (opens commit message buffer)
4. `C-c C-c` - Finish commit
5. `P p` - Push

But that's just the beginning:
- `b b` - Switch branch
- `b c` - Create branch
- `m m` - Merge
- `r i` - Interactive rebase
- `l l` - View log
- `d d` - Diff

🤔 **Why Though?** Magit makes git operations visual and interactive. You can stage individual hunks, lines, or even parts of lines. It's like having a git GUI that's actually faster than the command line.

### Company: Autocompletion That Just Works

```elisp
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-quick-access t
        company-tooltip-align-annotations t))

;; Better looking tooltips
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; Language specific backends
;; Note: company-tabnine is deprecated, use copilot.el or codeium.el for AI completions
;; Example with codeium:
;; (use-package codeium
;;   :init
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))
```

### Flycheck: Real-time Error Checking

```elisp
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.3
        flycheck-check-syntax-automatically '(save mode-enabled)))

;; Show errors in popup
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))
```

### Debugging: Yes, Inside Emacs

```elisp
;; DAP mode for debugging
(use-package dap-mode
  :ensure t
  :config
  (dap-auto-configure-mode)
  
  ;; Python debugging
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  
  ;; JavaScript debugging  
  (require 'dap-node)
  (dap-node-setup))
```

Debug workflow:
1. Set breakpoint: `C-c d b`
2. Start debugging: `C-c d d`
3. Step over: `n`
4. Step into: `i`
5. Continue: `c`
6. Inspect variables: hover or `C-c d i`

### Language-Specific IDE Features

#### Python Development

```elisp
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :config
  (setq python-indent-offset 4))

(use-package poetry  ; Poetry support
  :ensure t
  :hook (python-mode . poetry-tracking-mode))

(use-package blacken  ; Auto-format with black
  :ensure t
  :hook (python-mode . blacken-mode))

(use-package py-isort  ; Sort imports
  :ensure t
  :hook (before-save . py-isort-before-save))

(use-package pytest  ; Test runner
  :ensure t
  :bind (:map python-mode-map
              ("C-c t" . pytest-one)
              ("C-c T" . pytest-all)))
```

#### JavaScript/TypeScript

```elisp
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

(use-package prettier-js
  :ensure t
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

(use-package npm-mode  ; npm commands
  :ensure t
  :hook ((js2-mode . npm-mode)
         (typescript-mode . npm-mode)))
```

#### Rust Development

```elisp
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))
```

### The Integrated Terminal

```elisp
;; Better terminal emulation
(use-package vterm
  :ensure t
  :bind ("C-c t" . vterm)
  :config
  (setq vterm-max-scrollback 10000))

;; Or use built-in ansi-term
(defun my-term ()
  "Open terminal in current project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (ansi-term (getenv "SHELL"))))

(global-set-key (kbd "C-c T") 'my-term)
```

### Compilation Mode: Build Without Leaving

```elisp
;; Smart compile command
(use-package smart-compile
  :ensure t
  :bind ("C-c c" . smart-compile))

;; Or define project-specific compile commands
(dir-locals-set-class-variables
 'python-project
 '((nil . ((compile-command . "python -m pytest")))))

;; Better compilation output
(use-package fancy-compilation
  :ensure t
  :config
  (fancy-compilation-mode))
```

### Snippets: Code Templates

```elisp
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)
```

Create your own snippets:
1. `M-x yas-new-snippet`
2. Define template:
```
# -*- mode: snippet -*-
# name: def
# key: def
# --
def ${1:function_name}(${2:args}):
    """${3:docstring}"""
    ${0:pass}
```

### Docker Integration

```elisp
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose\\.yml\\'")
```

### REST Client

```elisp
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

;; Example .http file:
;; GET https://api.github.com/users/torvalds
;;
;; ###
;;
;; POST https://httpbin.org/post
;; Content-Type: application/json
;;
;; {
;;   "name": "Emacs",
;;   "awesome": true
;; }
```

### The Complete IDE Configuration

Here's a complete IDE setup for multiple languages:

```elisp
;;; IDE Configuration

;; Performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; LSP
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (js2-mode . lsp-deferred))
  :config
  (setq lsp-prefer-capf t
        lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t))

;; Completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1))

;; Error checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Projects
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

;; Debugging
(use-package dap-mode
  :ensure t
  :config
  (dap-auto-configure-mode))

;; Snippets
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))
```

### Remote Development

```elisp
;; TRAMP for remote editing
(setq tramp-default-method "ssh")

;; Open remote file:
;; C-x C-f /ssh:user@host:/path/to/file

;; LSP over TRAMP
(use-package lsp-mode
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pylsp-remote)))
```

### Performance Tips

```elisp
;; Increase GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 100000000)))

;; Optimize for long lines
(setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Disable line numbers in large files
(add-hook 'prog-mode-hook
          (lambda ()
            (when (> (buffer-size) 100000)
              (display-line-numbers-mode -1))))
```

### 🎮 **IDE Exercises**

1. **The Polyglot**: Set up LSP for 3 different languages
2. **The Git Master**: Do an interactive rebase entirely in Magit
3. **The Debugger**: Debug a program with breakpoints and variable inspection
4. **The Architect**: Create a multi-file project template with yasnippet
5. **The DevOps**: Edit a file on a remote server via TRAMP

### What's Next?

Chapter 8 will show you how Emacs can be your email client, IRC client, RSS reader, and more. Because why switch applications when you can do everything in Emacs?

But here's the thing: after setting up Emacs as an IDE, you might find it hard to go back. It's not just the features—it's the integration. Your git client knows about your projects. Your terminal is in the same window as your code. Your notes are a keystroke away. This isn't just an IDE; it's a development environment that grows with you.

---

*"I don't use an IDE. I use Emacs. It's like an IDE, but I can also read my email in it."*
—Every Emacs developer

---

### Chapter 7 Summary

- **LSP** brings modern IDE intelligence
- **Magit** makes git a joy
- **Projectile** manages projects brilliantly
- **Company** completes everything
- **Flycheck** catches errors instantly
- **DAP** enables visual debugging
- Language-specific tools for every ecosystem
- Everything integrates with everything
- It's not an IDE, it's **your** IDE