# Chapter 4: Configuration Basics
## Making Emacs Yours (And Only Yours)

Every Emacs user's configuration is unique, like a fingerprint or a particularly complex snowflake. Your config file is where Emacs stops being a generic text editor and starts being YOUR text editor. It's where the magic happens. It's also where things break spectacularly at 3 AM when you really need to get work done.

Welcome to the world of Emacs configuration, where "I'll just make this one small change" leads to four hours of Elisp debugging and somehow you've ended up writing a Twitter client.

### Where Configuration Lives

Emacs looks for configuration in these places (in order):

1. `~/.emacs.d/init.el` (modern, recommended)
2. `~/.emacs.el` (older style)
3. `~/.emacs` (ancient, avoid)

🚸 **IDE Refugee Note**: Unlike VS Code where settings are JSON, Emacs config is actual code (Elisp). This means your config can do literally anything. It also means you can break literally everything.

### Your First Real Config File

Let's build a config from scratch. Create `~/.emacs.d/init.el`:

```elisp
;;; init.el --- My Emacs configuration
;;; Commentary:
;; This is where the magic happens

;;; Code:

;; Who are you?
(setq user-full-name "Your Name"
      user-mail-address "you@example.com")

;; Where are we?
(setq default-directory "~/")

;; UI Improvements
(setq inhibit-startup-message t)  ; No splash screen
(tool-bar-mode -1)                ; No tool bar
(menu-bar-mode -1)                ; No menu bar
(scroll-bar-mode -1)              ; No scroll bar
(global-display-line-numbers-mode 1) ; Line numbers everywhere

;; Better defaults
(setq-default indent-tabs-mode nil)  ; Spaces, not tabs
(setq-default tab-width 4)           ; 4 spaces per tab
(setq require-final-newline t)       ; Files end with newline
(setq backup-directory-alist         ; Don't litter my directories
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Enable useful disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Make Emacs more responsive
(setq gc-cons-threshold 100000000)  ; 100 MB before garbage collection
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; Remember recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; Save cursor position in files
(save-place-mode 1)

;; Update buffers when files change on disk
(global-auto-revert-mode 1)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Better scrolling
(setq scroll-conservatively 101)  ; Don't jump around
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;;; init.el ends here
```

🤔 **Why Though?** Every line here fixes a default that hasn't aged well. Emacs defaults were chosen when monitors were 80x24 characters and memory was measured in kilobytes. We can do better now.

### The Customization Interface (The GUI Way)

Emacs has a built-in customization interface. Try it:

```
M-x customize
```

This opens a GUI(ish) interface for changing settings. When you save changes, it adds code to your init file that looks like:

```elisp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 '(column-number-mode t)
 '(display-time-mode t))
```

🎯 **Pro Tip**: Keep Custom variables in a separate file:
```elisp
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
```

### Hooks: Making Things Happen Automatically

Hooks are lists of functions that run at specific times. Every major mode has a hook:

```elisp
;; Run these when entering Python mode
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width 4)))

;; Multiple functions on one hook
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Or use a named function
(defun my-text-mode-setup ()
  "My settings for text modes."
  (turn-on-auto-fill)
  (flyspell-mode 1))

(add-hook 'text-mode-hook 'my-text-mode-setup)
```

### Keybinding Configuration

Make Emacs respond to your will:

```elisp
;; Simple global keybinding
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; Function keys
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'toggle-truncate-lines)

;; Rebind existing commands
(global-set-key (kbd "C-x C-b") 'ibuffer)  ; Better buffer list

;; Mode-specific keybinding
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-buffer)

;; Create your own keymap
(define-prefix-command 'my-custom-map)
(global-set-key (kbd "C-c m") 'my-custom-map)
(define-key my-custom-map (kbd "s") 'shell)
(define-key my-custom-map (kbd "t") 'ansi-term)
```

### Theming: Making Emacs Beautiful

```elisp
;; Built-in themes
(load-theme 'modus-themes-dark t)  ; High contrast dark theme

;; Or install better themes (after setting up packages)
;; (load-theme 'doom-dracula t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'zenburn t)

;; Font configuration
(set-face-attribute 'default nil
                    :family "JetBrains Mono"  ; or "Cascadia Code" or "Fira Code"
                    :height 140                ; Font size (1/10 pt)
                    :weight 'normal)

;; Enable ligatures (if your font supports them)
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode t))
```

### Organizing Your Configuration

As your config grows, organize it:

```elisp
;; Method 1: Sections with comments
;;; ========================================
;;; UI Configuration
;;; ========================================

;; UI settings here...

;;; ========================================
;;; Programming Configuration
;;; ========================================

;; Programming settings here...

;; Method 2: Separate files
(load (expand-file-name "settings/ui.el" user-emacs-directory))
(load (expand-file-name "settings/programming.el" user-emacs-directory))
(load (expand-file-name "settings/org.el" user-emacs-directory))

;; Method 3: Org-mode literate config (advanced)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
```

### Performance Optimization

Make Emacs fast:

```elisp
;; Faster startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; Disable expensive features during init
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my--file-name-handler-alist)))

;; Native compilation settings (Emacs 28+)
(when (fboundp 'native-compile-async)
  (setq package-native-compile t)
  (setq native-comp-async-report-warnings-errors nil))
```

### Platform-Specific Configuration

Make Emacs work everywhere:

```elisp
;; macOS specific
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Windows specific
(when (eq system-type 'windows-nt)
  (setq w32-get-true-file-attributes nil)  ; Faster file operations
  (setq inhibit-compacting-font-caches t)) ; Faster fonts

;; Linux specific
(when (eq system-type 'gnu/linux)
  (setq x-super-keysym 'meta))  ; Make Super key work as Meta

;; Terminal vs GUI
(if (display-graphic-p)
    (progn
      ;; GUI settings
      (tool-bar-mode -1)
      (scroll-bar-mode -1))
  ;; Terminal settings
  (xterm-mouse-mode 1))
```

### Creating Your Own Functions

Add custom functionality:

```elisp
;; Insert current date
(defun insert-current-date ()
  "Insert current date in YYYY-MM-DD format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(global-set-key (kbd "C-c d") 'insert-current-date)

;; Open config file quickly
(defun open-config ()
  "Open my Emacs configuration file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-c c") 'open-config)

;; Reload configuration
(defun reload-config ()
  "Reload my Emacs configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Configuration reloaded!"))

;; Kill all buffers except scratch
(defun kill-other-buffers ()
  "Kill all buffers except *scratch*."
  (interactive)
  (mapc 'kill-buffer
        (delq (get-buffer "*scratch*")
              (delq (current-buffer) (buffer-list))))
  (message "Killed all other buffers"))
```

### Configuration Debugging

When things go wrong (and they will):

```elisp
;; Debug on error
(setq debug-on-error t)  ; Show backtrace on errors

;; Profile startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Measure specific operations
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f seconds"
              (float-time (time-since time)))))

;; Use it like:
;; (measure-time
;;   (load-file "some-slow-file.el"))
```

### The Modular Approach

Here's a structure for a growing config:

```
~/.emacs.d/
├── init.el           # Main entry point
├── early-init.el     # Emacs 27+ early initialization
├── custom.el         # Custom-set-variables
├── settings/
│   ├── defaults.el   # Better defaults
│   ├── ui.el         # UI configuration
│   ├── editing.el    # Editing improvements
│   ├── keybindings.el # Custom keybindings
│   └── packages.el   # Package configuration
├── languages/
│   ├── python.el     # Python configuration
│   ├── javascript.el # JavaScript configuration
│   └── rust.el       # Rust configuration
└── backups/          # Backup files
```

### Common Configuration Patterns

```elisp
;; Use-package preview (Chapter 5 spoiler)
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Mode line customization
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-buffer-identification
                "  "
                mode-line-position
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;; Aliases for common commands
(defalias 'yes-or-no-p 'y-or-n-p)  ; y/n instead of yes/no
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
```

### Configuration Anti-Patterns (Don't Do These)

```elisp
;; DON'T: Copy random code without understanding
;; (setq some-variable 42)  ; What does this do?

;; DON'T: Ignore errors
;; (ignore-errors
;;   (require 'some-package))  ; Silent failures are evil

;; DON'T: Modify packages directly
;; Edit your config, not the package files

;; DON'T: Use setq for buffer-local variables globally
;; (setq tab-width 2)  ; Use setq-default instead

;; DON'T: Forget to byte-compile
;; Large configs benefit from byte-compilation
```

### 🎮 **Configuration Exercises**

1. **The Minimalist**: Create the smallest useful config (< 10 lines)
2. **The Organizer**: Split your config into at least 3 files
3. **The Speed Demon**: Measure and optimize your startup time
4. **The Artist**: Install and configure 3 different themes
5. **The Automator**: Create 5 useful custom functions

### Starter Kits and Distributions

If configuring from scratch seems daunting:

- **Spacemacs**: Emacs + Vim + sane defaults
- **Doom Emacs**: Fast, modern, and opinionated
- **Prelude**: Gentle enhancement of Emacs
- **Centaur Emacs**: Modern, fast, and gorgeous

But I recommend starting from scratch first. You'll learn more, and your config will be truly yours.

### What's Next?

Chapter 5 will introduce package management, where we'll install amazing tools that make Emacs rival any modern IDE. Your configuration will grow from a simple init file to a powerful development environment.

Remember: your config is a living document. It will grow with you, reflecting your changing needs and deepening understanding of Emacs. In ten years, you might still be using the same config file, refined and perfected through thousands of small improvements.

That's the beauty of Emacs configuration—it's not just customization, it's programming your perfect editor into existence.

---

*"My Emacs config is 3000 lines long and I understand every single one of them. Well, most of them. Okay, some of them."*
—Every Emacs user, eventually

---

### Chapter 4 Summary

- Configuration is **code**, not settings
- Start simple, grow organically
- **Hooks** run functions automatically
- **Keybindings** make Emacs yours
- **Organize** as you grow
- **Debug** when things break
- Your config is your **companion** for years to come