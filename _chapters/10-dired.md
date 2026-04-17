# Chapter 10: Dired - The Directory Editor
## File Management for People Who Think File Managers Are Beneath Them

Dired (Directory Editor) is what happens when you realize that file management is just text editing in disguise. Why click and drag when you can mark, filter, and batch-rename with regular expressions? Why open a separate file manager when Emacs IS the file manager?

Dired isn't just a file browser—it's a file *editor*. And once you understand that distinction, you'll never want to use Finder or Explorer again.

### Opening Dired

```
C-x d
```

Or just `C-x C-f` on a directory instead of a file. Boom, you're in Dired.

What you see is a buffer. A regular, editable buffer. That lists files. And you can edit it. This is going to get wild.

### Basic Navigation

Standard movement keys work, but these are Dired-specific:

- `RET` or `f` - Open file/directory
- `o` - Open in other window
- `^` - Go up to parent directory
- `n` / `p` - Next/previous line
- `<` / `>` - Beginning/end of buffer
- `j` - Jump to file (with completion)

🚸 **IDE Refugee Note**: This isn't a tree view like VS Code's sidebar. It's a full buffer showing one directory. But trust me, this is more powerful.

### Marking Files: The Power Move

Marking is how you select multiple files for operations:

- `m` - Mark file
- `u` - Unmark file
- `U` - Unmark all
- `t` - Toggle marks
- `* *` - Mark executables
- `* /` - Mark directories
- `* s` - Mark all files in subdirs
- `% m` - Mark by regexp

Example: Mark all Python files:
```
% m \.py$ RET
```

### Operating on Files

Once marked, unleash the power:

- `C` - Copy marked files
- `R` - Rename/move marked files
- `D` - Delete marked files
- `S` - Create symlinks
- `H` - Create hardlinks
- `M` - Change mode (chmod)
- `O` - Change owner (chown)
- `G` - Change group (chgrp)
- `Z` - Compress/uncompress

The workflow:
1. Mark files with `m`
2. Press operation key (like `C` for copy)
3. Enter destination
4. Confirm

### The Magic of Wdired

This is where minds get blown. Press:

```
C-x C-q
```

Now the buffer is editable. You can:
- Edit filenames directly
- Use regular Emacs editing commands
- Search and replace on filenames
- Use rectangles, multiple cursors, macros

When done, `C-c C-c` to save changes or `C-c C-k` to cancel.

🤔 **Why Though?** Because renaming files IS text editing. Why should it be different? In Dired, it isn't.

### Batch Renaming Like a Pro

Want to rename 100 files? Easy:

1. Enter Wdired mode: `C-x C-q`
2. Use regexp replace: `M-%`
3. Enter pattern and replacement
4. `!` to replace all
5. `C-c C-c` to execute

Example: Add date prefix to all files:
```
M-% ^ RET 2024-01-20_ RET !
```

### Dired-x: Extended Features

Add to your config:

```elisp
(require 'dired-x)
(add-hook 'dired-mode-hook 'dired-omit-mode)  ; Enable omit mode
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$")
```

New powers:
- `C-x C-j` - Jump to dired buffer of current file
- `* .` - Mark extension
- `* O` - Mark omitted files
- `F` - Visit all marked files

### File Previewing

```elisp
;; Auto-revert dired buffers
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Preview files
(use-package peep-dired
  :ensure t
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; Or use dired-preview
(use-package dired-preview
  :ensure t
  :config
  (dired-preview-global-mode 1))
```

### Dired Subtrees

```elisp
(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<tab>" . dired-subtree-toggle)))
```

Now TAB expands directories inline. It's like a tree view, but better.

### Advanced Marking

Mark files by content:

```elisp
(defun dired-mark-files-containing-regexp (regexp &optional marker-char)
  "Mark files containing REGEXP."
  (interactive
   (list (read-string "Mark files containing (regexp): ")
         (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
          (not (looking-at dired-re-dir))
          (let ((filename (dired-get-filename nil t)))
            (when filename
              (with-temp-buffer
                (insert-file-contents filename)
                (re-search-forward regexp nil t)))))
     "files containing regexp")))
```

### Image Handling

```elisp
(use-package image-dired
  :ensure nil  ; Built-in
  :bind (:map dired-mode-map
              ("C-t i" . image-dired)))

;; Better: dired-rainbow for color-coded files
(use-package dired-rainbow
  :ensure t
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl")))
```

### Git Integration in Dired

```elisp
(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Show git status in dired
(use-package diff-hl
  :ensure t
  :hook (dired-mode . diff-hl-dired-mode))
```

### Dired as a File Manager

#### Creating Files and Directories

- `+` - Create directory
- `C-x C-f` - Create new file (just type a new name)

#### Finding Files

```elisp
;; Find files by name
(use-package find-dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-f" . find-name-dired)))

;; fd integration (faster find)
(use-package fd-dired
  :ensure t)
```

#### Async Operations

```elisp
(use-package async
  :ensure t
  :config
  (dired-async-mode 1))
```

Now large copy/move operations won't freeze Emacs.

### Dired Configuration

```elisp
;; My complete dired setup
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alhF --group-directories-first")
  (dired-omit-files "^\\.[^.].*")
  (dired-omit-verbose nil)
  (dired-dwim-target t)  ; Suggest other dired buffer as target
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  
  ;; Auto refresh
  (setq global-auto-revert-non-file-buffers t)
  
  ;; Human readable sizes and better formatting
  (when (executable-find "ls")
    (setq dired-listing-switches "-alhF --group-directories-first")))
```

### Custom Dired Commands

```elisp
;; Open file externally
(defun dired-open-file ()
  "Open file with default application."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (cond
     ((eq system-type 'darwin) (call-process "open" nil 0 nil file))
     ((eq system-type 'gnu/linux) (call-process "xdg-open" nil 0 nil file))
     (t (message "Unsupported system")))))

(define-key dired-mode-map (kbd "E") 'dired-open-file)

;; Quick backup
(defun dired-backup-file ()
  "Create backup of file at point."
  (interactive)
  (let ((file (dired-get-filename))
        (backup-name (concat (dired-get-filename) ".bak")))
    (copy-file file backup-name 1)
    (revert-buffer)
    (message "Created backup: %s" backup-name)))

;; Archive marked files
(defun dired-archive-marked-files ()
  "Archive marked files to tar.gz."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (archive (read-string "Archive name: ")))
    (dired-do-shell-command
     (format "tar czf %s.tar.gz" archive)
     nil files)))
```

### Dired Bookmarks

```elisp
;; Quick directory bookmarks
(defun dired-bookmark-jump ()
  "Jump to bookmarked directories."
  (interactive)
  (let ((dir (completing-read "Directory: "
                              '("~/"
                                "~/Documents/"
                                "~/Downloads/"
                                "~/projects/"
                                "~/.emacs.d/"))))
    (dired dir)))

(global-set-key (kbd "C-c j") 'dired-bookmark-jump)
```

### Two-Panel File Manager

```elisp
(defun dired-dual-panel ()
  "Open dired in dual panel mode."
  (interactive)
  (delete-other-windows)
  (dired "~/")
  (split-window-horizontally)
  (other-window 1)
  (dired "~/"))

;; Now marking and copying between panels is easy!
```

### Dired and TRAMP

Open remote directories:

```
C-x d /ssh:user@host:/path/
```

All dired operations work on remote files!

### Advanced Dired Packages

```elisp
;; Icons in dired
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; Ranger-like preview
(use-package ranger
  :ensure t
  :config
  (setq ranger-cleanup-on-disable t))
```

### Dired Workflows

#### The Photo Organizer

```elisp
(defun dired-organize-photos ()
  "Organize photos by date."
  (interactive)
  (dired-mark-files-regexp "\\.\\(jpg\\|jpeg\\|png\\|gif\\)$")
  (let ((photos (dired-get-marked-files)))
    (dolist (photo photos)
      (let* ((attributes (file-attributes photo))
             (mod-time (nth 5 attributes))
             (date-dir (format-time-string "%Y-%m" mod-time)))
        (unless (file-exists-p date-dir)
          (make-directory date-dir))
        (rename-file photo (expand-file-name (file-name-nondirectory photo) date-dir))))))
```

#### The Bulk Renamer

```elisp
(defun dired-rename-with-counter ()
  "Rename marked files with counter."
  (interactive)
  (let ((prefix (read-string "Prefix: "))
        (counter 1))
    (dolist (file (dired-get-marked-files))
      (let* ((extension (file-name-extension file))
             (new-name (format "%s_%03d.%s" prefix counter extension)))
        (rename-file file new-name)
        (setq counter (1+ counter))))
    (revert-buffer)))
```

### 🎮 **Dired Exercises**

1. **The Organizer**: Sort 100 files into folders using marks and operations
2. **The Renamer**: Batch rename files using Wdired and regexp
3. **The Cleaner**: Find and delete all backup files in a directory tree
4. **The Archiver**: Create a function to archive old files by date
5. **The Manager**: Set up a two-panel file manager workflow

### What's Next?

Chapter 11 covers macros and registers—the automation tools that make repetitive tasks disappear. But now that you know Dired, you might never need another file manager again.

Dired exemplifies the Emacs philosophy: everything is text, everything is editable, everything is programmable. Your file manager isn't a separate application—it's just another buffer in Emacs. And that changes everything.

---

*"I used to have a file manager. Now I have Dired. The file manager is gathering dust somewhere, probably in ~/.trash where Dired put it."*
—Reformed File Manager User

---

### Chapter 10 Summary

- **Dired is a buffer**, not just a file list
- **Mark and operate** on multiple files
- **Wdired** makes filenames editable text
- **Everything** is a text editing operation
- **Regexp** power for marking and renaming
- **Integration** with git, images, remote files
- **Extensible** with custom commands
- File management **as text editing**
- You'll never need Finder/Explorer again