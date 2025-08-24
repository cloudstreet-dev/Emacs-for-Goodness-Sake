# Chapter 9: Just Enough Elisp
## Programming Your Editor (Or: How to Become a Wizard)

Emacs Lisp (Elisp) is what separates Emacs users from Emacs wizards. It's the difference between using Emacs and making Emacs yours. The beautiful part? You don't need to become a Lisp expert to write useful Elisp. You just need enough to solve your own problems.

This chapter won't make you a Lisp programmer. It will make you dangerous. In a good way.

### The REPL is Everywhere

The scratch buffer is your playground:

```elisp
;; Type this and press C-j after each line
(+ 2 2)
;; => 4

(* 6 7)
;; => 42

(message "Hello, Emacs!")
;; => "Hello, Emacs!"

(buffer-name)
;; => "*scratch*"
```

Or use `M-:` (eval-expression) from anywhere to evaluate Elisp instantly.

🚸 **IDE Refugee Note**: This is like having a JavaScript console for your editor, except the editor IS JavaScript (well, Lisp). You can modify your editor while using it. It's like changing the engine of a car while driving it. At 80mph. Blindfolded. But somehow it works.

### The Basics: Lists All the Way Down

Everything in Lisp is either an atom or a list:

```elisp
;; Atoms
42                  ; number
"hello"            ; string
'symbol            ; symbol
t                  ; true
nil                ; false/empty

;; Lists (the first element is usually a function)
(function arg1 arg2 arg3)

;; Quote prevents evaluation
'(this is just data)
(quote (same thing))

;; Nested lists
(+ 1 (* 2 3))      ; => 7
```

### Variables: Let There Be Bindings

```elisp
;; Global variable
(setq my-name "Emacs User")

;; Local variables
(let ((x 10)
      (y 20))
  (+ x y))         ; => 30

;; let* allows references to earlier bindings
(let* ((x 10)
       (y (* x 2)))
  (+ x y))         ; => 30

;; Dynamic (special) variables
(defvar my-special-var 100
  "This is a special variable with documentation.")

;; Constants
(defconst my-constant "unchangeable"
  "This shouldn't change.")
```

### Functions: Your Building Blocks

```elisp
;; Simple function
(defun greet (name)
  "Greet NAME enthusiastically."
  (message "Hello, %s! Welcome to Emacs!" name))

(greet "World")    ; => "Hello, World! Welcome to Emacs!"

;; Interactive functions (commands)
(defun insert-date ()
  "Insert current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; With arguments
(defun wrap-region (before after)
  "Wrap region with BEFORE and AFTER strings."
  (interactive "sBefore: \nsAfter: ")
  (save-excursion
    (goto-char (region-end))
    (insert after)
    (goto-char (region-beginning))
    (insert before)))

;; Optional arguments
(defun greet-optional (&optional name)
  "Greet NAME, or 'Friend' if not provided."
  (message "Hello, %s!" (or name "Friend")))
```

### Control Flow: Making Decisions

```elisp
;; if-then-else
(if (> 3 2)
    (message "Math works!")
  (message "Math is broken!"))

;; when (if without else)
(when (buffer-modified-p)
  (message "You have unsaved changes!")
  (save-buffer))

;; unless (opposite of when)
(unless (buffer-modified-p)
  (message "Buffer is saved"))

;; cond (multiple conditions)
(defun describe-number (n)
  (cond ((< n 0) "negative")
        ((= n 0) "zero")
        ((< n 10) "small")
        (t "large")))    ; t is the default case

;; case/pcase for pattern matching
(pcase major-mode
  ('python-mode (message "Pythonic!"))
  ('emacs-lisp-mode (message "Lispy!"))
  (_ (message "Something else")))
```

### Working with Buffers

```elisp
;; Current buffer operations
(buffer-name)                    ; Get name
(buffer-file-name)               ; Get file path
(buffer-modified-p)              ; Check if modified
(save-buffer)                    ; Save it

;; Switch buffers
(switch-to-buffer "*scratch*")
(with-current-buffer "*Messages*"
  (buffer-string))               ; Get all content

;; Create new buffer
(get-buffer-create "my-buffer")

;; Temporary buffer
(with-temp-buffer
  (insert "temporary content")
  (buffer-string))

;; Iterate over buffers
(dolist (buf (buffer-list))
  (message "Buffer: %s" (buffer-name buf)))
```

### Manipulating Text

```elisp
;; Point and region
(point)                          ; Current position
(point-min)                      ; Beginning of buffer
(point-max)                      ; End of buffer
(region-beginning)               ; Start of selection
(region-end)                     ; End of selection

;; Movement
(goto-char (point-min))          ; Go to beginning
(forward-char 5)                 ; Move forward 5 chars
(forward-line 2)                 ; Move down 2 lines
(beginning-of-line)              ; Start of line
(end-of-line)                    ; End of line

;; Insertion and deletion
(insert "Hello, ")
(insert "World!")
(delete-char 1)                  ; Delete forward
(delete-backward-char 1)         ; Delete backward
(kill-line)                      ; Kill to end of line

;; Search and replace
(search-forward "pattern")
(replace-string "old" "new")

;; Get text
(buffer-substring (point) (+ (point) 10))
(thing-at-point 'word)
(thing-at-point 'line)
```

### Hooks: Automation Magic

```elisp
;; Add to existing hook
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Mode-specific hook
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width 4)))

;; Create your own hook
(defvar my-startup-hook nil
  "Hook run after my configuration loads.")

(run-hooks 'my-startup-hook)

;; Remove from hook
(remove-hook 'before-save-hook 'delete-trailing-whitespace)
```

### Regular Expressions

```elisp
;; String match
(string-match "h.llo" "hello")   ; => 0 (match position)

;; Replace in string
(replace-regexp-in-string "l+" "L" "hello")  ; => "heLo"

;; Buffer search
(save-excursion
  (goto-char (point-min))
  (while (re-search-forward "TODO" nil t)
    (replace-match "DONE")))

;; Capturing groups
(when (string-match "\\([0-9]+\\)-\\([0-9]+\\)" "123-456")
  (match-string 1 "123-456"))    ; => "123"
```

### Lists and Loops

```elisp
;; List operations
(car '(1 2 3))                   ; => 1 (first)
(cdr '(1 2 3))                   ; => (2 3) (rest)
(nth 2 '(a b c d))               ; => c (zero-indexed)
(length '(1 2 3))                ; => 3
(append '(1 2) '(3 4))           ; => (1 2 3 4)
(cons 0 '(1 2 3))                ; => (0 1 2 3)

;; Loops
(dolist (item '(a b c))
  (message "%s" item))

(dotimes (i 5)
  (message "Count: %d" i))

(while (< (point) (point-max))
  (forward-line 1))

;; Map functions
(mapcar '1+ '(1 2 3))            ; => (2 3 4)
(mapcar 'buffer-name (buffer-list))
```

### Practical Examples

#### Auto-save on Focus Lost

```elisp
(defun auto-save-all ()
  "Save all modified buffers."
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'auto-save-all)
```

#### Smart Beginning of Line

```elisp
(defun smart-beginning-of-line ()
  "Move to beginning of line or first non-whitespace."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    (skip-chars-forward " \t")
    (when (eq pt (point))
      (beginning-of-line))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)
```

#### Toggle Between Files

```elisp
(defun toggle-between-files ()
  "Toggle between .h and .cpp files."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (cond
       ((string-match "\\.cpp$" file)
        (find-file (replace-regexp-in-string "\\.cpp$" ".h" file)))
       ((string-match "\\.h$" file)
        (find-file (replace-regexp-in-string "\\.h$" ".cpp" file)))))))
```

#### Project-specific Settings

```elisp
(defun my-project-setup ()
  "Setup for my specific project."
  (when (and (buffer-file-name)
             (string-match "my-project" (buffer-file-name)))
    (setq-local compile-command "make -j4")
    (setq-local tab-width 2)))

(add-hook 'prog-mode-hook 'my-project-setup)
```

### Macros: Code That Writes Code

```elisp
;; Simple macro
(defmacro when-let (binding &rest body)
  "Execute BODY when BINDING is non-nil."
  `(let (,binding)
     (when ,(car binding)
       ,@body)))

;; Use it
(when-let ((x (some-function)))
  (message "Got value: %s" x))

;; Macro for defining similar functions
(defmacro define-toggle (name variable)
  `(defun ,name ()
     ,(format "Toggle %s." variable)
     (interactive)
     (setq ,variable (not ,variable))
     (message "%s is now %s" ',variable ,variable)))

(define-toggle toggle-debug debug-on-error)
```

### Advice: Modifying Existing Functions

```elisp
;; Add behavior before a function
(advice-add 'save-buffer :before
            (lambda () (message "Saving buffer...")))

;; Add behavior after
(advice-add 'save-buffer :after
            (lambda () (message "Buffer saved!")))

;; Modify return value
(advice-add 'buffer-name :filter-return
            (lambda (name) (upcase name)))

;; Remove advice
(advice-remove 'save-buffer
               (lambda () (message "Saving buffer...")))
```

### Error Handling

```elisp
;; Catch errors
(condition-case err
    (/ 1 0)
  (error (message "Error occurred: %s" err)))

;; Ignore errors
(ignore-errors
  (some-function-that-might-fail))

;; User errors (shown in minibuffer)
(defun my-function ()
  (interactive)
  (when (not (buffer-file-name))
    (user-error "This buffer has no file!")))
```

### Creating a Minor Mode

```elisp
(define-minor-mode my-writing-mode
  "Minor mode for distraction-free writing."
  :lighter " Write"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c w") 'count-words)
            map)
  (if my-writing-mode
      (progn
        (visual-line-mode 1)
        (display-line-numbers-mode -1)
        (message "Writing mode enabled"))
    (visual-line-mode -1)
    (display-line-numbers-mode 1)
    (message "Writing mode disabled")))
```

### Debugging Elisp

```elisp
;; Enable debugging
(setq debug-on-error t)

;; Insert debug statement
(defun my-buggy-function ()
  (let ((x 10))
    (debug)  ; Enters debugger here
    (/ x 0)))

;; Trace function calls
(trace-function 'my-function)

;; Untrace
(untrace-function 'my-function)

;; Measure execution time
(benchmark-run 1000
  (mapcar '1+ '(1 2 3 4 5)))
```

### Real-World Package Example

Here's a simple package that adds word count to the mode line:

```elisp
;;; word-count-mode.el --- Display word count in mode line

;;; Commentary:
;; Shows live word count in the mode line

;;; Code:

(defvar word-count-mode-update-timer nil)

(defun word-count-buffer ()
  "Count words in buffer."
  (let ((words 0))
    (save-excursion
      (goto-char (point-min))
      (while (forward-word 1)
        (setq words (1+ words))))
    words))

(defun word-count-update-mode-line ()
  "Update word count in mode line."
  (setq mode-line-misc-info
        `((word-count-mode
           (:eval (format " Words: %d" (word-count-buffer)))))))

(define-minor-mode word-count-mode
  "Display word count in mode line."
  :lighter " WC"
  (if word-count-mode
      (progn
        (word-count-update-mode-line)
        (setq word-count-mode-update-timer
              (run-with-idle-timer 1 t 'word-count-update-mode-line)))
    (cancel-timer word-count-mode-update-timer)
    (setq mode-line-misc-info nil)))

(provide 'word-count-mode)
;;; word-count-mode.el ends here
```

### 🎮 **Elisp Exercises**

1. **The Helper**: Write a function that inserts your most-used code snippet
2. **The Navigator**: Create a function to jump between test and source files
3. **The Formatter**: Build a function that formats the current paragraph
4. **The Mode**: Create a minor mode for your specific workflow
5. **The Package**: Write a complete package and share it on MELPA

### What's Next?

Chapter 10 will explore Dired, Emacs's file manager that makes Finder/Explorer look like toys. But now that you know Elisp, you're not just using Emacs—you're programming it. Every problem you encounter can now be solved with a few lines of code.

This is the real power of Emacs: it's not just extensible, it's infinitely malleable. Your editor can now evolve with you, shaped by every problem you solve and every workflow you develop.

---

*"Give someone Emacs, and they can edit for a day. Teach them Elisp, and they can edit for a lifetime. And probably build a cryptocurrency in their config file."*
—Ancient Emacs Proverb

---

### Chapter 9 Summary

- **Everything is a list** (or an atom)
- **Functions are just lists** with the function first
- **Interactive functions** become commands
- **Hooks** automate everything
- **Advice** modifies existing behavior
- **Minor modes** add functionality
- **The scratch buffer** is your REPL
- Start small, solve real problems
- Your Elisp grows with you
- You're now a **wizard** 🧙‍♂️