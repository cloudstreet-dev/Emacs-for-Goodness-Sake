# Chapter 11: Macros and Registers
## Automation for Humans (And Cyborgs)

Imagine if you could record yourself editing text, then replay that recording 1000 times. Now imagine if you could save snippets of text or positions in variables you can instantly recall. That's macros and registers. Together, they turn repetitive tasks into a single keystroke and make you look like you're hacking the Matrix when really you're just editing a CSV file.

This is where Emacs stops being a text editor and starts being a text manipulation engine that responds to your will.

### Keyboard Macros: Your Personal Robot

The simplest way to record a macro:

```
F3     - Start recording
...    - Do stuff
F4     - Stop recording
F4     - Play it back
```

That's it. You just automated something.

🚸 **IDE Refugee Note**: This is like recording actions in Photoshop or Excel, except it works everywhere in Emacs, on any text, in any mode. And it's been here since the 1970s.

### A Real Example

Let's say you have:
```
alice
bob
charlie
dave
```

And want:
```
User: alice (ID: 1)
User: bob (ID: 2)
User: charlie (ID: 3)
User: dave (ID: 4)
```

Here's how:
1. Position cursor at start of "alice"
2. `F3` - Start recording
3. `C-a` - Beginning of line
4. Type: `User: `
5. `C-e` - End of line
6. Type: ` (ID: 1)`
7. `C-n` - Next line
8. `F4` - Stop recording

Now the magic:
- `F4` - Applies to bob (but with ID: 1)
- `C-u 2 F4` - Applies to next 2 lines

But wait, the ID doesn't increment! Let's fix that...

### The Macro Counter

Emacs has a built-in counter for macros:

1. `F3` - Start recording
2. `C-a` - Beginning of line
3. Type: `User: `
4. `C-e` - End of line
5. Type: ` (ID: `
6. `C-x C-k C-i` - Insert counter (starts at 0)
7. Type: `)`
8. `C-n` - Next line
9. `F4` - Stop recording

Set the counter before running:
```
C-x C-k C-c 1 RET    ; Set counter to 1
C-u 3 F4             ; Apply to next 3 lines
```

Now each line gets an incrementing ID!

### Macro Ring: Multiple Macros

You can have multiple macros:

- `C-x C-k C-k` - Execute second-to-last macro
- `C-x C-k C-n` - Cycle to next macro in ring
- `C-x C-k C-p` - Cycle to previous macro

### Editing Macros: Yes, You Can Debug Them

Made a mistake? Edit the macro:

```
C-x C-k C-e    ; Edit last macro
```

This opens a buffer showing your macro as text:

```
C-a            ; beginning-of-line
User: SPC      ; self-insert-command * 6
C-e            ; end-of-line
...
```

Edit it like any text, then `C-c C-c` to save.

### Saving Macros Forever

Turn your macro into a function:

```
C-x C-k n my-awesome-macro RET   ; Name it
M-x insert-kbd-macro RET          ; Insert as Elisp
```

This generates:

```elisp
(fset 'my-awesome-macro
   "\C-aUser: \C-e")
```

Put this in your config and bind it:

```elisp
(global-set-key (kbd "C-c m") 'my-awesome-macro)
```

### Advanced Macro Techniques

#### Conditional Macros

Use search to make macros conditional:

1. Start macro
2. `C-s pattern` - Search for pattern
3. If found, do something
4. End macro

If search fails, macro stops. Perfect for processing only certain lines.

#### Recursive Macros

A macro that calls itself:

1. Record macro that does something and moves forward
2. At the end, add `F4` (call-last-kbd-macro)
3. Stop recording
4. Run once - it runs forever (until error)

#### Query in Macros

```
C-x q    ; Query user during macro
```

When macro runs, it pauses and asks for confirmation.

### Registers: Your Multi-Clipboard

Registers are single-character variables that can store:
- Text
- Numbers  
- Positions
- Window configurations
- Rectangles
- Macros

#### Text Registers

```
C-x r s a    ; Save region to register 'a'
C-x r i a    ; Insert contents of register 'a'
```

Example workflow:
1. Select boilerplate text
2. `C-x r s b` - Save to register 'b'
3. Later: `C-x r i b` - Insert boilerplate

#### Position Registers

```
C-x r SPC a    ; Save position to register 'a'
C-x r j a      ; Jump to position in register 'a'
```

Perfect for jumping between locations:
1. `C-x r SPC h` - Mark header location
2. `C-x r SPC f` - Mark footer location
3. Edit in the middle
4. `C-x r j h` - Jump back to header
5. `C-x r j f` - Jump to footer

#### Number Registers

```
C-u 100 C-x r n a    ; Store 100 in register 'a'
C-x r i a            ; Insert 100
C-x r + a            ; Increment register 'a'
```

Use case: Counters in macros:

```elisp
(defun my-numbered-list ()
  "Insert a numbered list item."
  (interactive)
  (insert (format "%d. " (get-register ?n)))
  (set-register ?n (1+ (get-register ?n))))
```

#### Window Configuration Registers

```
C-x r w a    ; Save window configuration to 'a'
C-x r j a    ; Restore window configuration
```

This saves your entire window layout:
1. Set up complex window arrangement
2. `C-x r w z` - Save to register 'z'
3. Work in single window
4. `C-x r j z` - Restore layout instantly

#### Rectangle Registers

```
C-x r r a    ; Save rectangle to register 'a'
C-x r i a    ; Insert rectangle
```

Combined with rectangle commands, this is powerful for column editing.

### Combining Macros and Registers

This is where the magic happens:

```elisp
;; Macro that uses registers
F3                    ; Start macro
C-x r i h            ; Insert header from register
C-n                  ; Next line
... do stuff ...
C-x r i f            ; Insert footer from register
F4                    ; End macro
```

### Practical Examples

#### Converting CSV to SQL

Starting with:
```
John,Doe,30
Jane,Smith,25
Bob,Johnson,35
```

Want:
```sql
INSERT INTO users (first, last, age) VALUES ('John', 'Doe', 30);
INSERT INTO users (first, last, age) VALUES ('Jane', 'Smith', 25);
INSERT INTO users (first, last, age) VALUES ('Bob', 'Johnson', 35);
```

Macro:
1. `F3` - Start
2. `C-a` - Beginning of line
3. Type: `INSERT INTO users (first, last, age) VALUES ('`
4. `C-s ,` - Search for comma
5. Type: `', '`
6. `C-s ,` - Next comma
7. Type: `', `
8. `C-e` - End of line
9. Type: `);`
10. `C-n` - Next line
11. `F4` - Stop

#### Creating Test Data

Generate 100 test users:

```elisp
(defun generate-test-users (n)
  "Generate N test users."
  (interactive "nNumber of users: ")
  (dotimes (i n)
    (insert (format "User_%03d,user%03d@example.com,password%03d\n" 
                    i i i))))
```

Or with a macro using the counter:

1. `C-x C-k C-c 1` - Set counter to 1
2. `F3` - Start macro
3. Type: `User_`
4. `C-x C-k C-i` - Insert counter (formatted with leading zeros)
5. Type: `,user`
6. `C-x C-k C-i` - Insert counter again
7. Type: `@example.com`
8. `RET` - Newline
9. `F4` - Stop
10. `C-u 99 F4` - Create 99 more

### Advanced Register Usage

#### Register List

View all registers:

```
C-x r l    ; List registers
```

#### Append to Registers

```
C-x r + a    ; Append to text register
C-u C-x r s a ; Prepend region to register (with prefix arg)
```

#### Register Persistence

Save registers across sessions:

```elisp
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'register-alist)
```

### Bookmarks: Named Position Registers

Bookmarks are like permanent, named position registers:

```
C-x r m      ; Set bookmark
C-x r b      ; Jump to bookmark
C-x r l      ; List bookmarks
```

They're saved automatically and persist across sessions.

### Real-World Macro Workflows

#### The Refactorer

Convert function calls from `oldFunc(a, b, c)` to `newFunc(c, b, a)`:

1. `F3` - Start macro
2. `C-s oldFunc(` - Find function
3. `C-f` - Enter parens
4. `C-M-f` - Move forward over first argument
5. Mark argument and `C-x r s 1` - Save to register 1
6. `C-d` - Delete comma
7. Continue for other args...
8. Reassemble in new order
9. `F4` - Stop

#### The Data Transformer

Transform JSON to YAML:

```elisp
(defun json-to-yaml-macro ()
  "Macro to convert JSON to YAML."
  (interactive)
  (kmacro-set-counter 0)
  (while (re-search-forward "\"\\([^\"]+\\)\": " nil t)
    (replace-match "\\1: ")
    (when (looking-at "\"\\([^\"]+\\)\"")
      (replace-match "\\1"))))
```

### Macro Best Practices

1. **Start simple**: Record basic macro first, enhance later
2. **Use searches**: Make macros resilient to line variations
3. **Test on one**: Before applying to 1000 lines
4. **Save good ones**: Name and bind frequently used macros
5. **Combine with registers**: For complex transformations

### Debugging Macros

```elisp
;; Step through macro execution
(setq kmacro-step-edit-mini-window-height 15)
C-x C-k SPC    ; Step-edit macro

;; See what macro does
C-x C-k C-v    ; View last macro

;; Ring management
C-x C-k C-d    ; Delete head of macro ring
```

### 🎮 **Macro and Register Exercises**

1. **The Formatter**: Record a macro that formats messy code
2. **The Generator**: Create test data with incrementing IDs
3. **The Transformer**: Convert between data formats (JSON→CSV)
4. **The Navigator**: Set up position registers for a large file
5. **The Automator**: Create a complex macro using multiple registers

### What's Next?

The Appendix awaits with a survival guide for IDE refugees—tips and tricks to make your transition from VS Code/IntelliJ/Sublime as smooth as possible.

But now that you know macros and registers, you've unlocked Emacs's automation superpowers. You can record any sequence of actions and replay it endlessly. You can store and recall text, positions, and window configurations instantly. You're not just editing text—you're programming your editing.

---

*"I spent 2 hours writing a macro to save 5 minutes of work. Totally worth it because I'll use it again. Maybe. Probably. Okay, it was just fun."*
—Every Emacs User Eventually

---

### Chapter 11 Summary

- **Macros record keystrokes**, replay them infinitely
- **F3/F4** is your quick record/play
- **Counters** make macros dynamic
- **Registers** store text, positions, numbers, configurations
- **Single character** variables at your fingertips
- **Combine** macros and registers for complex automation
- **Save** macros as functions
- **Debug** and edit macros like code
- You're now an **automation wizard** 🧙‍♂️