# Chapter 3: Movement and Key Bindings
## Your Fingers Will Learn to Dance

There's a moment in every Emacs user's journey when the key bindings stop feeling like alien hieroglyphics and start feeling like music. Your fingers develop muscle memory, and suddenly you're navigating text at the speed of thought.

This chapter is about reaching that moment faster, and maybe having fewer hand cramps along the way.

### The Home Row Philosophy

Emacs was designed when keyboards looked different. The keyboard had the Control key where Caps Lock is now. This is important because it explains why Emacs seems determined to give you repetitive strain injury.

First thing you should do: Swap Caps Lock and Control. Seriously. Do it now.

- **macOS**: System Preferences → Keyboard → Modifier Keys
- **Linux**: Add `setxkbmap -option ctrl:swapcaps` to your `.bashrc`
- **Windows**: Use PowerToys or SharpKeys

🚸 **IDE Refugee Note**: "But I use Caps Lock!" No, you don't. Nobody does. And if you actually do, you can still access it—it's just moved to where Control was. This one change will save your pinky finger from years of torture.

### Basic Movement: Beyond the Arrow Keys

Yes, arrow keys work. But they're so far away! Learn these and your hands never leave home row:

```
    C-p (previous line)
         ↑
C-b ← C-f → (back/forward)
         ↓
    C-n (next line)
```

Think of it as: **b**ack, **f**orward, **n**ext, **p**revious.

🤔 **Why Though?** These keys were chosen because they're on the home row (well, close to it). Once you learn them, you can navigate without moving your hands. It's like touch typing, but for navigation.

### Word and Paragraph Movement

Characters are small. Words are better. Paragraphs are best:

- `M-f` - Forward one word
- `M-b` - Back one word
- `M-a` - Beginning of sentence
- `M-e` - End of sentence
- `M-{` - Beginning of paragraph
- `M-}` - End of paragraph

Notice the pattern? Control for small movements, Meta for bigger movements.

### Line Movement: Your Most-Used Commands

- `C-a` - Beginning of line (**a** for "start of alphabet")
- `C-e` - End of line (**e** for "end")

These become so natural you'll try to use them everywhere else and get frustrated when they don't work.

### Screen Movement: When You Need to Jump

- `M-<` - Beginning of buffer
- `M->` - End of buffer
- `C-v` - Page down
- `M-v` - Page up (Meta reverses things)
- `C-l` - Center screen on cursor (hit it multiple times!)

🎯 **Pro Tip**: `C-l` cycles through center → top → bottom. It's incredibly useful for positioning code exactly where you want it on screen.

### The Mighty Search: Your Navigation Superpower

Forget about carefully navigating to where you want to go. Just search for it:

- `C-s` - Search forward (incremental search)
- `C-r` - Search backward
- `C-s C-s` - Repeat last search
- `M-s w` - Word search (spaces match any whitespace)

While searching:
- `C-s` again - Next match
- `C-r` - Previous match
- `C-g` - Cancel search, return to start
- `Enter` - Exit search, stay at match

🚸 **IDE Refugee Note**: This isn't like Ctrl+F in other editors. It's incremental—it searches as you type. And when you're done, just start typing to edit. No need to "close" the search.

### The Kill Ring Dance: Cut, Copy, and Paste Evolved

Remember: Kill = Cut, Yank = Paste. Here's the full dance:

- `C-k` - Kill from cursor to end of line
- `C-w` - Kill region (cut)
- `M-w` - Copy region (finally, a copy that's not kill!)
- `C-y` - Yank (paste)
- `M-y` - Cycle through kill ring

But here's where it gets beautiful:

- `M-d` - Kill word forward
- `M-DEL` - Kill word backward
- `C-x DEL` - Kill to beginning of sentence
- `M-k` - Kill to end of sentence

Everything you kill goes on the kill ring. It's like having clipboard history built into your muscle memory.

### Undo/Redo: The Tree of Possibilities

- `C-/` or `C-_` or `C-x u` - Undo
- Undo your undos: Move cursor, then undo again

But here's the advanced move: undo-tree (install it in Chapter 5). It turns your linear undo into a branching tree of possibilities. You can literally see alternate timelines of your document.

### Multiple Cursors: When One Isn't Enough

This isn't built-in, but it's so useful we'll cheat and mention it here:

- `C-S-c C-S-c` - Multiple cursors on each line in region
- `C->` - Mark next like this
- `C-<` - Mark previous like this

(We'll install this package in Chapter 5, but I wanted you to know it exists.)

### Registers: Your Bookmark System

Registers are like variables for your editor. Store text, positions, or window configurations:

- `C-x r SPC a` - Store position in register 'a'
- `C-x r j a` - Jump to position in register 'a'
- `C-x r s a` - Store region in register 'a'
- `C-x r i a` - Insert contents of register 'a'

🎯 **Pro Tip**: Use registers for boilerplate text. Store your commonly-used code snippets and insert them instantly.

### Window Navigation: Dancing Through Panes

Remember, windows are the panes within your Emacs frame:

- `C-x o` - Other window (cycles through)
- `C-x 0` - Delete this window
- `C-x 1` - Delete other windows (maximize current)
- `C-x 2` - Split horizontally
- `C-x 3` - Split vertically
- `C-x +` - Balance windows (make them equal size)

Advanced window-fu:
- `C-x 4 f` - Find file in other window
- `C-x 4 b` - Switch buffer in other window
- `C-x 4 0` - Kill buffer and window

### The Prefix Key Pattern

Emacs uses prefix keys to group related commands:

- `C-x` - Commands about files, buffers, windows
- `C-c` - Mode-specific commands
- `C-h` - Help commands
- `M-s` - Search commands
- `C-x r` - Register and rectangle commands

This is why Emacs can have thousands of commands without running out of keys.

### Rectangle Editing: The Hidden Superpower

This will blow your mind. You can select and edit rectangular regions:

1. Set mark at corner: `C-SPC`
2. Move to opposite corner
3. `C-x r k` - Kill rectangle
4. `C-x r y` - Yank rectangle
5. `C-x r t` - Replace rectangle with text
6. `C-x r N` - Number lines in rectangle

🎮 **Try This**: 
1. Create a list of items
2. Select a rectangle at the beginning of all lines
3. `C-x r N` to number them
4. Watch minds blow (including your own)

### The Modal Alternative: Evil Mode

If you're coming from Vim, there's Evil mode. It's Vim emulation so good that Vim users switch to Emacs just to use it. We'll cover this in the appendix, but know it exists.

### Keyboard Macros: Your Personal Robot

This deserves its own chapter (and it'll get one), but here's a taste:

1. `F3` or `C-x (` - Start recording macro
2. Do stuff
3. `F4` or `C-x )` - Stop recording
4. `F4` or `C-x e` - Execute macro
5. `C-u 100 C-x e` - Execute macro 100 times

Example: Add semicolons to the end of 100 lines:
1. `F3` - Start recording
2. `C-e ;` - Go to end of line, add semicolon
3. `C-n C-a` - Next line, beginning
4. `F4` - Stop recording
5. `C-u 99 F4` - Do it 99 more times

### God Mode and Other Insanity

There are packages that completely reimagine Emacs keybindings:

- **God Mode**: Type `g` then commands without modifiers (`g x s` = `C-x C-s`)
- **Hydra**: Create custom key sequence menus
- **Which Key**: Shows available keys after a prefix
- **Key Chord**: Trigger commands with two-key combos

We'll explore these in the packages chapter, but they show how flexible Emacs really is.

### Your Personal Keybinding Manifesto

Here's how to create your own keybindings:

```elisp
;; Simple binding
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;; Unbind something annoying
(global-unset-key (kbd "C-z"))  ; Suspend frame

;; Bind function key
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Create a prefix key
(define-prefix-command 'my-prefix-map)
(global-set-key (kbd "C-c m") 'my-prefix-map)
(define-key my-prefix-map (kbd "t") 'toggle-truncate-lines)
```

🚸 **IDE Refugee Note**: Yes, you can make Emacs use CUA keys (Ctrl+C for copy, etc.). Add `(cua-mode t)` to your config. But try the Emacs way first—it's actually more powerful once you get used to it.

### The Learning Curve Strategy

Don't try to learn everything at once. Here's a progression:

**Week 1**: Basic movement
- `C-f`, `C-b`, `C-n`, `C-p`
- `C-a`, `C-e`
- `M-f`, `M-b`

**Week 2**: Killing and yanking
- `C-k`, `C-w`, `M-w`, `C-y`
- `M-d`, `M-DEL`

**Week 3**: Search and replace
- `C-s`, `C-r`
- `M-%` (query replace)

**Week 4**: Windows and buffers
- `C-x 2`, `C-x 3`, `C-x o`
- `C-x b`, `C-x C-b`

**Month 2**: Advanced movement
- Registers
- Rectangles
- Macros

### Exercises to Build Muscle Memory

🎮 **Daily Drills:**

1. **The No-Arrow Challenge**: Disable arrow keys for a day
   ```elisp
   (global-unset-key (kbd "<left>"))
   (global-unset-key (kbd "<right>"))
   (global-unset-key (kbd "<up>"))
   (global-unset-key (kbd "<down>"))
   ```

2. **The Speed Run**: 
   - Open a file
   - Navigate to 5 specific words using search
   - Kill 3 lines, yank them elsewhere
   - Time yourself, try to beat it

3. **The Rectangle Puzzle**:
   - Create a CSV file
   - Use rectangles to rearrange columns
   - Add line numbers to one column

4. **The Macro Marathon**:
   - Find a repetitive task
   - Solve it with a macro
   - Feel like a wizard

### The Ergonomics Section

Real talk: Emacs can hurt your hands if you're not careful.

**Prevention**:
1. Swap Caps Lock and Control (seriously, do it)
2. Use sticky keys if needed (tap Control instead of holding)
3. Learn to use both Control keys
4. Take breaks
5. Consider foot pedals for modifiers (I'm not joking)

**Alternative configurations**:
- Spacemacs: Space bar as leader key
- Doom Emacs: Modern keybindings
- Ergoemacs: Ergonomic keybindings

### The Keybinding Cheat Sheet

Print this out and stick it on your monitor:

```
MOVEMENT           EDITING             SEARCH
C-f  →            C-k    kill line    C-s    forward
C-b  ←            C-w    kill region  C-r    backward
C-n  ↓            M-w    copy region  C-M-s  regexp
C-p  ↑            C-y    yank         M-%    replace
M-f  word →       M-y    yank pop
M-b  word ←       C-/    undo         FILES
C-a  line start                       C-x C-f  find
C-e  line end     WINDOWS             C-x C-s  save
M-<  buffer start C-x 2  split horiz  C-x C-w  save as
M->  buffer end   C-x 3  split vert   
                  C-x o  other        HELP
                  C-x 1  only         C-h k  key
                  C-x 0  close        C-h f  function
```

### What's Next?

Now that your fingers are learning to dance, Chapter 4 will show you how to make Emacs truly yours through configuration. We'll start with simple tweaks and work our way up to a configuration that other Emacs users will envy.

But remember: keybindings are like vocabulary. The more you know, the more eloquently you can express your intentions to Emacs. And once you're fluent, you'll wonder how you ever lived without them.

---

*"The keyboard is my piano, Emacs is my music, and text is my symphony."*
—An Emacs user who learned all the keybindings

---

### Chapter 3 Summary

- **Movement is modal**: Character → Word → Line → Paragraph → Screen
- **Search is faster** than navigation
- **Kill ring** > clipboard
- **Registers** are bookmarks and variables
- **Rectangles** are mind-blowing
- **Macros** are your personal automation
- **Customize everything**—make Emacs fit your hands

Master these keys, and you'll edit text faster than you can think about editing text.