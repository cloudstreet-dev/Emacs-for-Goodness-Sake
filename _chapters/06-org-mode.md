# Chapter 6: Org-mode
## Your Life in Plain Text

I need to tell you something important: Org-mode is the reason half of us use Emacs. We came for the text editor, discovered Org-mode, and realized we'd been doing everything wrong our entire lives. It's not just a note-taking system—it's a plain text format that can handle your tasks, calendar, documents, spreadsheets, literate programming, and probably your taxes if you ask it nicely.

Org-mode is what happens when you give programmers a markup language and tell them to solve every organizational problem they've ever had. The result is beautiful, terrifying, and absolutely addictive.

### Your First Org File

Create a file called `life.org`:

```org
* Welcome to Org-mode
This is a heading. Press TAB on it. Mind = blown.

** This is a subheading
   And this is some text under it. Everything is collapsible!
   
*** Even deeper
    We can go as deep as we want. It's headings all the way down.

* TODO Buy milk
  SCHEDULED: <2024-01-20 Sat>
  
* DONE Discover Org-mode
  CLOSED: [2024-01-19 Fri 16:42]
```

Now try:
- `TAB` on a heading - Collapse/expand
- `Shift-TAB` - Cycle visibility globally
- `C-c C-t` on a TODO - Cycle through states

🤔 **Why Though?** Org-mode treats structure as data. Those asterisks aren't just formatting—they're semantic structure that Org can query, filter, and manipulate.

### The TODO System That Ruins All Other TODO Systems

```org
* TODO Write Chapter 6
  DEADLINE: <2024-01-25 Thu>
  :PROPERTIES:
  :EFFORT:   3:00
  :END:
  
  Some notes about this task.
  
** TODO Research other org tutorials
** TODO Write first draft
** TODO Edit and revise

* TODO [#A] Fix critical bug
  SCHEDULED: <2024-01-20 Sat 09:00>
  
* WAIT Waiting for Bob's response
  
* Projects :project:
** TODO Project Alpha
*** TODO Design phase
*** TODO Implementation
*** TODO Testing

* Personal :personal:
** TODO Call mom
   SCHEDULED: <2024-01-21 Sun 19:00 +1w>
```

Key commands:
- `C-c C-t` - Cycle TODO states
- `C-c C-s` - Schedule
- `C-c C-d` - Set deadline
- `C-c ,` - Set priority
- `C-c C-c` - Add tags

### The Agenda: Your Command Center

First, tell Org where your files live:

```elisp
(setq org-agenda-files '("~/org/"))
```

Now witness the magic:

```
M-x org-agenda
```

Press `a` for agenda view. You'll see all your scheduled tasks, deadlines, and appointments in a beautiful calendar view. This isn't just a task list—it's a temporal map of your life.

🚸 **IDE Refugee Note**: This is like having Jira, Google Calendar, and Todoist inside your text editor. Except it's faster, more flexible, and it's just plain text so you actually own your data.

### Tables: Spreadsheets for Humans

Type this and press TAB:

```org
|Name|Hours|Rate|Total|
|-
```

Org creates a table! Keep going:

```org
| Name    | Hours | Rate | Total |
|---------+-------+------+-------|
| Alice   |    10 |   50 |       |
| Bob     |    15 |   60 |       |
| Charlie |     8 |   55 |       |
|---------+-------+------+-------|
| Total   |       |      |       |
#+TBLFM: $4=$2*$3::@5$2=vsum(@2..@4)::@5$4=vsum(@2..@4)
```

Press `C-c C-c` on the TBLFM line. Org calculates everything!

Table commands:
- `TAB` / `Shift-TAB` - Navigate cells
- `C-c |` - Create table from region
- `C-c -` - Insert horizontal line
- `C-c RET` - Insert row
- `C-c ^` - Sort table

### Links: Connecting Everything

```org
* Links in Org

** Web links
- [[https://orgmode.org][Org-mode website]]
- [[https://google.com]] (plain URL)

** File links  
- [[file:~/Documents/report.pdf][Quarterly Report]]
- [[file:~/code/project/main.py::42][Line 42 of main.py]]

** Internal links
- [[*Chapter 1][Link to Chapter 1 heading]]
- [[#my-custom-id][Link to custom ID]]

** Create a link target
:PROPERTIES:
:CUSTOM_ID: my-custom-id
:END:

Store link with C-c l, insert with C-c C-l
```

### Code Blocks: Literate Programming

Org can execute code blocks and capture results:

```org
#+BEGIN_SRC python :results output
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

for i in range(10):
    print(f"F({i}) = {fibonacci(i)}")
#+END_SRC

#+RESULTS:
: F(0) = 0
: F(1) = 1
: F(2) = 1
: F(3) = 2
: F(4) = 3
: F(5) = 5
: F(6) = 8
: F(7) = 13
: F(8) = 21
: F(9) = 34
```

Press `C-c C-c` in the code block to execute it!

Configure Babel for languages:

```elisp
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)))
```

### Export: From Org to Everything

Org can export to HTML, PDF, Markdown, and more:

```org
#+TITLE: My Amazing Document
#+AUTHOR: Your Name
#+DATE: 2024-01-20
#+OPTIONS: toc:2 num:t

* Introduction
This document will be beautiful in any format.

** Background
Some background information.

* Main Content
The main content goes here.

* Conclusion
Wrapping things up.
```

Export with `C-c C-e`:
- `h h` - HTML
- `l p` - LaTeX PDF
- `m m` - Markdown
- `o o` - OpenDocument

### Capture: Never Lose a Thought

Set up capture templates:

```elisp
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %?\n  %i\n  %a")
        ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
         "* MEETING with %? :meeting:\n  %T")))

(global-set-key (kbd "C-c c") 'org-capture)
```

Now `C-c c` from anywhere captures thoughts instantly!

### Clocking: Time Tracking Built In

```org
* TODO Write documentation
  :LOGBOOK:
  CLOCK: [2024-01-20 Sat 10:00]--[2024-01-20 Sat 11:30] =>  1:30
  CLOCK: [2024-01-19 Fri 14:00]--[2024-01-19 Fri 16:00] =>  2:00
  :END:
```

Commands:
- `C-c C-x C-i` - Clock in
- `C-c C-x C-o` - Clock out
- `C-c C-x C-r` - Clock report

### Org-Roam: Your Second Brain

Install org-roam for Zettelkasten-style note-taking:

```elisp
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org-roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))
```

Now you have a personal wiki with backlinks!

### Advanced Org Features

#### Custom TODO States

```elisp
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d)")
        (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("NEXT" . (:foreground "blue" :weight bold))
        ("PROG" . (:foreground "yellow" :weight bold))
        ("WAIT" . (:foreground "orange" :weight bold))
        ("CANCELLED" . (:foreground "red" :weight bold))))
```

#### Properties and Column View

```org
* Project Overview
  :PROPERTIES:
  :COLUMNS: %40ITEM %TODO %3PRIORITY %TAGS %17Effort(Estimated){:} %CLOCKSUM
  :END:
  
** TODO Task 1
   :PROPERTIES:
   :Effort:   2:00
   :END:
   
** TODO Task 2
   :PROPERTIES:
   :Effort:   3:00
   :END:
```

Press `C-c C-x C-c` for column view!

#### Dynamic Blocks

```org
#+BEGIN: clocktable :scope file :maxlevel 2
#+END:

Press C-c C-c to update

#+BEGIN: columnview :id local
#+END:
```

### Org for Different Roles

#### The Project Manager

```org
* Q1 2024 Projects
** Project Alpha [33%]
   DEADLINE: <2024-03-31>
   - [X] Requirements gathering
   - [ ] Design phase
   - [ ] Implementation
   
** Project Beta [0%]
   SCHEDULED: <2024-02-01>
   - [ ] Kickoff meeting
   - [ ] Resource allocation
   
* Team Stand-ups
** <2024-01-20 Sat> Daily Standup
*** Alice
    - Completed API integration
    - Working on tests
*** Bob
    - Blocked on database access
    - Need DevOps help
```

#### The Researcher

```org
* Literature Review
** [[cite:@smith2023]] Smith et al. (2023)
   - Key finding: X correlates with Y
   - Methodology: Randomized controlled trial
   - Sample size: n=500
   
** [[cite:@jones2024]] Jones (2024)
   - Contradicts Smith on point Z
   - TODO: Investigate discrepancy
   
#+BIBLIOGRAPHY: references.bib
```

#### The Student

```org
* Courses
** CS101: Introduction to Programming
*** Lectures
**** <2024-01-20> Lecture 5: Functions
     - Pure functions vs side effects
     - Higher-order functions
     - TODO: Read Chapter 5
     
*** Assignments
**** TODO Assignment 2
     DEADLINE: <2024-01-25>
     - [X] Problem 1
     - [ ] Problem 2
     - [ ] Problem 3
```

### Your Org-mode Workflow

Here's a complete GTD setup:

```elisp
;; Capture
(global-set-key (kbd "C-c c") 'org-capture)

;; Agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; Refile
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

;; Archive
(setq org-archive-location "~/org/archive.org::datetree/")

;; Habits
(add-to-list 'org-modules 'org-habit)

;; Pretty bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))
```

### Org-mode Superpowers

#### Inline Images

```org
[[file:diagram.png]]

Then C-c C-x C-v to toggle display
```

#### LaTeX Fragments

```org
The equation $e^{i\pi} + 1 = 0$ is beautiful.

\begin{equation}
\int_{-\infty}^{\infty} e^{-x^2} dx = \sqrt{\pi}
\end{equation}

C-c C-x C-l to preview
```

#### Drawers

```org
* Task with hidden details
  :DETAILS:
  This information is hidden by default.
  It can contain multiple lines.
  :END:
```

### 🎮 **Org-mode Exercises**

1. **The Planner**: Create a complete GTD system with inbox, projects, and contexts
2. **The Documenter**: Write this chapter in Org and export to Markdown
3. **The Analyst**: Create a budget spreadsheet using Org tables
4. **The Programmer**: Write a literate program mixing code and documentation
5. **The Researcher**: Build a knowledge base with org-roam

### What's Next?

Chapter 7 will transform Emacs into an IDE that makes your colleagues jealous. But honestly? After learning Org-mode, you might never want to leave. Some people use Emacs exclusively for Org-mode, and that's perfectly valid.

Org-mode is more than a feature—it's a philosophy. It says that your thoughts, tasks, and knowledge should be in plain text, under your control, infinitely flexible. Once you internalize this, every other system feels constraining.

---

*"I switched to Emacs for Org-mode. That was 10 years ago. I'm still discovering new features."*
—Every Org-mode user

---

### Chapter 6 Summary

- **Everything is plain text** that you own forever
- **Headings** create structure, structure enables everything
- **TODO states** that actually make sense
- **Agenda** shows your temporal landscape
- **Tables** are spreadsheets for humans
- **Babel** enables literate programming
- **Export** to any format
- **Capture** thoughts instantly
- Org-mode is a **way of life**, not just a mode