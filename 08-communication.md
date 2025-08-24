# Chapter 8: Communication Hub
## Never Leave Emacs Again (Seriously, Why Would You?)

In 1976, the first email was read in Emacs. Today, in 2024, people are still reading email in Emacs. They're also chatting on IRC, browsing Reddit, managing their RSS feeds, and probably ordering pizza. This chapter is about turning Emacs into your communication command center, because context switching is for people who don't use Emacs.

Fair warning: After this chapter, you might never see your desktop again.

### Email: Because Mu4e > Everything Else

There are several email clients for Emacs, but mu4e (mu-for-emacs) is magical:

```elisp
;; First, install mu and mbsync (outside Emacs)
;; brew install mu mbsync  # macOS
;; apt-get install mu4e mbsync  # Debian/Ubuntu

(use-package mu4e
  :ensure nil  ; Installed with mu
  :config
  ;; Basic configuration
  (setq mu4e-maildir "~/Maildir"
        mu4e-attachment-dir "~/Downloads"
        mu4e-sent-folder "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive")
  
  ;; Sending mail
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-port 587
        smtpmail-stream-type 'starttls
        send-mail-function 'smtpmail-send-it)
  
  ;; Update every 5 minutes
  (setq mu4e-update-interval 300)
  
  ;; Show images
  (setq mu4e-view-show-images t)
  
  ;; Use imagemagick if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types)))
```

The mu4e workflow:
- `M-x mu4e` - Open mu4e
- `j` - Jump to folder
- `s` - Search
- `C` - Compose new message
- `R` - Reply
- `F` - Forward
- `d` - Move to trash
- `r` - Refile (archive)

🤔 **Why Though?** Email in Emacs means you can use all your text editing powers on emails. Org-mode integration, templates, keyboard macros—your email just became programmable.

### ERC: IRC Like It's 1988 (But Better)

```elisp
(use-package erc
  :ensure nil  ; Built-in
  :config
  (setq erc-server "irc.libera.chat"
        erc-port 6667
        erc-nick "your-nick"
        erc-user-full-name "Your Name"
        erc-track-shorten-start 8
        erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#org-mode"))
        erc-kill-buffer-on-part t
        erc-auto-query 'bury))

;; Better notifications
(use-package erc-hl-nicks
  :ensure t
  :hook (erc-mode . erc-hl-nicks-mode))

;; Desktop notifications
(use-package erc-desktop-notifications
  :ensure t
  :config
  (add-to-list 'erc-modules 'notifications))

;; Connect with:
;; M-x erc-tls  ; For SSL/TLS connections
```

ERC commands:
- `/join #channel` - Join channel
- `/part` - Leave channel
- `/msg nick message` - Private message
- `/quit` - Disconnect
- `C-c C-b` - Switch channels
- `C-c C-j` - Join channel

### Elfeed: RSS That Doesn't Suck

```elisp
(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '(("https://planet.emacslife.com/atom.xml" emacs)
          ("https://news.ycombinator.com/rss" tech news)
          ("https://www.reddit.com/r/emacs.rss" reddit emacs)
          ("https://xkcd.com/atom.xml" comic))))

;; Org integration for feed management
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))

;; Better UI
(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup))
```

Elfeed workflow:
- `M-x elfeed` - Open feed reader
- `g` - Refresh feeds
- `s` - Search/filter
- `b` - Open in browser
- `+` / `-` - Add/remove tags
- `r` - Mark as read
- `u` - Mark as unread

### Mastodon: Social Media in Emacs

```elisp
(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://mastodon.social"
        mastodon-active-user "your-username"))

;; M-x mastodon
;; Timeline: n/p for next/previous
;; Actions: f (favorite), b (boost), r (reply)
;; Post: t (new toot)
```

### Telegram/Matrix: Modern Chat

```elisp
;; Telegram
(use-package telega
  :ensure t
  :config
  (setq telega-use-images t
        telega-emoji-use-images t))

;; Matrix client
(use-package ement
  :ensure t
  :config
  (setq ement-save-sessions t))
```

### Slack: Unfortunately Necessary

```elisp
(use-package slack
  :ensure t
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t
        slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "your-team"
   :default t
   :client-id "your-client-id"
   :client-secret "your-client-secret"
   :token "your-token"))

;; Or use the newer emacs-slack alternative
(use-package emacs-slack
  :ensure t
  :config
  (setq emacs-slack-teams
        '((your-team
           :token "xoxb-your-token"
           :cookie "your-cookie"))))
```

### The Complete Communication Setup

Here's a full configuration for all your communication needs:

```elisp
;;; Communication Configuration

;; Email with mu4e
(require 'mu4e)
(setq mu4e-maildir "~/Maildir"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 300
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t)

;; Multiple accounts
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "Personal"
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/Personal" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "you@personal.com")
                  (mu4e-sent-folder . "/Personal/Sent")
                  (mu4e-drafts-folder . "/Personal/Drafts")))
        ,(make-mu4e-context
          :name "Work"
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/Work" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "you@work.com")
                  (mu4e-sent-folder . "/Work/Sent")
                  (mu4e-drafts-folder . "/Work/Drafts")))))

;; IRC
(setq erc-hide-list '("JOIN" "PART" "QUIT")
      erc-lurker-hide-list '("JOIN" "PART" "QUIT")
      erc-server-reconnect-attempts 5
      erc-server-reconnect-timeout 3)

;; RSS
(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed))

;; Notifications
(use-package alert
  :ensure t
  :config
  (when (eq system-type 'darwin)
    (setq alert-default-style 'osx-notifier))
  (when (eq system-type 'gnu/linux)
    (setq alert-default-style 'libnotify)))
```

### Email Productivity Tips

#### Email Templates

```elisp
(defun my-email-template-meeting ()
  "Insert meeting request template."
  (interactive)
  (insert "Subject: Meeting Request - [TOPIC]\n\n"
          "Hi [NAME],\n\n"
          "I'd like to schedule a meeting to discuss [TOPIC].\n\n"
          "Would any of these times work for you?\n"
          "- [TIME 1]\n"
          "- [TIME 2]\n"
          "- [TIME 3]\n\n"
          "Best regards,\n"
          user-full-name))
```

#### Org-mode Integration

```elisp
;; Capture emails to org
(use-package org-mu4e
  :ensure nil
  :config
  (setq org-mu4e-link-query-in-headers-mode nil))

;; Email to TODO
(defun my-mu4e-capture-todo ()
  "Capture email as TODO."
  (interactive)
  (org-capture nil "e"))

(add-to-list 'org-capture-templates
             '("e" "Email TODO" entry (file+headline "~/org/tasks.org" "Email")
               "* TODO %:fromname: %:subject\n%a\n%?"))
```

### Advanced IRC Configuration

```elisp
;; Auto-identify with NickServ
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      '((libera (("your-nick" . "your-password")))))

;; Logging
(setq erc-log-channels-directory "~/logs/erc/"
      erc-save-buffer-on-part t
      erc-save-queries-on-quit t)

;; Smart filtering
(use-package erc-scrolltoplace
  :ensure t
  :hook (erc-mode . erc-scrolltoplace-mode))
```

### Building Your Communication Dashboard

```elisp
(defun my-communication-dashboard ()
  "Open communication dashboard."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*mu4e-headers*")
  (split-window-horizontally)
  (other-window 1)
  (elfeed)
  (split-window-vertically)
  (other-window 1)
  (erc-track-switch-buffer 1))

(global-set-key (kbd "C-c d") 'my-communication-dashboard)
```

### Integrating Everything

```elisp
;; Send org headings as email
(defun org-send-subtree-as-email ()
  "Send current org subtree as email."
  (interactive)
  (let ((subject (nth 4 (org-heading-components)))
        (body (org-export-as 'ascii t nil t)))
    (compose-mail nil subject)
    (message-goto-body)
    (insert body)))

;; RSS to Org
(defun elfeed-to-org ()
  "Save current elfeed entry to org."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry))
        (title (elfeed-entry-title elfeed-show-entry)))
    (org-capture nil "l")
    (insert title)
    (org-insert-link nil link title)))

;; IRC to Org
(defun erc-to-org ()
  "Capture IRC conversation to org."
  (interactive)
  (org-capture nil "i"))
```

### Mobile Access Through Emacs

```elisp
;; Access Emacs from phone via SSH
;; 1. Run Emacs in daemon mode: emacs --daemon
;; 2. SSH from phone: ssh user@host
;; 3. Connect: emacsclient -t

;; Better terminal experience
(use-package emacs-everywhere
  :ensure t
  :config
  (setq emacs-everywhere-use-popup-frame t))
```

### Communication Workflows

#### The Morning Routine

```elisp
(defun morning-routine ()
  "Start the day right."
  (interactive)
  (mu4e-update-mail-and-index t)  ; Fetch mail
  (elfeed-update)                  ; Update RSS
  (org-agenda-list)                ; Show agenda
  (message "Good morning! ☕"))

(global-set-key (kbd "C-c m") 'morning-routine)
```

#### The Focus Mode

```elisp
(defun communication-focus-mode ()
  "Disable all notifications."
  (interactive)
  (setq erc-track-enable-keybindings nil
        mu4e-update-interval 0
        elfeed-search-update-hook nil)
  (message "Focus mode enabled. Notifications disabled."))

(defun communication-normal-mode ()
  "Re-enable notifications."
  (interactive)
  (setq erc-track-enable-keybindings t
        mu4e-update-interval 300)
  (message "Normal mode. Notifications enabled."))
```

### 🎮 **Communication Exercises**

1. **The Inbox Zero**: Process 50 emails using only keyboard commands
2. **The IRC Veteran**: Join 3 channels and set up auto-join
3. **The RSS Curator**: Set up 20 feeds organized by tags
4. **The Integrator**: Create an email→org→calendar workflow
5. **The Dashboard**: Build a custom communication dashboard

### What's Next?

Chapter 9 will dive into Elisp programming, giving you the power to bend Emacs to your will. But honestly, once you're reading email, chatting, and browsing feeds in Emacs, you might wonder why you need any other software at all.

The beauty of Emacs as a communication hub isn't just convenience—it's the integration. Your emails can become TODOs. Your RSS feeds can become org notes. Your IRC logs are searchable with grep. Everything is text, everything is scriptable, everything is yours.

---

*"I started using Emacs for email ironically. That was 5 years ago. I'm not sure it's ironic anymore."*
—Reformed Gmail User

---

### Chapter 8 Summary

- **Mu4e** makes email powerful and programmable
- **ERC** for IRC, because IRC never dies
- **Elfeed** for RSS that integrates with everything
- **Modern chat** with Telegram, Matrix, Slack
- **Everything is text**, therefore scriptable
- **Integration** is the superpower
- Your communication hub, your rules
- You may never need another app