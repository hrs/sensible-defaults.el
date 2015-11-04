;; Sensible defaults for getting started with Emacs.

;; Utility functions:

(defun sensible-defaults/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; Settings:

(defun sensible-defaults/open-files-from-home-directory ()
  (setq default-directory "~/"))

(defun sensible-defaults/increase-gc-threshold ()
    "Allow 20MB of memory (instead of 0.76MB) before calling GC."
    (setq gc-cons-threshold 20000000))

(defun sensible-defaults/backup-to-temp-directory ()
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(defun sensible-defaults/delete-trailing-whitespace ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun sensible-defaults/treat-camelcase-as-separate-words ()
  (add-hook 'prog-mode-hook 'subword-mode))

(defun sensible-defaults/automatically-follow-symlinks ()
  (setq vc-follow-symlinks t))

(defun sensible-defaults/make-scripts-executable ()
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

(defun sensible-defaults/single-space-after-periods ()
  (setq sentence-end-double-space nil))

(defun sensible-defaults/offer-to-create-parent-directories-on-save ()
  (add-hook 'before-save-hook
            (lambda ()
              (when buffer-file-name
                (let ((dir (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p dir))
                             (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                    (make-directory dir t)))))))

(defun sensible-defaults/apply-changes-to-highlighted-region ()
  (transient-mark-mode t))

(defun sensible-defaults/overwrite-selected-text ()
  (delete-selection-mode t))

(defun sensible-defaults/ensure-that-files-end-with-newline ()
  (setq require-final-newline t))

(defun sensible-defaults/confirm-closing-emacs ()
  (setq confirm-kill-emacs 'y-or-n-p))

(defun sensible-defaults/quiet-startup ()
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil))

(defun sensible-defaults/make-dired-file-sizes-human-readable ()
  (setq-default dired-listing-switches "-alh"))

(defun sensible-defaults/shorten-yes-or-no ()
  (fset 'yes-or-no-p 'y-or-n-p))

(defun sensible-defaults/always-highlight-code ()
  (global-font-lock-mode t))

(defun sensible-defaults/refresh-buffers-when-files-change ()
  (global-auto-revert-mode t))

(defun sensible-defaults/show-matching-parens ()
  (show-paren-mode t)
  (setq show-paren-delay 0.0))

(defun sensible-defaults/flash-screen-instead-of-ringing-bell ()
  (setq visible-bell t))

(defun sensible-defaults/set-default-line-length-to-80 ()
  (setq-default fill-column 80))

(defun sensible-defaults/yank-to-point-on-mouse-click ()
  (setq mouse-yank-at-point t))

(defun sensible-defaults/use-all-settings ()
  (sensible-defaults/open-files-from-home-directory)
  (sensible-defaults/increase-gc-threshold)
  (sensible-defaults/backup-to-temp-directory)
  (sensible-defaults/delete-trailing-whitespace)
  (sensible-defaults/treat-camelcase-as-separate-words)
  (sensible-defaults/automatically-follow-symlinks)
  (sensible-defaults/make-scripts-executable)
  (sensible-defaults/single-space-after-periods)
  (sensible-defaults/offer-to-create-parent-directories-on-save)
  (sensible-defaults/apply-changes-to-highlighted-region)
  (sensible-defaults/overwrite-selected-text)
  (sensible-defaults/ensure-that-files-end-with-newline)
  (sensible-defaults/confirm-closing-emacs)
  (sensible-defaults/quiet-startup)
  (sensible-defaults/make-dired-file-sizes-human-readable)
  (sensible-defaults/shorten-yes-or-no)
  (sensible-defaults/always-highlight-code)
  (sensible-defaults/refresh-buffers-when-files-change)
  (sensible-defaults/show-matching-parens)
  (sensible-defaults/flash-screen-instead-of-ringing-bell)
  (sensible-defaults/set-default-line-length-to-80)
  (sensible-defaults/yank-to-point-on-mouse-click))

;; Keybindings:

(defun sensible-defaults/bind-commenting-and-uncommenting ()
  (global-set-key (kbd "M-;")
                  'sensible-defaults/comment-or-uncomment-region-or-line))

(defun sensible-defaults/bind-home-and-end-keys ()
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line))

(defun sensible-defaults/bind-keys-to-change-text-size ()
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C-=") 'text-scale-increase)
  (define-key global-map (kbd "C-_") 'text-scale-decrease)
  (define-key global-map (kbd "C--") 'text-scale-decrease))

(defun sensible-defaults/use-all-keybindings ()
  (sensible-defaults/bind-commenting-and-uncommenting)
  (sensible-defaults/bind-home-and-end-keys)
  (sensible-defaults/bind-keys-to-change-text-size))
