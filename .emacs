;; Package init
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; Theme
(load-theme 'soft-charcoal t)

(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; Enable visual feedback on selections
(setq transient-mark-mode t)
(show-paren-mode t)

;;; Auto revert files
(global-auto-revert-mode 1)

;; Backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Column mode
(setq column-number-mode t)

;; Y or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Whitespace
(add-hook 'find-file-hook
          (lambda ()
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

(defun wb-remove-delete-whitespace-hook ()
  (interactive)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace t))

;; Trash can
(setq delete-by-moving-to-trash t)

(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$\\|\\.lx32fsl"))
(setq dired-listing-switches "-alhD")

;;; Spell checking
(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1)
                   (auto-fill-mode 1)
		   (local-set-key (kbd "C-'") 'other-window))))

;;; un-fill text
(defun wb-unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun wb-unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;;; Avoid mouse cursor
(if window-system
    (mouse-avoidance-mode 'cat-and-mouse))

;; Web browsing - Best for browing documentation.
(defun w3m-browse-url-other-window (url &optional newwin)
  (let ((w3m-pop-up-windows t))
    (if (one-window-p) (split-window nil nil t))
    (other-window 1)
    (w3m-browse-url url newwin)))

(setq browse-url-browser-function 'w3m-browse-url-other-window)

;; Term
(setq ansi-term-color-vector [unspecified “black” “red3” “lime green” “yellow3” “DeepSkyBlue3” “magenta3” “cyan3” “white”])

;; Set the frame's title. %b is the name of the buffer. %+ indicates
;; the state of the buffer: * if modified, % if read only, or -
;; otherwise. Two of them to emulate the mode line. %f for the file
;; name. Incredibly useful!
;; (setq frame-title-format "Emacs: %b %+%+ %f")
;; (setq frame-title-format '("Emacs @ " system-name ": %b %+%+ %f"))
(setq frame-title-format (concat invocation-name "@" system-name ": %b %+%+ %f"))

;;; Scroll bar
(if (require 'sml-modeline nil 'noerror)
    (progn
      (sml-modeline-mode 1)
      (scroll-bar-mode -1))
  (scroll-bar-mode 1)
  (set-scroll-bar-mode 'right))

;; default to unified diffs
(setq diff-switches "-u")

;; Copy paste between apps
(setq x-select-enable-clipboard t)

;; Disable toolbar mode
(tool-bar-mode 0)

;; Menu bar
(global-set-key [f11] 'menu-bar-mode)

;; Swap brackets and parens
(defun wb-swap-paren ()
  (interactive)
  (keyboard-translate ?\( ?\[)
  (keyboard-translate ?\[ ?\()
  (keyboard-translate ?\) ?\])
  (keyboard-translate ?\] ?\)))

;; Ido mode
(ido-mode 10)

;; Switch buffers
(global-set-key (kbd "C-'") 'other-window)

;; Fullscreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(global-set-key [f12] 'toggle-fullscreen)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Refresh directories automatically upon visiting
(setq dired-auto-revert-buffer t)

;;; Preserve case when dynamically expanding
(setq dabbrev-case-replace nil)

;;; Wordnet
(defun wn ()
  (interactive)
  (let ((word (current-word)))
    (shell-command (concat "wn " word " -over"))))

;;; My functions
(defun wb-decapitalize-first-letter ()
  "Forces the first letter of the word at point to be lowercase"
  (interactive)
  (let* ((word (current-word))
         (lower-case-word (concat (downcase (string (string-to-char word)))
                                  (substring word 1))))
    (kill-new lower-case-word)))

(defun column-name-to-label ()
  (interactive)
  (let* ((word (current-word))
         (word-list (split-string word "_"))
         (capitalized-word-list (mapcar (lambda (x)
                                          (concat (capitalize-first-letter x)
                                                  " "))
                                        word-list))
         (label (apply #'concat capitalized-word-list)))
    (kill-new (substring label 0 (- (length label) 1)))))

(defun field-name-to-sql-column ()
  (interactive)
  (save-excursion
    (replace-regexp " " "_" nil (line-beginning-position) (line-end-position))
    (replace-regexp "-" "_" nil (line-beginning-position) (line-end-position))
    (replace-regexp "/" "_" nil (line-beginning-position) (line-end-position))
    (replace-regexp "\\." "_" nil (line-beginning-position) (line-end-position))
    (downcase-region (line-beginning-position) (line-end-position))))



;;; Initialize workspace
(defun wb-acceptable-file-to-open (file)
  (or (string= (file-name-extension file) "java")
      (string= (file-name-extension file) "sql")
      (string= (file-name-extension file) "conf")
      (string= (file-name-extension file) "sh")
      (string= (file-name-extension file) "md")
      (string= (file-name-extension file) "markdown")))

(defun wb-load-all-files-in-root-directory (directory)
  (mapc (lambda (item)
          (when (not (string= "." (subseq (file-name-nondirectory item) 0 1)))
            (cond ((and (file-directory-p item) (not (file-symlink-p item)) (not (string= "target" (file-name-nondirectory item ))))
                   (wb-load-all-files-in-root-directory item))
                  ((and (not (file-directory-p item)) (wb-acceptable-file-to-open item))
                   (find-file-other-window item))
                  (t nil))))
        (directory-files directory 'absolute)))

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

(dolist (hook '(emacs-lisp-mode-hook
		lisp-mode-hook
		slime-repl-mode-hook))
  (add-hook hook #'(lambda () (paredit-mode 1))))

;;; Bell
(setq visible-bell 1)

;;; mode line
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "dark green")

;;; Highlight the line your are on.
(global-hl-line-mode 1)
(set-face-background 'hl-line "gray28")

;;; Lisp setup
(when (boundp 'wb-paredit-location)
  (load wb-paredit-location))

(when (boundp 'wb-load-slime)
  (setq inferior-lisp-program "sbcl --noinform")
  (load (expand-file-name wb-load-slime))
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy slime-asdf slime-xref-browser slime-indentation slime-mrepl))
  (add-hook 'lisp-mode-hook
	    (lambda ()
	      (define-key lisp-mode-map (kbd "C-;") 'slime-complete-symbol))))

;;; Tabs and indentation.
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 120 4))

;;; For SQL files I like to set tab spaces to 2
(defun local-sql-mode-hook-fun ()
  (setq tab-stop-list (number-sequence 2 120 2))
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

(add-hook 'sql-mode-hook 'local-sql-mode-hook-fun)

;;; Copy current file name to clipboard.
;;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun wb-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;; Python
(setq python-shell-interpreter "python3")

;;; SQLI
(dolist (hook '(sql-interactive-mode-hook comint-mode))
  (add-hook hook
            (lambda ()
              (local-set-key (kbd "C-c C-l C-l") 'comint-dynamic-list-input-ring))))
