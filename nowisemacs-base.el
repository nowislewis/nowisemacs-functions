;; -*- lexical-binding: t; -*-
;; basic modes
(setq use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq ring-bell-function 'ignore)       ;关闭烦人的出错时的提示声
(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)
;; 平滑地进行半屏滚动，避免滚动后recenter操作
(setq scroll-step 1
      scroll-conservatively 10000)
(setq completions-detailed t) ;;useful in emacs 28
(setq use-dialog-box nil)               ;never pop dialog
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处

(defvar nowisemacs-config-file "~/.emacs.d/init.org")
(defvar nowisemacs-doc-root-dir (file-truename "~/Documents/emacs"))
(defvar nowisemacs-doc-other-file-dir (concat nowisemacs-doc-root-dir "/other-files"))
(defvar nowisemacs-doc-org-mode-dir (concat nowisemacs-doc-root-dir "/orgmode"))
;; (setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
;; (set-frame-parameter nil 'fullscreen 'maximized)

(savehist-mode)

;; setup
(require 'setup)

(setup-define :delay
  (lambda (&optional time)
    `(run-with-idle-timer ,(or time 1) nil
                          (lambda () (require ',(setup-get 'feature)))))
  :documentation "Delay loading the feature until a certain amount of idle time has passed.")


(setup-define :load-after
    (lambda (features &rest body)
      (let ((body `(progn
                     (require ',(setup-get 'feature))
                     ,@body)))
        (dolist (feature (if (listp features)
                             (nreverse features)
                           (list features)))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
  :documentation "Load the current feature after FEATURES."
  :indent 1)

(setup-define :if-system
  (lambda (systemtype)
    `(unless (eq system-type ,systemtype)
       ,(setup-quit)))
  :documentation "If SYSTEMTYPE is not the current systemtype, stop evaluating form.")

(setup-define :autoload
  (lambda (load-func)
    (let ((body '())
          (feature-string (symbol-name (setup-get 'feature))))
      `(autoload ',load-func ,feature-string nil t)))
  :documentation "Load the current feature after FEATURES.")

(setup-define :messure-time
  (lambda ()
    (let ((feature-string (symbol-name (setup-get 'feature)))
          (load-time `(time-subtract-millis (current-time) start-time)))
      `(progn
         (message (format (concat ,feature-string ":%d") ,load-time))
         (setq start-time (current-time))
         )))
  :documentation "Messure the current feature after FEATURES.")

;; insert pairs
  (defun insert-quotations (&optional arg)
    "Enclose following ARG sexps in quotation marks.
    Leave point after open-paren."
    (interactive "*P")
    (insert-pair arg ?\' ?\'))

  (defun insert-quotes (&optional arg)
    "Enclose following ARG sexps in quotes.
    Leave point after open-quote."
    (interactive "*P")
    (insert-pair arg ?\" ?\"))

  (defun insert-backquote (&optional arg)
    "Enclose following ARG sexps in quotations with backquote.
    Leave point after open-quotation."
    (interactive "*P")
    (insert-pair arg ?\` ?\'))

  (defun insert-star (&optional arg)
    "Enclose following ARG sexps in stars.
  Leave point after open-quotation."
    (interactive "*P")
    (insert-pair arg ?\* ?\*))

  (defun insert-bracket (&optional arg)
    "Enclose following ARG sexps in brackets.
  Leave point after open-quotation."
    (interactive "*P")
    (insert-pair arg ?\[ ?\]))

  (defun insert-curly (&optional arg)
    "Enclose following ARG sexps in curly braces.
  Leave point after open-quotation."
    (interactive "*P")
    (insert-pair arg ?\{ ?\}))

  (defun insert-equate (&optional arg)
    "Enclose following ARG sexps in equations.
  Leave point after open-quotation."
    (interactive "*P")
    (insert-pair arg ?\= ?\=))

;; line number
;; only enable line number in some modes, borrowed from lazycat-emacs
  (setq display-line-numbers-width-start t)
  (setq line-number-display-limit large-file-warning-threshold)
  (setq line-number-display-limit-width 1000)

  (dolist (hook (list
                 'c-mode-common-hook
                 'c-mode-hook
                 'emacs-lisp-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'java-mode-hook
                 'asm-mode-hook

                 'haskell-mode-hook
                 'rcirc-mode-hook
                 'erc-mode-hook
                 'sh-mode-hook
                 'makefile-gmake-mode-hook
                 'python-mode-hook
                 'js-mode-hook
                 'html-mode-hook
                 'css-mode-hook
                 'tuareg-mode-hook
                 'go-mode-hook
                 'coffee-mode-hook
                 'qml-mode-hook
                 'markdown-mode-hook
                 'slime-repl-mode-hook
                 'package-menu-mode-hook
                 'cmake-mode-hook
                 'php-mode-hook
                 'web-mode-hook
                 'coffee-mode-hook
                 'sws-mode-hook
                 'jade-mode-hook
                 'vala-mode-hook
                 'rust-mode-hook
                 'ruby-mode-hook
                 'qmake-mode-hook
                 'lua-mode-hook
                 'swift-mode-hook
                 'llvm-mode-hook
                 'conf-toml-mode-hook
                 'nxml-mode-hook
                 'nim-mode-hook
                 'org-mode-hook
                 'verilog-mode-hook
                 ))
    (add-hook hook (lambda () (display-line-numbers-mode))))

;; auto-revert changed files

;;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(provide 'nowisemacs-base)
