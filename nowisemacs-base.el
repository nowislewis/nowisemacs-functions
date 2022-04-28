;; -*- lexical-binding: t; -*-
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

;; (setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
;; (set-frame-parameter nil 'fullscreen 'maximized)

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

(provide 'nowisemacs-base)
