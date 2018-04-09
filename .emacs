;;; General

(setq debug-on-error t)

;; Turn off gui niceties
(scroll-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

;; Set font
(cond
 ((eq system-type 'darwin)
  (set-default-font "Menlo-12"))
 ((eq system-type 'gnu/linux)
  (set-default-font "DejaVu Sans Mono-10"))
 ((eq system-type 'windows-nt)
  (set-default-font "Consolas-11")))

;; prevent gc during startup then set it back
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 20000000)))

;; Disable startup message
(setq inhibit-startup-screen t)

;; Disable *scratch* message
(setq initial-scratch-message nil)

;; Don't beep
(setq ring-bell-function 'ignore)

;; Start in fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;; Keep customizations in their own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize t)

(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Always load newer files
(setq load-prefer-newer t)

;; Also show column numbers
(column-number-mode 1)

;; Always use UTF-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; *-unix variants use \n line endings even on windows
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Ignore case for file completion
(setq read-file-name-completion-ignore-case t)

;; visible marked region
(transient-mark-mode t)

;; less prompts
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil
      kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(defun find-file--make-directory-maybe (filename &optional wildcards)
  "When creating a file also create parent directories if needed."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(advice-add 'find-file :before #'find-file--make-directory-maybe)

;; deleted files to go trash
(setq delete-by-moving-to-trash t)

;; Hide mouse while typing
(setq make-pointer-invisible t)

;; Tabs are spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Open symlinks
(setq-default find-file-visit-truename nil)

;; case insensitive search
(setq search-upper-case nil)
(setq isearch-case-fold-search 'yes)

;; Turn on auto-fill mode
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; Auto revert if file changed on disk
(global-auto-revert-mode t)

;; Disable fringe
(fringe-mode 0)
;; (set-fringe-mode '(0 . 0))

;; Save on lost focus
(add-hook 'focus-out-hook
          (lambda () (save-some-buffers t)))

;; require newline before EOF
(setq require-final-newline t)

;; Save history
(setq savehist-additional-variables
      '(search-ring regexp-search-ring)
      savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode t)

;; Backups
(setq delete-auto-save-files t
      delete-old-versions t
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Disable lockfiles
(setq create-lockfiles nil)

;; Disable stuff we don't need
(global-auto-composition-mode 0)
(auto-compression-mode 0)
(auto-encryption-mode 0)

;; paste behaviour
(setq save-interprogram-paste-before-kill t)

;; ediff always in a single frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; uniquify
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

;; undo for windows
(winner-mode t)

;; apropos search everywhere
(setq apropos-do-all t)

;; show number of matches when searching
(use-package anzu
  :ensure t
  :config
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "orange" :weight 'bold)
  (global-anzu-mode 1))

;; saveplace
(setq save-place-file (concat user-emacs-directory "places"))
(save-place-mode 1)

;; quick access to .emacs
(set-register ?i '(file . "~/.emacs"))
(set-register ?o '(file . "~/org"))

;; ido
(ido-mode 1)
(ido-everywhere 1)

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-faces nil
      ;; If I'm not quick to press RET, ido finds a file with a
      ;; similar name in whatever directories I've recenty
      ;; used. Disable this behaviour.
      ido-auto-merge-work-directories-length nil)

(use-package flx-ido
  :ensure t
  :config (flx-ido-mode 1))

(use-package ido-completing-read+
  :ensure t
  :config (ido-ubiquitous-mode 1))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; project.el

(setq project-vc-ignores '("third_party"))

;; GPG

(use-package epa-file
  :defer t
  :config
  (setq epa-pinentry-mode 'loopback)
  (epa-file-enable))

;;; Appearance

;; (load-theme 'leuven t)
;; (custom-set-faces
;;  `(fringe ((t (:background ,(face-background 'default)))))
;;  `(font-lock-comment-face ((t (:slant normal)))))

(use-package solarized-theme
  :ensure t
  :config
  ;; fix weird modeline underline
  (setq x-underline-at-descent-line t)
  (load-theme 'solarized-light t))

;;; macOS

(setq mac-pass-command-to-system nil)
(setq ns-pop-up-frames nil)
(setq ns-command-modifier 'meta
      ns-option-modifier 'none)

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config (exec-path-from-shell-initialize))

;;; dired

(use-package dired
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("C-x C-q" . wdired-change-to-wdired-mode))
  :init
  (defun my-dired-mode-hook ()
    (dired-hide-details-mode t))

  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired nil))
  :config
  (use-package dired-x
    :config
    (setq-default dired-omit-files-p t)
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))
  (add-hook 'dired-mode-hook #'my-dired-mode-hook))

;;; flyspell

(setenv "DICTIONARY" "en_US")

(use-package flyspell
  :bind (:map flyspell-mode-map
              ([down-mouse-3] . flyspell-correct-word))
  :init
  (setq ispell-really-hunspell t))

;;; Keybindings

(when (eq system-type 'darwin)
  (bind-key "M-§" 'other-frame))

(when (eq system-type 'windows-nt)
  (bind-key "C-z" 'undo))

(bind-keys
 ("M-p" . backward-paragraph)
 ("M-n" . forward-paragraph)
 ("C-s" . isearch-forward-regexp)
 ("C-r" . isearch-backward-regexp)
 ("M-%" . query-replace-regexp)
 ("M-z" . undo)
 ("C-x k" . kill-this-buffer)
 ("C-x C-b" . ibuffer)
 ("C-." . hippie-expand)
 ("C-x TAB" . imenu)
 ("C-x m" . eshell)
 ("C-," . project-find-file))

;; tab indents and autocompletes
(setq tab-always-indent 'complete)

(defun my-kill-line ()
  (interactive)
  (if (bolp)
      (kill-whole-line)
    (if (looking-at "[[:space:]]*$")
        (delete-indentation t)
      (kill-line))))

(bind-key "C-k" #'my-kill-line)

(defun my-cleanup-buffer ()
  (interactive)
  (if (use-region-p)
      (progn
        (indent-region (region-beginning) (region-end))
        (delete-trailing-whitespace begin end))
    (indent-region (point-min) (point-max))
    (delete-trailing-whitespace)))

(bind-key "C-c n" #'my-cleanup-buffer)

(defun my-swap-window-buffers ()
  (interactive)
  (let ((this-buffer (window-buffer))
        (other-buffer (window-buffer (next-window))))
    (set-window-buffer (selected-window) other-buffer)
    (set-window-buffer (next-window) this-buffer)))

(bind-key* "M-0" #'my-swap-window-buffers)

(defun my-comment-dwim (&optional arg)
  (interactive "P")
  (if (not (use-region-p))
      (comment-line 1)
    (comment-dwim arg)))

(bind-key* "M-;" #'my-comment-dwim)

;;; All languages

;; Always pair parenthesis
(setq electric-pair-preserve-balance t
      electric-pair-delete-adjacent-pairs t
      electric-pair-open-newline-between-pairs nil)
(electric-pair-mode 1)

;; Always show matching parens
(show-paren-mode 1)

;; Force indentation
(electric-indent-mode 1)

;; correct M-(f/b) in CamelCase
(global-subword-mode t)

;; highlight current line
(global-hl-line-mode t)

;; auto insert for include guards and such
(setq auto-insert-alist '())
(setq auto-insert-query nil)
(auto-insert-mode 1)

(defun my-prog-mode-hook ()
  (abbrev-mode -1)
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\)\\>" 1 'font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'my-prog-mode-hook)

;; Licenses

(setq my-licenses
      '((proprietary . "Copyright 2018 Vitor Pereira

Proprietary and confidential. Unauthorized reproduction or
transmission of this file in any form, in whole or in part, is
strictly prohibited.")
        (apachev2 . "Copyright 2018 Vitor Pereira

Licensed under the Apache License, Version 2.0 (the \"License\");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an \"AS IS\" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.")))

(defun my-license (prefix)
  (when (boundp 'my-use-license)
    (concat
     (replace-regexp-in-string
      "^" prefix
      (cdr (assq my-use-license my-licenses)))
     "\n\n")))

;; c/c++/objc

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*#import" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(use-package find-file
  :config
  (nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".mm"))
  (add-to-list 'cc-other-file-alist '("\\.mm\\'" (".h"))))

(defun my-compile-with-makefile ()
  (interactive)
  (let ((makefile-dir (locate-dominating-file default-directory "Makefile")))
    (if makefile-dir
        (with-temp-buffer
          (cd makefile-dir)
          (compile "make -k"))
      (error "Makefile not found in parent directories"))))

(defun my-compile-with-cmake ()
  (interactive)
  (let ((cmakelists-dir
         (locate-dominating-file default-directory "CMakeLists.txt")))
    (if cmakelists-dir
        (with-temp-buffer
          (cd (concat cmakelists-dir "build"))
          (compile "make -k"))
      (error "CMakeLists.txt not found in parent directories"))))

;; place point at the first error
(setq compilation-scroll-output 'first-error)

(defun my-c-initialization-hook ()
  (bind-key "C-c o" #'ff-find-other-file c-mode-base-map)
  (bind-key "C-c C-c" #'my-compile-with-cmake c-mode-base-map))

(add-hook 'c-initialization-hook #'my-c-initialization-hook)

(use-package google-c-style
  :ensure t
  :commands google-set-c-style
  :init
  (defun my-c-mode-common-hook ()
    (google-set-c-style)
    (c-set-offset 'inextern-lang '0))
  (add-hook 'c-mode-common-hook #'my-c-mode-common-hook))

(defun my-include-guard-name ()
  (let* ((fname (buffer-file-name))
         (rel-fname (file-relative-name fname
                                        (locate-dominating-file fname ".git"))))
    (upcase (concat (replace-regexp-in-string "[^[:alnum:]]" "_" rel-fname)
                    "_"))))

(define-auto-insert
  '("\\.\\(h\\|hh\\|hpp\\|hxx\\)\\'" . "C/C++/ObjC Header")
  '((my-include-guard-name)
    (my-license "// ")
    "#ifndef " str n "#define " str n n _ n n "#endif  // " str))

(define-auto-insert
  '("\\.\\(c\\|cc\\|cpp\\|cxx\\|mm?\\)\\'" . "C/C++/ObjC Source")
  '(nil
    (my-license "// ")))

(define-auto-insert
  '("\\.mk\\|Makefile\\'" . "Makefile")
  '(nil (my-license "# ")))

(use-package protobuf-mode
  :load-path "/usr/local/opt/protobuf/share/doc/protobuf/editors"
  :mode "\\.proto\\'")


(use-package cmake-mode
  :load-path "/usr/local/share/emacs/site-lisp/cmake"
  :mode "\\.cmake\\|CMakeLists.txt\\'")

(define-auto-insert
  '("\\.cmake\\|CMakeLists.txt\\'" . "CMake")
  '(nil (my-license "# ")))

(use-package gn-mode
  :load-path "~/.emacs.d/third_party/"
  :mode "\\.gni?\\'")

(define-auto-insert
  '("\\.gn\\'" . "GN")
  '(nil (my-license "# ")))


(use-package bazel-mode
  :ensure t
  :mode "\\.bzl\\|WORKSPACE\\'")

;; swift

;; (use-package swift-mode
;;   :load-path "~/.emacs.d/third_party/"
;;   :mode ("\\.swift\\'"))

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'"
  :init
  (setq swift-mode:basic-offset 2
        swift-mode:multiline-statement-offset 4
        swift-mode:parenthesized-expression-offset 4))

;; assembly

(use-package asm-mode
  :defer t
  :init
  (setq asm-comment-char ?\/)
  (font-lock-add-keywords 'asm-mode cpp-font-lock-keywords))

;; python

;; (use-package python
;;   :mode ("\\.py\\|\\.bzl\\|BUILD\\|WORKSPACE\\'" . python-mode))

(define-auto-insert
  '("\\.py\\'" . "Python header")
  '(nil (my-license "# ")))

;; org

(use-package org
  :defer t
  :init (setq org-startup-indented t))

;; SQL

(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :commands sql-mode-hook
  :bind (:map sql-mode-map
              ("TAB" . self-insert-command))
  :init
  (use-package sqlup-mode
    :ensure t
    :commands sqlup-mode
    :init (add-hook 'sql-mode-hook #'sqlup-mode))
  :config
  (sql-highlight-postgres-keywords))

;; web

(use-package web-mode
  :ensure t
  :mode "\\.djhtml\\'"
  :commands web-mode-hook
  :init
  (setq web-mode-markup-indent-offset 2)
  (defun my-web-mode-hook ()
    (setq-local electric-pair-inhibit-predicate
                (lambda (c)
                  (if (char-equal c ?{)
                      t
                    (electric-pair-default-inhibit c)))))
  (add-hook 'web-mode-hook #'my-web-mode-hook))

(use-package css-mode
  :mode "\\.css\\'"
  :init (setq css-indent-offset 2))

;; kdb+/q

(use-package q-mode
  :load-path "~/src/third_party/q-mode"
  :mode "\\.q\\'")

;; erlang

(use-package erlang
  :ensure t
  :mode ("\\.erl\\'". erlang-mode)
  :bind (:map erlang-mode-map
              ("RET" . electric-newline-and-maybe-indent))
  :init
  (setq erlang-electric-commands '())
  :config
  (setq erlang-indent-level 2)
  (defun my-erlang-mode-hook ()
    (electric-indent-local-mode 0))
  (add-hook 'erlang-mode-hook 'my-erlang-mode-hook))

;; markdown

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; latex

(add-to-list 'auto-mode-alist '("\\.dot\\'" . c-mode))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq-default TeX-engine 'xetex)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-extra-bindings t)
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))

;; sh

(setq sh-basic-offset 2
      sh-indentation 2)

(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'"
  :init
  (setq fish-indent-offset 2))

;;; Etc.

(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (custom-set-variables '(magit-set-upstream-on-push (quote dontask))))


(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))


(use-package expand-region
  :ensure t
  :bind (("C-c e" . er/expand-region)
         ("C-c d" . er/contract-region)))


(use-package toggle-quotes
  :ensure t
  :bind ("M-\"" . toggle-quotes))


(use-package goto-last-change
  :ensure t
  :bind ("C-\\" . goto-last-change))


(use-package window-numbering
  :ensure t
  :config (window-numbering-mode t))


(use-package ws-butler
  :ensure t
  :commands ws-butler-mode
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))


(use-package dumb-jump
  :ensure
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)))


(use-package esup
  :ensure
  :commands esup)
