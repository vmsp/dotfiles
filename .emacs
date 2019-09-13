;; -*- lexical-binding: t; -*-

;;; General

;; Turn off gui niceties
(scroll-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(unless (display-graphic-p) (menu-bar-mode -1))

;; Set font
;; (cond
;;  ((eq system-type 'darwin)
;;   (set-frame-font "Input Mono Narrow-12"))
;;  ((eq system-type 'gnu/linux)
;;   (set-frame-font "DejaVu Sans Mono-10"))
;;  ((eq system-type 'windows-nt)
;;   (set-frame-font "Consolas-11")))

;; turn off a few things, for faster startup, and then reset them
(defvar my-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1
      file-name-handler-alist nil)

(defun my-reset-defaults ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1
        file-name-handler-alist my-file-name-handler-alist))
(add-hook #'after-init-hook #'my-reset-defaults)

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

;; package.el
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

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
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Ignore case for file completion
(setq read-file-name-completion-ignore-case t)

;; visible marked region
(transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; less prompts
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil
      kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; create file and parent directories if needed
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                                 parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;; deleted files to go trash
(setq delete-by-moving-to-trash t)

;; Hide mouse while typing
(setq make-pointer-invisible t)

;; Tab size
(setq-default tab-width 2)

;; Indent with spaces, by default
(setq-default indent-tabs-mode nil)

;; Open symlinks
(setq-default find-file-visit-truename nil)

;; case insensitive search
(setq search-upper-case nil)
(setq isearch-case-fold-search 'yes)

;; Turn on auto-fill mode
(setq-default fill-column 80)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; Auto refresh buffers
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Save a list of recent files visited. Open recent files with C-x C-r
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Disable fringe
(fringe-mode 0)

;; Save on lost focus
(add-hook 'focus-out-hook
          (lambda () (save-some-buffers t)))

;; require newline before EOF
(setq require-final-newline t)

;; Save history
(setq savehist-additional-variables
      '(search-ring regexp-search-ring)
      savehist-file (concat user-emacs-directory "savehist")
      history-length 1000)
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

;; ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
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
  (global-anzu-mode 1))

;; Keep cursor away from edges when scrolling up/down
(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode 1))

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
      ido-use-virtual-buffers t
      ;; If I'm not quick to press RET, ido finds a file with a similar name in
      ;; whatever directories I've recenty used. Disable this behaviour.
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

;; GPG

(use-package epa-file
  :defer t
  :config
  (setq epa-pinentry-mode 'loopback)
  (epa-file-enable))

;;; Appearance

;; (load-theme 'leuven t)

(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox t))

;; (use-package monokai-theme
;;   :ensure t
;;   :config (load-theme 'monokai t))

;; (use-package rebecca-theme
;;   :ensure t
;;   :config (load-theme 'rebecca t))

;; (use-package dracula-theme
;;   :ensure t
;;   :config (load-theme 'dracula t))

;;; macOS

;; (setq mac-pass-command-to-system nil)
(setq ns-pop-up-frames nil
      ns-command-modifier 'meta
      ns-option-modifier 'none)

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config (exec-path-from-shell-initialize))

;;; dired

(use-package dired
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("RET" . dired-find-alternate-file)
         ("C-x C-q" . wdired-change-to-wdired-mode))
  :init
  (defun my-dired-mode-hook ()
    ;; (disable-command 'dired-find-file-other-buffer)
    (dired-hide-details-mode t))
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired nil))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
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
 ;; ("M-p" . backward-paragraph)
 ;; ("M-n" . forward-paragraph)
 ("C-s" . isearch-forward-regexp)
 ("C-r" . isearch-backward-regexp)
 ("M-%" . query-replace-regexp)
 ("M-z" . undo)
 ("C-x k" . kill-this-buffer)
 ("C-x C-b" . ibuffer)
 ("C-." . hippie-expand)
 ("C-x TAB" . imenu)
 ("C-x C-r" . recentf-open-files))

(bind-keys
 ("M-p" . backward-paragraph)
 ("M-n" . forward-paragraph))

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

(bind-key "C-c C-f" #'my-cleanup-buffer)

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

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to
NEW-NAME. Blatantly stolen from Steve Yegge."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

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
;; (add-hook 'prog-mode-hook 'hl-line-mode)

;; auto insert for include guards and such
(setq auto-insert-alist '()
      auto-insert-query nil)
(auto-insert-mode 1)

(defun my-prog-mode-hook ()
  (abbrev-mode -1)
  ;; (display-line-numbers-mode)
  (font-lock-add-keywords
   nil
   ;; TODO(<identifier>): Highlight TODOs like this.
   '(("\\<\\(TODO\\)\\((.+)\\)?:" 1 'font-lock-warning-face prepend))))

(add-hook 'prog-mode-hook #'my-prog-mode-hook)

;; Licenses

(setq my-licenses
      '((proprietary . "Copyright 2018 Vitor Pereira.

Proprietary and confidential. Unauthorized reproduction or
transmission of this file in any form, in whole or in part, is
strictly prohibited.")
        (apachev2 . "Copyright 2018 Vitor Pereira.

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
    ;; (c-guess)
    (c-set-offset 'inextern-lang '0))
  (add-hook 'c-mode-common-hook #'my-c-mode-common-hook))

(defun my-include-guard-name ()
  (let* ((fname (buffer-file-name))
         (rel-fname (file-relative-name fname (locate-dominating-file
                                               fname ".git"))))
    (upcase (concat (replace-regexp-in-string "[^[:alnum:]]" "_" rel-fname)
                    "_"))))

(define-auto-insert
  '("\\.\\(h\\|hpp\\|hxx\\)\\'" . "C/C++/ObjC Header")
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

;; Protocol Buffers

(use-package protobuf-mode
  :load-path "/usr/local/opt/protobuf/share/doc/protobuf/editors"
  :mode "\\.proto\\'"
  :config (add-hook 'protobuf-mode-hook 'turn-on-auto-fill))

;; build systems

(use-package cmake-mode
  :load-path "/usr/local/share/emacs/site-lisp/cmake"
  :mode "\\.cmake\\|CMakeLists.txt\\'")

(define-auto-insert
  '("\\.cmake\\|CMakeLists.txt\\'" . "CMake")
  '(nil (my-license "# ")))

(use-package gn-mode
  :load-path "~/src/third_party/gn/tools/gn/misc/emacs"
  :mode "\\.gni?\\'")

(define-auto-insert
  '("\\.gn\\'" . "GN")
  '(nil (my-license "# ")))

;; swift

(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'")
  :init
  (setq swift-mode:basic-offset 2))

;; python

(setq python-fill-docstring-style 'pep-257-nn)

;; (setq python-indent-offset 2)

;; (defun my-python-indent-continuation (origfun &rest args)
;;   (pcase (python-indent-context)
;;     (`(:inside-paren-newline-start . ,start)
;;      (save-excursion
;;        (goto-char start)
;;        (+ (current-indentation)
;;           (* python-indent-offset python-indent-def-block-scale))))
;;     (_ (apply origfun args))))

;; (advice-add #'python-indent--calculate-indentation
;;             :around
;;             #'my-python-indent-continuation)

(define-auto-insert
  '("\\.py\\'" . "Python header")
  '(nil (my-license "# ")))

;; ruby

;; (use-package chruby
;;   :ensure t
;;   :commands chruby-use-corresponding)

;; org

(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :init (setq org-startup-indented t
              org-startup-truncated nil))

;; SQL

(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :commands sql-mode-hook
  :init
  (use-package sqlup-mode
    :ensure t
    :hook sql-mode)
  (use-package sql-indent
    :ensure t
    :hook (sql-mode . sqlind-minor-mode))
  :config
  (sql-highlight-postgres-keywords))

;; js

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :init
  (setq js-indent-level 2
        js2-strict-missing-semi-warning nil
        js2-mode-show-strict-warnings nil
        js2-include-node-externs t))

;; jsx

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx\\'")

;; ts

(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?\\'"
  :init
  (setq typescript-indent-level 2))

;; web

(use-package web-mode
  :ensure t
  :mode (("\\.\\(html\\|dtl\\)\\'" . web-mode)
         ("\\.html.erb\\'" . web-mode)
         ("\\.js.erb\\'" . web-mode)
         ("\\.svelte\\'" . web-mode))
  :commands web-mode-hook
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        ;; web-mode-enable-auto-pairing nil
        ;; web-mode-auto-quote-style 2
        web-mode-comment-formats '(("javascript" . "//")
                                   ("jsx" . "//"))))

(use-package css-mode
  :mode "\\.css\\'"
  :init (setq css-indent-offset 2))

;; graphql

(use-package graphql-mode
  :ensure t
  :mode ("\\.graphql\\'"))

;; kdb+/q

(use-package q-mode
  :load-path "~/src/third_party/q-mode"
  :mode "\\.q\\'")

;; erlang

(use-package erlang
  :ensure t
  :mode ("\\(\\.[he]rl\\|\\(relx\\|rebar\\|sys\\).config\\)\\'" . erlang-mode)
  :init
  ;; restores regular newline and indent
  (setq erlang-electric-commands '(erlang-electric-newline)
        erlang-electric-newline-inhibit nil)
  (setq erlang-indent-level 2
        erlang-check-module-name t))

(define-auto-insert
  '("\\(\\.[he]rl\\|\\(relx\\|rebar\\|sys\\).config\\)\\'" . "Erlang")
  '(nil (my-license "%% ")))

;; go

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

;; clojure

;; (use-package clojure-mode
;;   :mode (("\\.clj\\'" . clojure-mode)
;;          ("\\.cljs\\'" . clojurescript-mode))
;;   :config
;;   (add-hook #'clojure-mode-hook #'turn-on-eldoc-mode)
;;   (add-hook #'clojurescript-mode-hook #'turn-on-eldoc-mode))

;; (use-package cider
;;   :ensure t
;;   :defer t)

;; hack

(use-package hack-mode
  :ensure t
  :mode (("\\.hack\\'" . hack-mode)))

;; markdown

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-enable-math t)
  (setq markdown-command "multimarkdown")
  :config
  (add-hook 'markdown-mode-hook 'turn-on-flyspell))

;; sh

(setq sh-basic-offset 2
      sh-indentation 2)

;; lua

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :init
  (setq lua-indent-level 2))

;; yaml

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

;; docker

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;; nginx

(use-package nginx-mode
  :ensure t
  :mode "nginx.conf\\'")

;;; eshell

(use-package eshell
  :bind ("C-x m" . eshell)
  :init
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  :config
  (use-package em-smart
    :init
    (setq eshell-smart-space-goes-to-end t)))

;;; Etc.

(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  :custom
  (magit-set-upstream-on-push (quote dontask)))


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
  :bind ("C--" . goto-last-change))


(use-package winum
  :ensure t
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map))
  :config (winum-mode))


(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))


(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)))


(use-package treemacs
  :ensure t
  :bind (("<f8>" . treemacs-select-window)
         :map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action))
  :init
  (setq treemacs-no-png-images t
        treemacs-width 42))


(use-package treemacs-projectile
  :ensure t
  :defer t)


(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("M-." . projectile-find-file)
         ("M-F" . projectile-grep)))


(use-package projectile-rails
  :ensure t
  :hook (ruby-mode . projectile-rails-mode)
  :bind-keymap ("C-l" . projectile-rails-command-map))


(use-package avy
  :ensure t
  :bind ("M--" . avy-goto-char))


(use-package bazel-mode
  :load-path "~/src/third_party/emacs-bazel-mode"
  :mode ("\\.bzl\\'"
         "WORKSPACE\\'"
         "BUILD\\'"))
