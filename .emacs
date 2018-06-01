(require 'package)
(package-initialize)

;; ======================================================================
;; Testing
;; ======================================================================

;; Enable stacktrace on elisp error:
;; (setq debug-on-error t)

(defun flycheck-refresh()
  (interactive)
  (flycheck-mode)
  (flycheck-mode))

;; ======================================================================
;; General
;; ======================================================================
(require 'flycheck)
(require 'swiper)

;; ===== MELPA =====
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; ===== C-tools =====
(add-to-list 'load-path "~/.emacs.d/elpa/c-tools")
(require 'c-tools)

;; ===== Doom themes =====
(require 'doom-themes)
;; Enable bolding and italics with doom themes
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
;; Load theme
(load-theme 'doom-one t)
;; (load-theme 'doom-vibrant t)
;; ^ see page for other details

;; ===== Atom One Dark Theme =====
;; (require 'atom-one-dark-theme)
;; (load-theme 'atom-one-dark t)

;; ===== Powerline =====
(require 'powerline)
(powerline-default-theme)

;; ===== Good Options =====
;; (menu-bar-mode -1) ;; disable menu bar
(tool-bar-mode -1) ;; disable tool bar
(scroll-bar-mode -1) ;; disable scroll bar
(global-linum-mode t) ;; line nums
;; (setq linum-format " %d")
(global-hl-line-mode) ;; highlight current line
(setq select-enable-clipboard t) ;; good clipboard stoof
(setq inhibit-splash-screen t) ;; disable the welcome screen
(setq-default fill-column 80) ;; 80 character lines by default w/ auto-fill-mode
(global-auto-revert-mode t) ;; Constant page refreshment and reversion
;; small compilation window
(setq split-height-threshold 0)
(setq compilation-window-height 10)


;; My current emacs C source dir
(setq source-directory "~/downloads/emacs-25.3/src/")

;; ===== Bind keys =====
(global-set-key (kbd "C-j") 'newline-and-indent) ;; equal to RET
(global-set-key (kbd "C-<f11>") 'compile) ;; C-f11 = compile
(global-set-key (kbd "C-\'") 'company-complete) ;; auto-complete
(global-set-key (kbd "C-;") 'iedit-mode) ;; toggle iedit
(global-set-key (kbd "M-.") 'helm-gtags-dwim) ;; helm gtags jump to def
(global-set-key (kbd "M-n") 'next-block-soe)
(global-set-key (kbd "M-p") 'previous-block-soe)
(global-set-key (kbd "M-DEL") 'backward-delete-word)
(global-set-key (kbd "C-x C-z") 'save-buffers-kill-terminal) ;; new save+exit
;; (global-set-key (kbd "C-c o") 'window-manage-mode)
(global-set-key (kbd "C-o") 'window-manage-mode) ;; was open-line
(global-set-key (kbd "C-c k") 'path-to-clip)

;; ===== Unbind keys =====
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-z")) ;; disable minimize
(global-unset-key (kbd "C-r")) ;; disable reverse isearch (using swiper)

;; The command comment-region (C-c C-c) seems kind of pointless/useless
;; considering the utility of comment-line (C-x C-;)
(global-unset-key (kbd "C-c C-c"))

;; ===== Font ======
(set-face-attribute 'default nil
		    ;; :family "DejaVu Sans Mono"
		    ;; :height 110)
		    :family "Inconsolata"
		    :height 130)


(set-face-attribute 'variable-pitch nil
		    ;; :family "DejaVu Sans Mono"
		    ;; :height 110)
		    :family "Inconsolata"
		    :height 130)
		    

;; ===== Colors =====
(set-face-attribute 'mode-line nil
		    :background "#5294e2"
		    :foreground "#f3f4f5"
		    :distant-foreground "#e3e4e5")

(set-face-attribute 'cursor nil
		    :background "#5294e2")

(set-face-attribute 'vertical-border nil
		    :foreground "#5294e2")
                    ;; :foreground "#1d2026")
                    ;; :background "51afef")

(set-face-attribute 'linum nil
		    :background "#2f343f"
		    :foreground "#6f7986")

(set-face-attribute 'swiper-line-face nil
		    :weight 'bold
		    :background "#5294e2"
		    :foreground "#d5d4d3")

(set-face-attribute 'swiper-match-face-2 nil
		    :weight 'bold
		    :background "#dcaeea"
		    :foreground "#1b2229")

(set-face-attribute 'swiper-match-face-3 nil
		    :weight 'bold
		    :background "#eccf7a"
		    :foreground "#1b2229")

;; Interestingly enough, the color and background for this have
;; already been set! :O
(set-face-attribute 'swiper-match-face-4 nil
		    :weight 'bold)
		    ;; :background "#98be65"
                    ;; :foreground "#1b2229")

;; ===== Semantic (Global) =====
(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

;; ===== Irony =====
;; MUST BE BEFORE COMPANY FOR company-irony-c-headers and other packages to work
(require 'irony)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; ===== Company (Global) + company-eclim =====
(require 'company)
(require 'company-irony)
(require 'company-c-headers)
(require 'company-irony-c-headers)

;; For eclim
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(setq company-emacs-eclim-ignore-case t)

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends
		'(company-irony-c-headers ;;company-c-headers
		  company-semantic company-irony company-anaconda)))

;; Company keybinds
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; ===== Yasnippet (Global) =====
(require 'yasnippet)
(yas-global-mode 1)

;; ===== Smartparens (Global) =====
(require 'smartparens-config)
;; (smartparens-global-mode)
(show-paren-mode 1)

;; ===== Help Window Changes =====
;; (Shrinks help buffers to 20% size)
(add-to-list 'help-mode-hook
 	     (lambda() (interactive)
	       (when (not (get-buffer-window "*Help*"))
		 (save-selected-window
		   (save-excursion
		     (let* ((w (split-window-vertically)))
		       (select-window w)
		       (switch-to-buffer "*Help*")
		       (enlarge-window (- 15 (window-size)))))))))

;; ======================================================================
;; Helm (General continued)
;; ======================================================================

;; helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c C-f") 'helm-projectile)
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ;; better yank cycle
(global-set-key (kbd "C-c r") 'helm-recentf)      ;; recently opened files
(helm-mode 1)

;; Tagging incremental completion
(require 'helm-gtags)
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Helm-gtags keybinds
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;;; Helm keybinds
(global-set-key (kbd "C-x b") 'helm-mini)  ;; use helm minibuffer
(global-set-key (kbd "C-s") 'swiper-helm)  ;; swiper!

;; autoresize mode
(helm-autoresize-mode 1)
;; define size (part of autoresize mode)
(setq helm-autoresize-max-height 25)
(setq helm-autoresize-min-height 25)

;; helm window in current buffer
;; (setq helm-split-window-in-side-p t)

;; Helm colors
(set-face-attribute 'helm-candidate-number nil
		    :background "#dcaeea")

(set-face-attribute 'helm-selection nil
		    ;; :background "#51afef"
		    :background "#5294E2"
		    :foreground "#d5d4d3")

;; (set-face-attribute 'dired-directory nil
;; 		    :foreground "#dcaeea")

;; ======================================================================
;; C/C++
;; ======================================================================
;; C style!
;; (setq c-default-style "linux") ;; linux default style
(setq c-basic-offset 3) ;; C indentation size of 3

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; ===== C++ =====
(defun my-c++-mode-hook()
  (auto-fill-mode)
  (flycheck-mode)
  ;; for c++ headers (Update regularly)
  (setq flycheck-clang-include-path
	(list "usr/include/c++/7.3.1/" "/usr/include/X11"
	      "/usr/include/GL" "/usr/include/boost"))
  (setq flycheck-gcc-include-path
	(list "usr/include/c++/7.3.1/" "/usr/include/X11"
	      "/usr/include/GL" "/usr/include/boost"))
  
  (flycheck-select-checker 'c/c++-gcc)

  ;; also for indentation
  (c-set-offset 'substatement-open 0)
  (irony-mode))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; ===== C =====
(defun my-c-mode-hook ()
  (auto-fill-mode)
  (flycheck-mode)
  (flycheck-select-checker 'c/c++-gcc)
  (c-set-offset 'substatement-open 0)
  (irony-mode))

(add-hook 'c-mode-hook 'my-c-mode-hook)

;; ======================================================================
;; Java (eclim)
;; ======================================================================
(require 'eclim)
(require 'eclimd)
(setq eclimd-autostart t)

;; company-eclim
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

(defun my-java-mode-hook()
  (eclim-mode t)
  (local-set-key (kbd "<C-f11>") 'eclim-run-class)
  ;; C-d for doc in browser, C-D for doc in emacs
  (local-set-key (kbd "C-c d") 'eclim-java-browse-documentation-at-point)
  (local-set-key
   (kbd "C-c D") 'eclim-java-show-documentation-for-current-element))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; display messages
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; ======================================================================
;; elisp
;; ======================================================================
(defun my-elisp-mode-hook()
  (flycheck-mode)
  (auto-fill-mode))
(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

;; ======================================================================
;; python
;; ======================================================================

;; anaconda-mode + keys: https://github.com/proofit404/anaconda-mode
(defun my-python-mode-hook()
  (flycheck-mode)
  (auto-fill-mode)
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; ======================================================================
;; Webstuff
;; ======================================================================
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(defun my-web-mode-hook()
  (setq web-mode-markup-indent-offset 3))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; ======================================================================
;; General (For when everything is done loading)
;; ======================================================================
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; Wait 30s before autocomplete
(setq company-idle-delay 30)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 30)
 '(global-company-mode t)
 '(package-selected-packages
   (quote
    (dired-sidebar coffee-mode atom-one-dark-theme company-c-headers company-emacs-eclim company-irony company-irony-c-headers company-jedi counsel counsel-gtags counsel-projectile doom-themes flx-ido flycheck flycheck-irony helm helm-gtags helm-projectile iedit irony irony-eldoc popwin powerline projectile smartparens swiper swiper-helm yasnippet-snippets ace-window chess company minimap)))
 '(window-manage-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "#5294e2" :foreground "#1B2229"))))
 '(company-echo-common ((t (:foreground "#FF6C6B"))))
 '(company-preview ((t (:foreground "#DCAEEA"))))
 '(company-preview-common ((t (:foreground "#DCAEEA"))))
 '(company-scrollbar-fg ((t (:background "#DCAEEA"))))
 '(company-tooltip-annotation ((t (:foreground "#ECBEFA"))))
 '(company-tooltip-common ((t (:foreground "#DCAEEA"))))
 '(company-tooltip-search ((t (:background "#5294e2" :foreground "#282c34"))))
 '(company-tooltip-selection ((t (:background "#5294e2" :foreground "#f4f5f3"))))
 '(cursor ((t (:background "#5294e2"))))
 '(custom-button-mouse ((t (:background "#5294e2" :foreground "#282c34" :box (:line-width 1 :style none)))))
 '(custom-button-pressed ((t (:background "#5294e2" :foreground "#282c34" :box (:line-width 1 :style none)))))
 '(dired-directory ((t (:foreground "#51afef" :weight bold))))
 '(eclim-problems-highlight-error-face ((t (:underline "#ff6c6b"))))
 '(eclim-problems-highlight-warning-face ((t (:underline "#ecd37a"))))
 '(helm-candidate-number ((t (:background "#dcaeea" :foreground "black"))))
 '(highlight ((t (:background "#5294e2" :foreground "#1B2229"))))
 '(widget-field ((t (:background "#3f444f")))))



;; cpplint (currently disabled)
;; (defun flymake-google-init()
;;   (require 'flymake-google-cpplint)
;;   (custom-set-variables
;;    '(flymake-google-cpplint-command "/usr/bin/cpplint"))
;;   (flymake-google-cpplint-load)
;;   )
;; (add-hook 'c-mode-hook 'flymake-google-init)
;; (add-hook 'c++-mode-hook 'flymake-google-init)

;; google-c-style
;; (require 'google-c-style)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; transparency stuff (meh)
;; (defun on-after-init ()
;;   (unless (display-graphic-p (selected-frame))
;;     (set-face-background 'default "unspecified-bg" (selected-frame))))
;; (add-hook 'window-setup-hook 'on-after-init)

;; ;; cedet intellisense (disabled in favor of irony-mode + company-mode)
;; (semantic-mode 1)
;; (global-semanticdb-minor-mode)
;; (defun col:add-semantic-to-autocomplete()
;;   (add-to-list 'ac-sources 'ac-source-semantic)
;;   )
;; (add-hook 'c-mode-common-hook 'col:add-semantic-to-autocomplete)
;; (global-ede-mode 1)

;; ;; project dependent cedet bit
;; ;; Template
;; ;; (ede-cpp-root-project "project_name" :file "path_to_file`"
;; ;; 		      :include-path '("path_to_dir_with_included_files"))

;; (ede-cpp-root-project "my project" :file "~/testing/my_program/src/main.cpp"
;; 		      :include-path '("/../my_inc")) 

;; ivy (disabled, kept swiper tho)
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "<f6>") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (global-set-key (kbd "C-c C-f") 'counsel-projectile)
;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; ;; irony-mode
;; (defun col:enable-irony()
;;   (irony-mode))
;; (add-hook 'c++-mode-hook 'col:enable-irony)
;; (add-hook 'c-mode-hook 'col:enable-irony)
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; flx-ido
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; ;; disable ido faces for highlights
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)

;; sr-speedbar
;; (require 'sr-speedbar)
;; (setq speedbar-use-images nil) ;; disable images
;; (setq sr-speedbar-right-side nil) ;; appear left

;; Helpful
;; (require 'helpful)
;; (global-set-key (kbd "C-h f") #'helpful-callable)
;; (global-set-key (kbd "C-h v") #'helpful-variable)
;; (global-set-key (kbd "C-h k") #'helpful-key)
;; (global-set-key (kbd "C-c C-.") #'helpful-at-point)

;;; .emacs ends here








