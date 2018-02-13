(require 'package)
(package-initialize)

;; ======================================================================
;; Testing
;; ======================================================================

;; Enable stacktrace on elisp error:
(setq debug-on-error t)

;; c-tools yo
(add-to-list 'load-path "~/.emacs.d/elpa/c-tools")
(require 'c-tools)

(defun dir-to-kill-ring()
  (interactive)
  (kill-new (buffer-file-name)))

;; ======================================================================
;; General
;; ======================================================================
(require 'flycheck)

;; ===== MELPA =====
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; ===== Doom themes =====
(require 'doom-themes)
;; Enable bolding and italics with doom themes
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
;; Load theme
(load-theme 'doom-one t)
;; (load-theme 'doom-vibrant t)
;; ^ see page for other details

;; ===== Font ======
(set-face-attribute 'default nil
		    :family "Inconsolata"
		    :height 130
		    :weight 'bold)

(set-face-attribute 'variable-pitch nil
		    :family "Inconsolata"
		    :height 130
		    :weight 'bold)


;; ===== Good Options =====
;; (menu-bar-mode -1) ;; disable menu bar
(tool-bar-mode -1) ;; disable tool bar
(scroll-bar-mode -1) ;; disable scroll bar
(global-linum-mode t) ;; line nums
(global-hl-line-mode) ;; highlight current line
(setq select-enable-clipboard t) ;; good clipboard stoof
(setq inhibit-splash-screen t) ;; disable the welcome screen
(setq c-basic-offset 3) ;; C indentation size of 3

;; small compilation window
(setq split-height-threshold 0)
(setq compilation-window-height 10)

;; ===== bind keys =====
(global-set-key (kbd "C-j") 'newline-and-indent) ;; equal to RET
(global-set-key (kbd "C-<f11>") 'compile) ;; C-f11 = compile
(global-set-key (kbd "C-\'") 'company-complete) ;; auto-complete
(global-set-key (kbd "C-;") 'iedit-mode) ;; toggle iedit
;; (global-set-key (kbd "M-.") 'helm-gtags-dwim) ;; helm gtags jump to def
(global-set-key (kbd "M-n") 'next-block-soe)
(global-set-key (kbd "M-p") 'previous-block-soe)
(global-set-key (kbd "M-DEL") 'backward-delete-word)
;; replace suspend-frame with save-buffers-kill-terminal
;; (originally C-x C-c)
(global-set-key (kbd "C-x C-z") 'save-buffers-kill-terminal)

;; ===== unbind keys =====
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-z")) ;; disable minimize
(global-unset-key (kbd "C-r")) ;; disable reverse isearch (using swiper)

;; My current emacs C source dir
(setq source-directory "~/downloads/emacs-25.3/src/")

;; ===== Semantic (Global) =====
(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

;; ===== Company (Global) =====
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends
		'(company-c-headers company-semantic)))
;; Company keybinds
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; ===== Yasnippet (Global) =====
(require 'yasnippet)
(yas-global-mode 1)

;; ===== Smartparens =====
(require 'smartparens-config)
;; (smartparens-global-mode)
(show-paren-mode 1)

;; Help window changes:
;; Shrink help buffers to 20% size
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
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-c C-f") #'helm-projectile)
(global-set-key (kbd "M-y") #'helm-show-kill-ring) ;; better yank cycle
(global-set-key (kbd "C-c r") #'helm-recentf)      ;; recently opened files
(helm-mode 1)

;; moe-theme helm colors
(set-face-attribute 'helm-selection nil
		    ;; :background "#51afef"
		    :background "#5294E2"
		    :foreground "#d5d4d3")

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

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;;; Helm related keybinds
(global-set-key (kbd "C-x b") 'helm-mini)  ;; use helm minibuffer
(global-set-key (kbd "C-s") 'swiper-helm)  ;; swiper!

;; autoresize mode
(helm-autoresize-mode 1)
;; define size (part of autoresize mode)
(setq helm-autoresize-max-height 25)
(setq helm-autoresize-min-height 25)

;; helm window in current buffer
;; (setq helm-split-window-in-side-p t)

;; ======================================================================
;; C/C++
;; ======================================================================
;; C style!
;; (setq c-default-style "linux") ;; linux default style

;; autofill
(add-hook 'c++-mode-hook 'auto-fill-mode)
(add-hook 'c-mode-hook 'auto-fill-mode)

;; flycheck
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;; for c++ headers
(add-hook 'c++-mode-hook
	  (lambda ()
	    (interactive)
	    (setq flycheck-clang-include-path
		  (list "/usr/include/X11" "/usr/include/GL"
			"usr/include/c++/7.3.0/"))
	    (setq flycheck-gcc-include-path
		  (list "/usr/include/X11" "/usr/include/GL"
			"usr/include/c++/7.3.0/"))

	    (flycheck-select-checker 'c/c++-gcc)))

(add-hook 'c-mode-hook
	  (lambda () (interactive) (flycheck-select-checker 'c/c++-gcc)))
	    

;; ======================================================================
;; Java (eclim, disabled for now)
;; ======================================================================
;; (require 'eclim)
;; (setq eclimd-autostart t)

;; ;; company-eclim
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)

;; (add-hook 'java-mode-hook '(lambda () (interactive) (eclim-mode t)))

;; ;; display messages
;; (setq help-at-pt-display-when-idle t)
;; (setq help-at-pt-timer-delay 0.1)
;; (help-at-pt-set-timer)

;; ======================================================================
;; elisp
;; ======================================================================

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-fill-mode)
;; (add-hook 'emacs-lisp-mode-hook 'iedit-mode)      ;; causes startup error

;; ======================================================================
;; python
;; ======================================================================
;; (elpy-enable)
(add-hook 'python-mode-hook 'flycheck-mode)

;; ======================================================================
;; Webstuff
;; ======================================================================
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

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
 '(package-selected-packages
   (quote
    (minimap company-c-headers company-emacs-eclim company-irony company-irony-c-headers counsel counsel-gtags counsel-projectile flx-ido flycheck flycheck-irony helm helm-gtags helm-projectile irony popwin powerline projectile swiper swiper-helm smartparens moe-theme iedit helm-projectile doom-themes ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; ;; autocomplete (using company-mode now)
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; ;; ac header locations
;; (defun col:init-ac-c-header()
;;   (require 'auto-complete-c-headers)
;;   (add-to-list 'ac-sources 'ac-source-c-headers)
;;   (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1/../../../../include/c++/6.3.1")
;;   (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1/../../../../include/c++/6.3.1/x86_64-pc-linux-gnu")
;;   (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1/../../../../include/c++/6.3.1/backward")
;;   (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1/include")
;;   (add-to-list 'achead:include-directories '"/usr/local/include")
;;   (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1/include-fixed")
;;   (add-to-list 'achead:include-directories '"/include")
;; )
;; (add-hook 'c++-mode-hook 'col:init-ac-c-header)
;; (add-hook 'c-mode-hook 'col:init-ac-c-header)

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





