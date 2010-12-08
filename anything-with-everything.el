;;; anything-with-everything.el --- Framework to enable you to write anything functions in any language

;; Copyright (C) 2010  Hiroshige Umino

;; Author: Hiroshige Umino <yaotti@gmail.com>
;; Keywords: anything
;;
;;
;; You can write a program in which language you like and you can use it
;; with anything interface.
;;
;; * Simplest usage: 
;; (require 'anything-with-everything)
;; (add-anything-plugin "anything-my-cool-process" "my-cool-script.pl")
;; With this you can execute M-x `anything-my-cool-process'
;;
;; * Requirement for your script:
;; Your script have to accept these two command line flag:
;; --init, --action
;;  $ yourscript --init=list # is executed to build up anything candidates.
;; You have to print candidates for anything.el. One candidate per line.
;;  $ yourscript --action=open # is required to execute anything action.
;; With selecting a candidate, anything-with-everything.el executes
;;  $ yourscript --action=open SELCTED_CANDIDATE
;;
;; Example:
;; Download this file into ~/.emacs.d/anything/scripts/get-emacswiki-changes.pl
;;   https://gist.github.com/raw/732759/5e210890a98fc9b2de7463f8b3ec4c7d7d1b9d01/get-emacswiki-changes.pl
;; Try to execute this like
;;  $ perl get_emacswiki-changes.pl --init=list
;; If this works well, eval these elisp:
;;
;; (require 'anything-with-everything)
;; (defun-anything-function-with-script "anything-emacswiki-changes" "get-emacswiki-changes.pl")
;; then `anything-emacswiki-changes' is defined so you can execute this like
;; M-x `anything-emacswiki-changes'
;; If you want to remove candidates cache buffer, do C-c C-u in anything buffer.
;;
;;
;; Advanced usage:
;; You can assign multiple anything sources like
;; (defun-anything-function-with-script "anything-with-multisource" "yourscript.pl"
;;   (("script-option1" . "First source") ("script-option2" . "Second Source")))
;;
;; This means you get candiates with
;;  $ yourscript --init=script-optin1 # for first source
;;  $ yourscript --init=script-optin2 # for second source
;;
;; An example for this usage is as follows:
;; Download this file into ~/.emacs.d/anything/scripts/get-hb-entries.pl
;;   https://gist.github.com/raw/731856/cf186c3da3d463aaec35c9d130fa79173780e79e/get-hb-entries.pl
;; and eval this
;; (defun-anything-function-with-script "anything-hb-entries" "get-hb-entries.pl"
;;   (("list" . "Hot Entries") ("my_entries" . "My Entries")))
;; And execute M-x `anything-hateb-entries'.
;; You can get candidates from multiple anything source.

;;
;; TODO: Support multiple actions
;;

(defvar awe:scripts-root-dir
  "~/.emacs.d/anything/scripts/"
  "The root directory containing scripts for anything plugins")

(defun awe:defvar-source (elt script action-symbol)
  "defvar source with ((option source-name symbol-name) script action-symbol)"
  `(defvar ,(intern (caddr elt))
     '((name . ,(cadr elt))
       (init . (lambda ()
                 (unless (anything-candidate-buffer)
                   (with-current-buffer
                       (anything-candidate-buffer 'global)
                     (message "Building up candidates...")
                     (insert (shell-command-to-string
                              (concat ,script " --init=" ,(car elt))))))))
       (candidates-in-buffer)
       (action . (("Open" . ,action-symbol)))))
  )


(defmacro* defun-anything-function-with-script (function-name script &optional sources)
  (let* ((script-path (concat awe:scripts-root-dir script))
        (script-sources (if sources sources `(("list" . ,script-path)))))
    (let ((function-symbol (intern function-name))
          (function-description
           (concat "Execute Anything script: " script-path))
          (anything-script-buffer-name
           (format "*Anything script: %s*" script-path))
          (action-symbol (intern (concat "awe:action-for-" script-path)))
          (source-option-name-symbols
           (mapcar (lambda (pair)
                     (list
                      (car pair)
                      (cdr pair)
                      (format "anything-c-source-for-%s-in-%s"
                              (car pair) script)))
                   script-sources
                   )))
         `(progn
       ;; define action
       (defun ,action-symbol (candidate)
         (let ((quoted-candidate (format "\"%s\"" candidate)))
           (shell-command-to-string (concat ,script-path " --action=open " quoted-candidate))))

       ;; define anything function
       (defun ,function-symbol ()
         ,function-description
         (interactive)
         (anything-other-buffer ',(mapcar (lambda (elt) (intern (caddr elt))) source-option-name-symbols)
                                ,anything-script-buffer-name)
         )
       ;; defvar source(s)
       ,@(mapcar
          (lambda (elt) (awe:defvar-source elt script-path action-symbol))
          source-option-name-symbols)
    ))))

(provide 'anything-with-everything)
