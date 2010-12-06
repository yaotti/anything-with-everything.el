;;; anything-with-everything.el --- Framework to enable you to write anything plugins in any language

;; Copyright (C) 2010  Hiroshige Umino

;; Author: Hiroshige Umino <yaotti@gmail.com>
;; Keywords: anything
;;
;;
;; Example:
;; Download this file: XXX: gist url
;; (require 'anything-with-everything)
;; (defun-anything-plugin "anything-hotentry" ;function name to register
;;                        "get-hotentries.pl" ;script name
;;                        "Get HatenaBookmark's hotentries" ;description
;;                        "Hot Entries"       ;source name
;;                        "View Entry"        ;action name (execute with --action=open)
;; )
;; and then `anything-hotentry' and `anything-hotentry-clear-cache' are defined so
;; you can execute them like M-x `anything-hotentry'
;; If you want to remove candidates cache, do M-x `anything-hotentry-clear-cache'

(require 'cl)


(defvar awe:scripts-root-dir
  "~/.emacs.d/anything/scripts/"
  "The root directory containing scripts for anything plugins")

(defmacro* add-anything-plugin (function-name script &optional description source-name action-name)
  (let ((function-symbol (intern function-name))
        (function-description
         (if description description
           (concat "Execute Anything script: " script)))
        (sname (if source-name source-name (concat "Source from " script)))
        (aname (if action-name action-name "Open"))
        (cache-clear-function-symbol
         (intern (concat function-name "-clear-cache")))
        (cache-clear-function-description
         (format "Function to clear candidate cache for %s" script)
         )
                (action-symbol (intern (concat "awe:action-for-" script)))
        (source-symbol (intern (concat "awe:source-for-" script)))
        (script-path (concat awe:scripts-root-dir script)))
    `(progn
       (defun ,action-symbol (candidate)
         (let ((quoted-candidate (format "\"%s\"" candidate)))
           (shell-command-to-string (concat ,script-path " --action=open " quoted-candidate))))
       (defvar ,source-symbol
         '((name . ,sname)
           (init . (lambda ()
                     (unless (anything-candidate-buffer)
                       (with-current-buffer
                           (anything-candidate-buffer 'global)
                         (message "Building up candidates...")
                         (insert (shell-command-to-string
                                  (concat ,script-path " --list")))))))
           (candidates-in-buffer)
           (action . ((,aname . ,action-symbol)))))
       (defun ,function-symbol ()
         ,description
         (interactive)
         (anything-other-buffer ,source-symbol
                                (format "*Anything script: %s" ,script))
         )
       (defun ,cache-clear-function-symbol ()
         ,cache-clear-function-description
         (interactive)
         (let ((candidate-buffer-name
                (format " *anything candidates:%s*" ,sname)))
           (message "Clear cache for anything source: %s" ,sname)
           (kill-buffer candidate-buffer-name)))
       (message (concat "Defined functions for " ,script))
       )))

(provide 'anything-with-everything)


;; (add-anything-plugin "function-name"
;;                      "script.pl"
;;                      "description"
;;                      '(("option-value1" . "Source1 Name") ; --action=option-value1が実行される
;;                        ("option-value2" . "Source2 Name")
;;                        )
;;                      '(("action1-option" . "Action1 Name"))
;;                      )
;; ;; Simple
;; (add-anything-plugin "function-name"
;;                      "script.pl"
;;                      "description"
;;                      "Source1 Name"     ; script --listを実行
;;                      "Action1 Name"     ; script --openを実行
;;                      )

;; ;; Simplest
;; (add-anything-plugin "anything-hotentry-simple" "get-hotentries.pl")
;; (anything-hotentry-simple)
;; (anything-hotentry-simple-clear-cache)
