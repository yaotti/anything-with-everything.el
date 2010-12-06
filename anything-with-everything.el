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


(defvar awe:scripts-root-dir
  "~/.emacs.d/anything/scripts/"
  "The root directory containing scripts for anything plugins")

(defmacro add-anything-plugin (function-name script description source-name action-name)
  (let ((function-symbol (intern function-name))
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
         '((name . ,source-name)
           (init . (lambda ()
                     (unless (anything-candidate-buffer)
                       (with-current-buffer
                           (anything-candidate-buffer 'global)
                         (message "Building up candidates...")
                         (insert (shell-command-to-string
                                  (concat ,script-path " --list")))))))
           (candidates-in-buffer)
           (action . ((,action-name . ,action-symbol)))))
       (defun ,function-symbol ()
         ,description
         (interactive)
         (anything-other-buffer ,source-symbol
                                (format "*Anything script: %s" ,script))
         )
       (defun ,cache-clear-function-symbol ()
         ,cache-clear-function-description
         (interactive)
         (let* ((source-name (assoc-default 'name
                                           ,source-symbol))
                (candidate-buffer-name
                 (format " *anything candidates:%s*" source-name)))
           (message "Clear cache for anything source: %s" source-name)
           (kill-buffer candidate-buffer-name)))
       (message (concat "Defined functions for " ,script))
       )))

(provide 'anything-with-everything)