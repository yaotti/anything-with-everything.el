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

(eval-when-compile (require 'cl))


(defvar awe:scripts-root-dir
  "~/.emacs.d/anything/scripts/"
  "The root directory containing scripts for anything plugins")

(defmacro* add-anything-plugin (function-name script &optional description sources action-name)
  (let* ((function-symbol (intern function-name))
         (function-description
          (if description description
            (concat "Execute Anything script: " script)))
         ;; (sname (if source-name source-name (concat "Source from " script)))
         (aname (if action-name action-name "Open"))
         (cache-clear-function-symbol
          (intern (concat function-name "-clear-cache")))
         (cache-clear-function-description
          (format "Function to clear candidate cache for %s" script)
          )
         (action-symbol (intern (concat "awe:action-for-" script)))
         ;;(sources (if (stringp sources) '(("list" . sources)) sources))
         ;;(source-symbol (intern (concat "awe:source-for-" script)))
         (source-symbols (mapcar (lambda (pair)
                                   ;; XXX
                                   (message
                                    (format "-----awe:source-for-%s-in-%s"
                                            (car pair)
                                            script))
                                   ;; XXX end
                                   (intern
                                    (format "awe:source-for-%s-in-%s"
                                            (car pair)
                                            script)))
                                 (cadr sources)))
         (script-path (concat awe:scripts-root-dir script)))
    `(progn
       ;; define action
       (defun ,action-symbol (candidate)
         (let ((quoted-candidate (format "\"%s\"" candidate)))
           (shell-command-to-string (concat ,script-path " --action=open " quoted-candidate))))
       ;; define source
       ;; XXX
       ;; ,(message (format "sources length: %d" (length sources)))
       ;; ,(message (format "first source: %s" (cadr sources)))
       ;; XXX end
       (mapcar
        (lambda (pair)
          (message (car pair))
          (defvar ,(intern (format "awe:source-for-%s-in-%s"
                                   (car pair)
                                   script))
            '((name . ,(cdr pair))
              (init . (lambda ()
                        (unless (anything-candidate-buffer)
                          (with-current-buffer
                              (anything-candidate-buffer 'global)
                            (message "Building up candidates...")
                            (insert (shell-command-to-string
                                     (concat ,script-path " --init=" (car pair))))))))
              (candidates-in-buffer)
              (action . ((,aname . ,action-symbol))))))
        ,'(cadr sources))
       ;; define anything function
       (defun ,function-symbol ()
         ,description
         (interactive)
         (anything-other-buffer '(,source-symbols)
                                (format "*Anything script: %s" ,script))
         )
       ;; (defun ,cache-clear-function-symbol ()
       ;;   ,cache-clear-function-description
       ;;   (interactive)
       ;;   (let ((candidate-buffer-name
       ;;          (format " *anything candidates:%s*" ,sname)))
       ;;     (message "Clear cache for anything source: %s" ,sname)
       ;;     (kill-buffer candidate-buffer-name)))
       (message (concat "Defined functions for " ,script))
       )))

;; Simple
(add-anything-plugin "anything-hotentry-1" "get-hotentries.pl"
                     "Description"
                     '(("list" . "Hot Entries")
                       ("my_entries" . "My Entries")
                       ("list" . "Hot Entries")))
(anything-hotentry-1)
(anything-hotentry-1-clear-cache)



;; (add-anything-plugin "function-name"
;;                      "script.pl"
;;                      "description"
;;                      '(("option-value1" . "Source1 Name") ; script --init=option-value1が実行される
;;                        ("option-value2" . "Source2 Name")
;;                        )
;;                      '(("action1-option" . "Action1 Name"))
;;                      )
;; ;; Simple
;; (add-anything-plugin "function-name"
;;                      "script.pl"
;;                      "description"
;;                      "Source1 Name"     ; script --init=listを実行
;;                      "Action1 Name"     ; script --action=openを実行
;;                      )

;; ;; Simplest
;; (add-anything-plugin "anything-hotentry-1" "get-hotentries.pl")
;; (anything-hotentry-1)
;; (anything-hotentry-1-clear-cache)

;; ;; Simplest
;; (add-anything-plugin "anything-hotentry-1" "get-hotentries.pl")
;; (anything-hotentry-1)
;; (anything-hotentry-1-clear-cache)


(provide 'anything-with-everything)