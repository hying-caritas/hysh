(defun %slime-stop-monitor-input ()
  (let* ((connection (slime-connection))
	 (process (slime-inferior-process connection))
	 (stop-monitor-input-str
	  "(require :asdf)(require :hysh)(hysh:stop-monitor-input)\n"))
    (with-current-buffer (process-buffer process)
      (goto-char (process-mark process))
      (insert-before-markers stop-monitor-input-str)
      (process-send-string process stop-monitor-input-str))))

(defun slime-stop-monitor-input (&optional arg)
  (interactive "P")
  (when (slime-connected-p)
    (%slime-stop-monitor-input)))

;; (defun slime-repl-mode-stop-monitor-input-hook ()
;;   (define-key slime-repl-mode-map (kbd "C-c d")
;;     'slime-stop-monitor-input))
;; (add-hook 'slime-repl-mode-hook 'slime-repl-mode-stop-monitor-input-hook)
