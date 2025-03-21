;;; ellellemm.el --- Use LLM models within Emacs.

;; Copyright (C) 2024 Deepankar Sharma

;; Author: Deepankar Sharma <deepankarsharma@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (plz "3.18.01"))
;; Keywords: llm, productivity
;; URL: https://github.com/deepankarsharma/ellellemm

;;; Commentary:

;; Allow idiomatic use of LLMs while using Emacs.

;;; Code:

(require 'cl)
(require 'plz)
(require 'ediff)

(defvar *ellellemm-model* "claude-3-5-sonnet-20240620"
  "The currently active model for ellellemm queries.")

(defvar *ellellemm-debug-mode* nil
  "When non-nil, enable debug output for ellellemm operations.")

(defgroup *ellellemm-group* nil
  "Group to customize ellellemm.")

(defvar *ellellemm-questions* '()
  "List of questions I have asked ellellemm so far.")

(defvar *ellellemm-regions* '()
  "List of stored regions in the form (START END OVERLAY).")

(when (and '*ellellemm-questions*
           (not (member *ellellemm-questions* savehist-additional-variables)))
  (push '*ellellemm-questions* savehist-additional-variables))

;; Supported models
;; Provider Name,  Model name
;; groq, llama-3.1-70b-versatile
;; groq, llama-3.1-8b-instant
;; groq, mixtral-8x7b-32768
;; claude, claude-3-7-sonnet-latest
;; claude, claude-3-5-haiku-latest
;; gemini, gemini-2.0-flash
;; gemini, gemini-2.0-pro-exp-02-05


;; Selection and region related functions.
(defun ellellemm-add-region ()
  "Add the current region to `ellellemm-regions` and highlight it."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'face 'highlight)
          (push (list start end overlay) ellellemm-regions))
        (message "Region added!"))
    (message "No active region to add.")))

(defun ellellemm-highlight-regions ()
  "Highlight all stored regions in `ellellemm-regions`."
  (interactive)
  (dolist (entry ellellemm-regions)
    (let ((start (nth 0 entry))
          (end (nth 1 entry)))
      (let ((overlay (make-overlay start end)))
        (overlay-put overlay 'face 'highlight)
        (setf (nth 2 entry) overlay))))
  (message "All stored regions highlighted."))

(defun ellellemm-hide-highlights ()
  "Remove highlight from all stored regions."
  (interactive)
  (dolist (entry ellellemm-regions)
    (when (nth 2 entry) ;; Check if overlay exists
      (delete-overlay (nth 2 entry))
      (setf (nth 2 entry) nil)))
  (message "All highlights removed."))

(defun ellellemm-clear-regions ()
  "Clear all stored regions and remove overlays."
  (interactive)
  (ellellemm-hide-highlights)
  (setq ellellemm-regions '())
  (message "Cleared all stored regions."))

;; Keybindings using `C-c e` prefix for easy recall
(global-set-key (kbd "C-c e a") 'ellellemm-add-region)      ;; (A)dd region
(global-set-key (kbd "C-c e h") 'ellellemm-highlight-regions) ;; (H)ighlight stored regions
(global-set-key (kbd "C-c e u") 'ellellemm-hide-highlights)   ;; (U)nhighlight regions
(global-set-key (kbd "C-c e c") 'ellellemm-clear-regions)    ;; (C)lear all region



;; Utility functions
(defun get-anthropic-api-key ()
  "Retrieve the Anthropic API key from the ANTHROPIC_API_KEY environment variable."
  (or (getenv "ANTHROPIC_API_KEY")
      (error "ANTHROPIC_API_KEY environment variable is not set")))

(defun get-groq-api-key ()
  "Retrieve the Groq API key from the GROQ_API_KEY environment variable."
  (or (getenv "GROQ_API_KEY")
      (error "GROQ_API_KEY environment variable is not set")))

(defun get-or-create-ellellemm-buffer ()
  "Return the *ellellemm-buffer*, creating it if it doesn't exist."
  (let ((buffer-name "*ellellemm-buffer*"))
    (or (get-buffer buffer-name)
        (let
            ((buffer (generate-new-buffer buffer-name)))
          (with-current-buffer "*ellellemm-buffer*"
            (markdown-mode))
          buffer))))

(defun insert-line-separator ()
  "Insert a visible line separator followed by two newlines."
  (insert "----------------\n\n"))

(defun make-buffer-writable (buffer)
  "Make BUFFER writable, handling potential errors."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (setq buffer-read-only nil)
      (when (fboundp 'read-only-mode)
        (read-only-mode -1)))))

(defun get-surrounding-lines (n)
  "Get N lines before and after the current line from the current buffer."
  (let* ((current-line (line-number-at-pos))
         (total-lines (count-lines (point-min) (point-max)))
         (start-line (max 1 (- current-line n)))
         (end-line (min total-lines (+ current-line n)))
         (start-pos (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- start-line))
                      (point)))
         (end-pos (save-excursion
                    (goto-char (point-min))
                    (forward-line end-line)
                    (point))))
    (buffer-substring-no-properties start-pos end-pos)))

(defmacro with-buffer-read-only (&rest body)
  "Execute BODY while the buffer is read-only.
Temporarily disables user editing of the buffer."
  `(let ((inhibit-read-only t)
         (old-modified (buffer-modified-p)))
     (unwind-protect
         (progn
           (setq buffer-read-only t)
           ,@body)
       (setq buffer-read-only nil)
       (set-buffer-modified-p old-modified))))

(defun ellellemm-provider-from-model (model)
  "Deduce the provider (groq or claude) from the MODEL name."
  (cond
   ((string-prefix-p "claude-" model) "claude")
   ((string-prefix-p "gemini-" model) "gemini")
   ((or (string-prefix-p "llama-" model)
        (string-prefix-p "mixtral-" model))
    "groq")
   (t (error "Unknown model provider for %s" model))))

(defun ellellemm-provider-fn (model)
  "Return the appropriate provider function for the given MODEL."
  (let ((provider (ellellemm-provider-from-model model)))
    (pcase provider
      ("claude" #'claude-external-process)
      ("gemini" #'gemini-external-process)
      ("groq" #'groq-external-process)
      (_ (error "Unknown provider for model %s" model)))))


(defun ellellemm-buffer-jump-to (position)
  "Jump to POSITION in ellellemm buffer."
  (let* ((buffer (get-or-create-ellellemm-buffer))
        (window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (goto-char position))
    (if (windowp window)
        (select-window window)
      (switch-to-buffer-other-window buffer))))
      
(defun ellellemm-buffer-jump-to (position)
  "Jump to POSITION in ellellemm buffer and ensure it's visible."
  (let* ((buffer (get-or-create-ellellemm-buffer))
         (window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (goto-char position)
      (unless (pos-visible-in-window-p position)
        (recenter)))
    (if (windowp window)
        (select-window window)
      (switch-to-buffer-other-window buffer))
    (with-selected-window (get-buffer-window buffer)
      (unless (pos-visible-in-window-p position)
        (recenter)))))


(defun ellellemm-buffer-jump-to (position)
  "Jump to POSITION in ellellemm buffer and ensure it's visible."
  (message "position: %d" position)
  (let* ((buffer (get-or-create-ellellemm-buffer))
         (window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (goto-char position))
    (if (windowp window)
        (progn
          (select-window window)
          (set-window-point window position)
          (unless (pos-visible-in-window-p position window)
            (recenter)))
      (switch-to-buffer-other-window buffer))
    (with-current-buffer buffer
      (goto-char position))
    (when (get-buffer-window buffer)
      (set-window-point (get-buffer-window buffer) position)
      (unless (pos-visible-in-window-p position (get-buffer-window buffer))
        (with-selected-window (get-buffer-window buffer)
          (recenter))))))


(defun ellellemm-buffer-jump-to (position)
  "Jump to POSITION in ellellemm buffer and ensure it's visible."
  (let ((buffer (get-or-create-ellellemm-buffer)))
    (pop-to-buffer buffer)
    (goto-char position)
    (unless (pos-visible-in-window-p)
      (recenter))))

;; ****************************************************************
;; Calls to LLM providers
;; ****************************************************************
(defun groq-external-process (prompt buffer model jump-to-point &optional finalizer-function)
  "Stream Groq's response to PROMPT using MODEL and insert it into BUFFER using external processes.
If FINALIZER-FUNCTION is provided, it will be called when the process is finished."
  (let* ((url "https://api.groq.com/openai/v1/chat/completions")
         (api-key (get-groq-api-key))
         (json-payload `(("model" . ,model)
                         ("messages" . [,(list (cons "role" "user")
                                               (cons "content" prompt))])
                         ("max_tokens" . 1024)
                         ("stream" . t)))
         (temp-file (make-temp-file "groq-request-" nil ".json"))
         (curl-and-jq-command (format "curl -s -N -X POST %s \
-H 'Authorization: Bearer %s' \
-H 'Content-Type: application/json' \
-d @%s \
| grep '^data:' \
| sed -u 's/^data: //g' \
| grep '^{' \
| jq -j 'select(.choices != null) | .choices[0].delta.content // empty'"
                                      url api-key temp-file)))
    (with-current-buffer buffer
      (when *ellellemm-debug-mode*
        (insert "Debug: Groq curl command:\n")
        (insert (format "%s\n\n" (replace-regexp-in-string api-key "$GROQ_API_KEY" curl-and-jq-command)))))
    (with-temp-file temp-file
      (insert (json-encode json-payload)))
    (make-process
     :name "groq-stream"
     :buffer buffer
     :command (list "bash" "-c" curl-and-jq-command)
     :filter (lambda (proc string)
               (when (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc)
                   (with-buffer-read-only
                    (goto-char (point-max))
                    (insert string)))))
     :sentinel (lexical-let ((finalizer-function finalizer-function)
                             (jump-to-point jump-to-point))
                            (lambda (proc event)
                              (when (string= event "finished\n")
                                (message "Groq's response complete.")
                                (ellellemm-buffer-jump-to jump-to-point)
                                (when finalizer-function
                                  (funcall finalizer-function))))))))

(defun claude-external-process (prompt buffer model jump-to-point &optional finalizer-function)
  "Stream Claude's response to PROMPT using MODEL and insert it into BUFFER using external processes.
If FINALIZER-FUNCTION is provided, it will be called when the process is finished."
  (let* ((url "https://api.anthropic.com/v1/messages")
         (api-key (get-anthropic-api-key))
         (json-payload `(("model" . ,model)
                         ("messages" . [,(list (cons "role" "user")
                                               (cons "content" prompt))])
                         ("max_tokens" . 1024)
                         ("stream" . t)))
         (temp-file (make-temp-file "claude-request-" nil ".json"))
         (curl-and-jq-command (format "curl -s -N -X POST %s \
-H 'anthropic-version: 2023-06-01' \
-H 'content-type: application/json' \
-H 'x-api-key: %s' \
-d @%s \
| grep '^data:' \
| sed -u 's/^data: //g' \
| grep '^{' \
| jq -j 'select(.type == \"content_block_delta\") | .delta.text // empty'"
                                      url api-key temp-file)))
    (with-current-buffer buffer
      (when *ellellemm-debug-mode*
        (insert "Debug: Claude curl command:\n")
        (insert (format "%s\n\n" (replace-regexp-in-string api-key "$ANTHROPIC_API_KEY" curl-and-jq-command)))))
    (with-temp-file temp-file
      (insert (json-encode json-payload)))
    (make-process
     :name "claude-stream"
     :buffer buffer
     :command (list "bash" "-c" curl-and-jq-command)
     :filter (lambda (proc string)
               (when (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc)
                   (with-buffer-read-only
                    (goto-char (point-max))
                    (insert string)))))
     :sentinel (lexical-let ((finalizer-function finalizer-function)
                             (jump-to-point jump-to-point))
                            (lambda (proc event)
                              (when (string= event "finished\n")
                                (message "Claude's response complete.")
                                (ellellemm-buffer-jump-to jump-to-point)
                                (when finalizer-function
                                  (funcall finalizer-function))))))))

(defun gemini-external-process (prompt buffer model jump-to-point &optional finalizer-function)
  "Stream Google Gemini's response to PROMPT using MODEL and put in BUFFER.
If FINALIZER-FUNCTION is provided, it will be called when the process is finished."
  (let* ((url (format "https://generativelanguage.googleapis.com/v1beta/models/%s:streamGenerateContent?alt=sse&key=%s" model (get-google-gemini-api-key)))
         (json-payload `(("contents" . [,(list (cons "role" "user")
                                                (cons "parts" (list (cons "text" prompt))))]))) ;  Added stopSequences for completeness, even if empty.
         (temp-file (make-temp-file "gemini-request-" nil ".json"))
         (curl-command (format "curl -s -N -X POST \"%s\" \
-H 'Content-Type: application/json' \
-d @%s \
| grep '^data:' \
| sed -u 's/^data: //g'"
                               url temp-file))
         (jq-command "jq -r --raw-output '.candidates[0].content.parts[0].text // empty'")
         (curl-and-jq-command (format "%s | %s" curl-command jq-command)))
    (with-current-buffer buffer
      (when *ellellemm-debug-mode*
        (insert "Debug: Gemini curl command:\n")
        (insert (format "%s\n\n" (replace-regexp-in-string (get-google-gemini-api-key) "$GOOGLE_GEMINI_API_KEY" curl-and-jq-command)))
        (insert curl-and-jq-command)))
    (with-temp-file temp-file
      (insert (json-encode json-payload)))
    (make-process
     :name "gemini-stream"
     :buffer buffer
     :command (list "bash" "-c" curl-and-jq-command)
     :filter (lambda (proc string)
                (when (buffer-live-p (process-buffer proc))
                  (with-current-buffer (process-buffer proc)
                    (with-buffer-read-only
                      (goto-char (point-max))
                      (insert string)))))
     :sentinel (lexical-let ((finalizer-function finalizer-function)
                              (jump-to-point jump-to-point))
                 (lambda (proc event)
                   (when (string= event "finished\n")
                     (message "Gemini's response complete.")
                     (ellellemm-buffer-jump-to jump-to-point)
                     (when finalizer-function
                       (funcall finalizer-function))))))))

(defun get-google-gemini-api-key ()
  "Retrieves the Google Gemini API key from Emacs's configuration."
   (let ((api-key (getenv "GOOGLE_GEMINI_API_KEY")))
     (unless api-key
          (error "Google Gemini API key not found.  Set the GOOGLE_GEMINI_API_KEY environment variable"))
     api-key))

;; ****************************************************************
;; ************* Prompts *****************
;; ****************************************************************

(defun generate-single-question-prompt (question)
  "Generate a prompt for Claude using the QUESTION, current region and surrounding context."
  (format "Answer the following technical question. Provide answer in markdown mode. %s" question))

(defun generate-code-explanation-prompt ()
  "Generate a prompt for Claude to explain the code in the selected region."
  (if (use-region-p)
      (let* ((region-start (region-beginning))
             (region-end (region-end))
             (selected-code (buffer-substring-no-properties region-start region-end))
             (buffer-name (buffer-name))
             (major-mode-name (symbol-name major-mode))
             (surrounding-context (get-surrounding-lines 200)))
        (format "Please explain the following code snippet.

Context information:
- Buffer name: %s
- Buffer mode: %s
- The code is part of a larger file. Here's the surrounding context (200 lines before and after):

```
%s
```

Now, please explain this specific code snippet:

```
%s
```

Provide a detailed explanation of what this code does, its purpose, and any important concepts or patterns it demonstrates. If you notice any potential issues or improvements, please mention those as well. Produce markdown output"
                buffer-name
                major-mode-name
                surrounding-context
                selected-code))
    (error "No region selected.  Please select a region of code to explain")))

(defun generate-code-question-prompt (code question)
  "Generate a prompt for Claude to answer a QUESTION about the given CODE."
  (let* ((buffer-name (buffer-name))
         (major-mode-name (symbol-name major-mode)))
    (format "Please answer the following question about this code snippet.

Context information:
- Buffer name: %s
- Buffer mode: %s

Code snippet:

```
%s
```

Question: %s

Please provide a detailed answer to the question, explaining any relevant concepts or patterns in the code. If additional context is needed to fully answer the question, please mention that. Produce markdown output."
            buffer-name
            major-mode-name
            code
            question)))

(defun generate-patch-prompt (buffer-name buffer-contents buffer-mode instructions)
  "Generate a prompt for creating a patch based on the given buffer information and instructions."
  (format "Please create a patch for the following buffer:

Buffer Name: %s
Buffer Mode: %s

Current Buffer Contents:
```
%s
```

Please provide a unified diff patch that implements the following instructions:
%s

Please provide the patch in the standard unified diff format, starting with '--- a/' and '+++ b/' lines. Do not include any explanations or comments outside the patch itself."
          buffer-name
          buffer-mode
          buffer-contents
          instructions))





;; ****************************************************************
;; *-model handler functions
;; ****************************************************************

(defun ellellemm-ask-model (question model)
  "Ask QUESTION to the specified MODEL."
  (let* ((prompt (generate-single-question-prompt question))
         (buffer (get-or-create-ellellemm-buffer))
         (provider (ellellemm-provider-from-model model))
         (provider-fn (ellellemm-provider-fn model)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n\n\n\n")
      (insert-line-separator)
      (let
          ((current-position (point-max)))
        (insert (format "# Question: %s" question))
        (newline 2)
        (insert (format "%s's response (%s):\n\n" provider model))
        (funcall provider-fn prompt buffer model current-position)))))

(defun ellellemm-explain-region-model (model)
  "Explain the code in the selected region using the specified MODEL."
  (let* ((prompt (generate-code-explanation-prompt))
         (buffer (get-or-create-ellellemm-buffer))
         (provider (ellellemm-provider-from-model model))
         (provider-fn (ellellemm-provider-fn model)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (newline 4)
      (insert-line-separator)
      (let
          ((current-position (point-max)))
        (insert "# Code Explanation")
        (newline 2)
        (insert (format "%s's explanation (%s):\n\n" provider model))
        (funcall provider-fn prompt buffer model current-position)))))

(defun ellellemm-ask-about-region-model (question model)
  "Ask a QUESTION about the code in the selected region using MODEL."
  (if (use-region-p)
      (let* ((region-start (region-beginning))
             (region-end (region-end))
             (selected-code (buffer-substring-no-properties region-start region-end))
             (prompt (generate-code-question-prompt selected-code question))
             (buffer (get-or-create-ellellemm-buffer))
             (provider (ellellemm-provider-from-model *ellellemm-model*))
             (provider-fn (ellellemm-provider-fn *ellellemm-model*)))
        (with-current-buffer buffer
          (goto-char (point-max))
          (newline 4)
          (insert-line-separator)
          (let
              ((current-position (point-max)))
            (insert "# Question about Code\n\n")
            (insert (format "Question: %s\n\n" question))
            (insert (format "%s's response (%s):\n\n" provider model))
            (funcall provider-fn prompt buffer model current-position))))
    (error "No region selected.  Please select a region of code to ask about")))

(defun ellellemm-generate-patch-model (instructions model)
  "Generate a patch for the current buffer based on INSTRUCTIONS using the specified MODEL, then apply it using ediff."
  (let* ((buffer-name (buffer-name))
         (buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
         (buffer-mode (symbol-name major-mode))
         (prompt (generate-patch-prompt buffer-name buffer-contents buffer-mode instructions))
         (output-buffer (get-buffer-create "*ellellemm-patch*"))
         (provider (ellellemm-provider-from-model model))
         (provider-fn (ellellemm-provider-fn model)))
    (with-current-buffer output-buffer
      (erase-buffer))
    
    ;; Generate the patch
    (funcall provider-fn prompt output-buffer model
             (lexical-let ((prompt prompt)
                           (output-buffer output-buffer)
                           (model model))
               (lambda ()
                 (with-current-buffer output-buffer
                   (goto-char (point-min))
                   (if (re-search-forward "^--- a/" nil t)
                       (let ((patch-content (buffer-substring-no-properties (match-beginning 0) (point-max))))
                         ;; Create a temporary file for the patch
                         (let ((patch-file (make-temp-file "ellellemm-patch-")))
                           (when *ellellemm-debug-mode*
                             (message "patch-file: %s" patch-file))
                           (with-temp-file patch-file
                             (insert patch-content)
                             (let ((current-prefix-arg 2))
                               (ediff-patch-buffer 2 output-buffer)))))
                     (error "No valid patch found in the generated content"))))))))


             

;; ************* END USER FACING **********************************
;; Emacs interactive functions
;; ****************************************************************

(defun ellellemm-set-model (model)
  "Set the active MODEL for ellellemm queries."
  (interactive
   (list
    (completing-read "Choose model: "
                     '("claude-3-7-sonnet-latest"
                       "claude-3-5-haiku-latest"
                       "llama-3.1-70b-versatile"
                       "llama-3.1-8b-instant"
                       "mixtral-8x7b-32768"
                       "gemini-2.0-flash"
                       "gemini-2.0-pro-exp-02-05"))))
  (setq *ellellemm-model* model)
  (message "Active model set to %s" model))


(defun ellellemm-ask (question)
  "Ask QUESTION using the current *ellellemm-model*."
  (interactive "sAsk your question: ")
  (add-to-list '*ellellemm-questions* question)
  (ellellemm-ask-model question *ellellemm-model*))

(defun ellellemm-explain-region ()
  "Explain the code in the selected region using the current *ellellemm-model*."
  (interactive)
  (ellellemm-explain-region-model *ellellemm-model*))

(defun ellellemm-ask-about-region (question)
  "Ask a QUESTION about the code in the selected region using the current *ellellemm-model*."
  (interactive "sAsk a question about the selected code: ")
  (ellellemm-ask-about-region-model question *ellellemm-model*))

(defun ellellemm-toggle-debug-mode ()
  "Toggle the debug mode for ellellemm operations."
  (interactive)
  (setq *ellellemm-debug-mode* (not *ellellemm-debug-mode*))
  (message "Ellellemm debug mode %s" (if *ellellemm-debug-mode* "enabled" "disabled")))
(provide 'ellellemm)

(defun ellellemm-generate-patch (instructions)
  "Generate and apply a patch for the current buffer based on INSTRUCTIONS using the active model."
  (interactive "sEnter patch instructions: ")
  (ellellemm-generate-patch-model instructions *ellellemm-model*))

(defun ellellemm-accept-patch ()
  "Replace the content of the current buffer with the content of its patched version."
  (interactive)
  (let* ((current-buffer-name (buffer-name))
         (patched-buffer-name (concat current-buffer-name "_patched"))
         (patched-buffer (get-buffer patched-buffer-name)))
    (if patched-buffer
        (progn
          ;; Ensure we're in the original buffer
          (switch-to-buffer current-buffer-name)
          ;; Replace contents
          (let ((inhibit-read-only t))  ; Temporarily allow editing read-only buffers
            (erase-buffer)
            (insert-buffer-substring patched-buffer))
          ;; Kill the patched buffer
          (kill-buffer patched-buffer)
          (make-buffer-writable (get-buffer current-buffer-name))
          (message "Patch accepted and applied. Buffer '%s' updated." current-buffer-name))
      (error "No patched buffer found with name '%s'" patched-buffer-name))))

(defun ellellemm-reject-patch ()
  "Reject the patch by killing the patched buffer without modifying the current buffer."
  (interactive)
  (let* ((current-buffer-name (buffer-name))
         (patched-buffer-name (concat current-buffer-name "_patched"))
         (patched-buffer (get-buffer patched-buffer-name)))
    (if patched-buffer
        (progn
          (kill-buffer patched-buffer)
          (message "Patch rejected. Buffer '%s' was killed." patched-buffer-name))
      (message "No patched buffer found with name '%s'. Nothing to reject." patched-buffer-name))))



;;; ellellemm.el ends here
