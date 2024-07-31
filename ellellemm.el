;;; ellellemm.el --- Use LLM models within Emacs.

;; Copyright (C) 2024 Deepankar Sharma

;; Author: Deepankar Sharma <deepankarsharma@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: llm, productivity
;; URL: https://github.com/deepankarsharma/ellellemm

;;; Commentary:

;; Allow idiomatic use of LLMs while using Emacs.

;;; Code:

(require 'plz)


;; Supported models
;; Provider Name,  Model name
;; groq, llama-3.1-70b-versatile
;; groq, llama-3.1-8b-instant
;; groq, mixtral-8x7b-32768
;; claude, claude-3-5-sonnet-20240620
;; claude, claude-3-opus-20240229
;; claude, claude-3-haiku-20240307



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
            (poly-markdown-mode))
          buffer))))

(defun insert-line-separator ()
  "Insert a visible line separator followed by two newlines."
  (insert "----------------\n\n"))


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



;; Calls to LLM providers
(defun groq-external-process (prompt buffer model)
  "Stream Groq's response to PROMPT using MODEL and insert it into BUFFER using external processes."
  (let* ((url "https://api.groq.com/openai/v1/chat/completions")
         (api-key (get-groq-api-key))
         (json-payload (json-encode
                        `(("model" . ,model)
                          ("messages" . [,(list (cons "role" "user")
                                                (cons "content" prompt))])
                          ("max_tokens" . 1024)
                          ("stream" . t))))
         (curl-and-jq-command (format "curl -s -N -X POST %s \
-H 'Authorization: Bearer %s' \
-H 'Content-Type: application/json' \
-d '%s' \
| grep '^data:' \
| sed -u 's/^data: //g' \
| jq -j 'select(.choices != null) | .choices[0].delta.content // empty'"
                                       url api-key json-payload)))
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
     :sentinel (lambda (proc event)
                 (when (string= event "finished\n")
                   (message "Groq's response complete."))))))

(defun groq-external-process (prompt buffer model)
  "Stream Groq's response to PROMPT using MODEL and insert it into BUFFER using external processes."
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
| jq -j 'select(.choices != null) | .choices[0].delta.content // empty'"
                                      url api-key temp-file)))
    (message "groq curl command: %s" curl-and-jq-command)
    (unwind-protect
        (progn
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
           :sentinel (lambda (proc event)
                       (when (string= event "finished\n")
                         (message "Groq's response complete.")))))
      (delete-file temp-file))))


(defun claude-external-process (prompt buffer model)
  "Stream Claude's response to PROMPT using MODEL and insert it into BUFFER using external processes."
  (let* ((url "https://api.anthropic.com/v1/messages")
         (api-key (get-anthropic-api-key))
         (json-payload (json-encode
                        `(("model" . ,model)
                          ("messages" . [,(list (cons "role" "user")
                                                (cons "content" prompt))])
                          ("max_tokens" . 1024)
                          ("stream" . t))))
         (curl-and-jq-command (format "curl -s -N -X POST %s \
-H 'anthropic-version: 2023-06-01' \
-H 'content-type: application/json' \
-H 'x-api-key: %s' \
-d '%s' \
| grep '^data:' \
| sed -u 's/^data: //g' \
| jq -j 'select(.type == \"content_block_delta\") | .delta.text // empty'"
                                       url api-key json-payload)))
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
                    ;;(sit-for 0.01)))))
     :sentinel (lambda (proc event)
                 (when (string= event "finished\n")
                   (message "Claude's response complete."))))))

(defun claude-external-process (prompt buffer model)
  "Stream Claude's response to PROMPT using MODEL and insert it into BUFFER using external processes."
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
| jq -j 'select(.type == \"content_block_delta\") | .delta.text // empty'"
                                      url api-key temp-file)))
    (message "claude curl command: %s" curl-and-jq-command)
    (unwind-protect
        (progn
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
           :sentinel (lambda (proc event)
                       (when (string= event "finished\n")
                         (message "Claude's response complete.")))))
      (delete-file temp-file))))


;; ************* Prompts *****************


(defun generate-single-question-prompt (question)
  "Generate a prompt for Claude using the current region and surrounding context."
  (format "Answer the following technical question. Provide answer in markdown mode. %s" question))

(defun generate-code-explanation-prompt ()
  "Generate a prompt for Claude to explain the code in the selected region.  "
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

Provide a detailed explanation of what this code does, its purpose, and any important concepts or patterns it demonstrates. If you notice any potential issues or improvements, please mention those as well."
                buffer-name
                major-mode-name
                surrounding-context
                selected-code))
    (error "No region selected.  Please select a region of code to explain")))


;; ************* END USER FACING *****************
;; Emacs interactive functions
(defun ellellemm-region-as-question ()
  "Ask question based on the current region."
  (ellellemm-ask-question (buffer-substring-no-properties (region-beginning) (region-end))))

(defun ellellemm-line-as-question ()
  "Set region to current line and call wrap-and-comment-question function."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (goto-char start)
      (set-mark-command nil)
      (goto-char end)
      (ellellemm-region-as-question))))

(defun ellellemm-ask-model (question model provider)
  "Ask QUESTION to the specified MODEL using the given PROVIDER."
  (let ((prompt (generate-single-question-prompt question))
        (buffer (get-or-create-ellellemm-buffer)))
    (with-current-buffer buffer
      (insert-line-separator)
      (insert (format "# Question: %s" question))
      (newline 2)
      (insert (format "%s's response (%s):\n\n" provider model)))
    (cond
     ((string= provider "groq") (groq-external-process prompt buffer model))
     ((string= provider "claude") (claude-external-process prompt buffer model))
     (t (error "Unsupported provider: %s" provider)))))

(defun ellellemm-groq-ask-llama31-versatile (question)
  "Ask QUESTION using Groq's llama-3.1-70b-versatile model."
  (interactive "sAsk your question: ")
  (ellellemm-ask-model question "llama-3.1-70b-versatile" "groq"))

(defun ellellemm-groq-ask-llama31-instant (question)
  "Ask QUESTION using Groq's llama-3.1-8b-instant model."
  (interactive "sAsk your question: ")
  (ellellemm-ask-model question "llama-3.1-8b-instant" "groq"))

(defun ellellemm-groq-ask-mixtral (question)
  "Ask QUESTION using Groq's mixtral-8x7b-32768 model."
  (interactive "sAsk your question: ")
  (ellellemm-ask-model question "mixtral-8x7b-32768" "groq"))

(defun ellellemm-claude-ask-sonnet (question)
  "Ask QUESTION using Claude's claude-3-5-sonnet-20240620 model."
  (interactive "sAsk your question: ")
  (ellellemm-ask-model question "claude-3-5-sonnet-20240620" "claude"))

(defun ellellemm-claude-ask-opus (question)
  "Ask QUESTION using Claude's claude-3-opus-20240229 model."
  (interactive "sAsk your question: ")
  (ellellemm-ask-model question "claude-3-opus-20240229" "claude"))

(defun ellellemm-claude-ask-haiku (question)
  "Ask QUESTION using Claude's claude-3-haiku-20240307 model."
  (interactive "sAsk your question: ")
  (ellellemm-ask-model question "claude-3-haiku-20240307" "claude"))

(defun ellellemm-ask-question (question)
  "Ask QUESTION using Claude's claude-3-5-sonnet-20240620 model."
  (interactive "sAsk your question: ")
  (ellellemm-ask-model question "claude-3-5-sonnet-20240620" "claude"))

(defun ellellemm-explain-code-model (model provider)
  "Explain the code in the selected region using the specified MODEL and PROVIDER."
  (let ((prompt (generate-code-explanation-prompt))
        (buffer (get-or-create-ellellemm-buffer)))
    (with-current-buffer buffer
      (insert-line-separator)
      (insert "# Code Explanation\n\n")
      (insert (format "%s's explanation (%s):\n\n" provider model)))
    (cond
     ((string= provider "groq") (groq-external-process prompt buffer model))
     ((string= provider "claude") (claude-external-process prompt buffer model))
     (t (error "Unsupported provider: %s" provider)))))

(defun ellellemm-groq-explain-llama31-versatile ()
  "Explain the code in the selected region using Groq's llama-3.1-70b-versatile model."
  (interactive)
  (ellellemm-explain-code-model "llama-3.1-70b-versatile" "groq"))

(defun ellellemm-groq-explain-llama31-instant ()
  "Explain the code in the selected region using Groq's llama-3.1-8b-instant model."
  (interactive)
  (ellellemm-explain-code-model "llama-3.1-8b-instant" "groq"))

(defun ellellemm-groq-explain-mixtral ()
  "Explain the code in the selected region using Groq's mixtral-8x7b-32768 model."
  (interactive)
  (ellellemm-explain-code-model "mixtral-8x7b-32768" "groq"))

(defun ellellemm-claude-explain-sonnet ()
  "Explain the code in the selected region using Claude's claude-3-5-sonnet-20240620 model."
  (interactive)
  (ellellemm-explain-code-model "claude-3-5-sonnet-20240620" "claude"))

(defun ellellemm-claude-explain-opus ()
  "Explain the code in the selected region using Claude's claude-3-opus-20240229 model."
  (interactive)
  (ellellemm-explain-code-model "claude-3-opus-20240229" "claude"))

(defun ellellemm-claude-explain-haiku ()
  "Explain the code in the selected region using Claude's claude-3-haiku-20240307 model."
  (interactive)
  (ellellemm-explain-code-model "claude-3-haiku-20240307" "claude"))

(defun ellellemm-explain-code ()
  "Explain the code in the selected region using Claude's claude-3-5-sonnet-20240620 model (default)."
  (interactive)
  (ellellemm-explain-code-model "claude-3-5-sonnet-20240620" "claude"))

(provide 'ellellemm)

;;; ellellemm.el ends here
