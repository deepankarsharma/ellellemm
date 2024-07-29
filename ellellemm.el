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

(defun get-anthropic-api-key ()
  "Retrieve the Anthropic API key from the ANTHROPIC_API_KEY environment variable."
  (or (getenv "ANTHROPIC_API_KEY")
      (error "ANTHROPIC_API_KEY environment variable is not set")))


(defun parse-content-block-deltas (input-string)
  "Parse INPUT-STRING, extract text from content block deltas, and return as a concatenated string."
  (let ((lines (split-string input-string "\n"))
        (result ""))
    (dolist (line lines)
      (when (string-prefix-p "data:" line)
        (let* ((json-string (substring line 5))  ; Remove "data:" prefix
               (json-obj (condition-case nil
                             (json-read-from-string json-string)
                           (error nil))))  ; Ignore JSON parsing errors
          (when (and json-obj
                     (equal (alist-get 'type json-obj) "content_block_delta")
                     (alist-get 'delta json-obj)
                     (alist-get 'text (alist-get 'delta json-obj)))
            (setq result (concat result (alist-get 'text (alist-get 'delta json-obj))))))))
    result))  ; Return the concatenated string

(defun stream-claude-response (prompt)
  "Stream Claude's response to PROMPT and insert it into the current buffer."
; (interactive "sEnter your prompt: ")
  (let ((url "https://api.anthropic.com/v1/messages")
        (api-key (get-anthropic-api-key))
        (start-point (point))
        (partial-line "")
        (full-response ""))
      
    (plz 'post url
      :headers `(("anthropic-version" . "2023-06-01")
                 ("content-type" . "application/json")
                 ("x-api-key" . ,api-key))
      :body (json-encode
             `(("model" . "claude-3-5-sonnet-20240620")
               ("messages" . [,(list (cons "role" "user")
                                     (cons "content" prompt))])
               ("max_tokens" . 1024)
               ("stream" . t)))
      :decode nil
      :as 'buffer
      :filter
      (lambda (_proc string)
        (insert (parse-content-block-deltas string))
        (sit-for 0.01))
      :finally
      (lambda ()
        (message "Claude's response complete.")))))


(defun ellellemm-region-as-question ()
  "Wrap the current region with <QUESTION> tags and comment it out."
  (interactive)
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      ;; Insert the opening tag
      (goto-char beg)
      (insert "<QUESTION>\n")
      ;; Insert the closing tag
      (goto-char (+ end 11))  ; +11 to account for the inserted opening tag
      (insert "\n</QUESTION>")
      ;; Expand the region to include the added tags
      (set-mark (- beg 1))  ; -1 to include the opening '<'
      (goto-char (+ end 24))  ; +24 to account for both tags and newlines
      ;; Comment out the expanded region
      (comment-region (region-beginning) (region-end)))))

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

(defun generate-prompt ()
  "Generate a prompt for a LLM, including buffer mode info and instructions for handling questions and answers."
  (let* ((mode-name (symbol-name major-mode))
         (buffer-text (buffer-substring-no-properties (point-min) (point-max))))
    (concat
     "You are acting as a text processing assistant. "
     "The current buffer is in " mode-name " mode"
     ". Factor this information into your responses.\n\n"
     "Process the following text according to these strict guidelines:\n"
     "1. For each <QUESTION> tag pair:\n"
     "   a. Change the tags from <QUESTION> to <ANSWERED>.\n"
     "   b. Insert a concise, relevant answer immediately AFTER the closing </ANSWERED> tag.\n"
     "   c. Make sure the entire question from the <QUESTION> tag is present in the <ANSWERED>.\n tag"
     "2. Do not insert any text, comments, or explanations outside of the answers.\n"
     "3. Do not add any introductory or concluding remarks.\n"
     "4. Maintain the original formatting and structure of the text.\n"
     "5. Only make the following changes: change question tags to answer tags and insert answers after the closing tags.\n\n"
     "Here's the text to process:\n\n"
     buffer-text
     "\n\nProcess the above text strictly according to the given instructions, without any additional commentary.")))


(defun ellellemm-fill-buffer-with-answers ()
  "Answer questions in current buffer,"
  (interactive)
  (let*
      ((prompt (generate-prompt)))
    (erase-buffer)
  (stream-claude-response prompt)))

(provide 'ellellemm)

;;; ellellemm.el ends here
