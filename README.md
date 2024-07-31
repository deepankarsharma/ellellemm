# ellellemm

ellellemm is an Emacs package that allows users to leverage Large Language Models (LLMs) for coding and productivity tasks directly within Emacs.

## Features

- Interact with various LLM models from Groq and Anthropic (Claude)
- Ask questions and get responses within Emacs
- Explain code snippets using AI
- Seamless integration with your Emacs workflow

## Installation

1. Ensure you have the required dependencies:
   - Emacs 24.3 or later
   - `plz` package
   - `poly-markdown-mode` package

2. Set up your API keys as environment variables:
   - `ANTHROPIC_API_KEY` for Claude models
   - `GROQ_API_KEY` for Groq models

3. Add the `ellellemm.el` file to your Emacs load path and require it in your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/ellellemm")
(require 'ellellemm)
```

## Usage

ellellemm provides several interactive functions that you can use to interact with LLMs:

| Function Name | Description |
|---------------|-------------|
| `ellellemm-region-as-question` | Ask a question based on the selected region |
| `ellellemm-line-as-question` | Ask a question based on the current line |
| `ellellemm-groq-ask-llama31-versatile` | Ask a question using Groq's llama-3.1-70b-versatile model |
| `ellellemm-groq-ask-llama31-instant` | Ask a question using Groq's llama-3.1-8b-instant model |
| `ellellemm-groq-ask-mixtral` | Ask a question using Groq's mixtral-8x7b-32768 model |
| `ellellemm-claude-ask-sonnet` | Ask a question using Claude's claude-3-5-sonnet-20240620 model |
| `ellellemm-claude-ask-opus` | Ask a question using Claude's claude-3-opus-20240229 model |
| `ellellemm-claude-ask-haiku` | Ask a question using Claude's claude-3-haiku-20240307 model |
| `ellellemm-ask-question` | Ask a question using Claude's claude-3-5-sonnet-20240620 model (default) |
| `ellellemm-groq-explain-llama31-versatile` | Explain selected code using Groq's llama-3.1-70b-versatile model |
| `ellellemm-groq-explain-llama31-instant` | Explain selected code using Groq's llama-3.1-8b-instant model |
| `ellellemm-groq-explain-mixtral` | Explain selected code using Groq's mixtral-8x7b-32768 model |
| `ellellemm-claude-explain-sonnet` | Explain selected code using Claude's claude-3-5-sonnet-20240620 model |
| `ellellemm-claude-explain-opus` | Explain selected code using Claude's claude-3-opus-20240229 model |
| `ellellemm-claude-explain-haiku` | Explain selected code using Claude's claude-3-haiku-20240307 model |
| `ellellemm-explain-code` | Explain selected code using Claude's claude-3-5-sonnet-20240620 model (default) |

You can bind these functions to keyboard shortcuts or call them using `M-x`.

## Configuration

You can customize the behavior of ellellemm by modifying the provided functions or adding new ones based on your specific needs.

## Contributing

Contributions to ellellemm are welcome! Please feel free to submit pull requests or create issues for bugs and feature requests.

## License

This project is licensed under the Apache License, Version 2.0. See the [LICENSE](LICENSE) file for details.

## Author

Deepankar Sharma <deepankarsharma@gmail.com>
