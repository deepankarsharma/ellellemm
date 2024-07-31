# ellellemm

ellellemm is an Emacs package that allows idiomatic use of Large Language Models (LLMs) within Emacs. It supports models from both Groq and Anthropic, providing a seamless interface for interacting with these powerful AI models directly from your Emacs environment.

## Features

- Support for multiple LLM providers (Groq and Anthropic)
- Easy model switching
- Interactive functions for asking questions, explaining code, and more
- Debug mode for troubleshooting
- Markdown output for clear formatting

## Installation

1. Ensure you have Emacs 24.3 or later installed.
2. Copy `ellellemm.el` to your Emacs load path.
3. Add the following to your Emacs configuration:

```elisp
(require 'ellellemm)
```

## Configuration

Set your API keys as environment variables:

- For Anthropic: `ANTHROPIC_API_KEY`
- For Groq: `GROQ_API_KEY`

## Usage

ellellemm provides several interactive functions for interacting with LLMs:

| Function | Description |
|----------|-------------|
| `ellellemm-set-model` | Set the active model for ellellemm queries |
| `ellellemm-ask` | Ask a question using the current model |
| `ellellemm-explain-region` | Explain the code in the selected region using the current model |
| `ellellemm-ask-about-region` | Ask a question about the code in the selected region using the current model |
| `ellellemm-toggle-debug-mode` | Toggle debug mode for ellellemm operations |

To use these functions, call them interactively using `M-x` or bind them to keyboard shortcuts.

## Supported Models

ellellemm supports the following models:

### Groq
- llama-3.1-70b-versatile
- llama-3.1-8b-instant
- mixtral-8x7b-32768

### Anthropic (Claude)
- claude-3-5-sonnet-20240620
- claude-3-opus-20240229
- claude-3-haiku-20240307

You can switch between these models using the `ellellemm-set-model` function.

## Contributing

Contributions to ellellemm are welcome! Please feel free to submit pull requests or create issues for bugs and feature requests.

## Disclaimer

This software is provided as-is, and users are responsible for complying with the terms of service of the LLM providers (Groq and Anthropic) when using this package.
