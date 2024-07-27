# ellellemm

`ellellemm` is an Emacs package that allows users to interact with Language Models (LLMs) directly from within Emacs. It provides functionality to ask questions, get answers, and process text using LLMs, specifically using the Anthropic Claude API.

## Features

- Stream responses from Claude directly into your Emacs buffer
- Mark regions or lines as questions for LLM processing
- Process entire buffers to answer multiple questions at once
- Seamless integration with Emacs workflow

## Installation

1. Ensure you have Emacs 24.3 or later installed.
2. Place `ellellemm.el` in your Emacs load path.
3. Add the following to your Emacs configuration:

```elisp
(require 'ellellemm)
```

4. Set the `ANTHROPIC_API_KEY` environment variable with your Anthropic API key.

## Usage

Here are the main interactive functions provided by `ellellemm`:

| Function | Description |
|----------|-------------|
| `ellellemm-region-as-question` | Wraps the current region with `<QUESTION>` tags and comments it out. |
| `ellellemm-line-as-question` | Sets the current line as a question, wrapping it with `<QUESTION>` tags and commenting it out. |
| `ellellemm-fill-buffer-with-answers` | Processes the entire buffer, answering all questions marked with `<QUESTION>` tags. |

## Example Workflow

1. Write your questions in the buffer, each on a separate line.
2. Use `ellellemm-line-as-question` on each line to mark them as questions.
3. Once all questions are marked, use `ellellemm-fill-buffer-with-answers` to process and answer all questions.

## Configuration

The package uses the `ANTHROPIC_API_KEY` environment variable for authentication. Make sure to set this variable with your Anthropic API key before using the package.

## Dependencies

- `plz`: Used for making HTTP requests to the Anthropic API.

## Contributing

Contributions to `ellellemm` are welcome! Please feel free to submit pull requests or create issues on the GitHub repository.

## License

This project is licensed under the Apache License 2.0. See the [LICENSE](LICENSE) file for details.

## Author

Deepankar Sharma <deepankarsharma@gmail.com>

## Acknowledgments

- Thanks to Anthropic for providing the Claude API.
- This package is inspired by the growing need for AI assistance in programming environments.
