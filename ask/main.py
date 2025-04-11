
import click
import os
from google import genai
from google.api_core import exceptions as google_exceptions

# --- Configuration ---
# Attempt to get the API key from environment variables
API_KEY = os.getenv("GEMINI_API_KEY")
MODEL_NAME = "gemini-2.0-flash" # Use the latest 1.5 Pro model

# --- Click CLI Setup ---

@click.group()
def cli():
    """
    A simple CLI tool to interact with Google Gemini.
    Requires the GEMINI_API_KEY environment variable to be set.
    """
    pass


@cli.command(name="list-models") # Use kebab-case for command name
def list_models_command():
    """
    Lists available Gemini models accessible with the configured API key.
    """
    # API key check/config is handled by the main cli group now
    click.echo("Fetching available models...")
    try:
        client = genai.Client(api_key=API_KEY)
        models = client.models.list()
        click.echo(click.style("--- Available Models ---", fg='cyan'))
        count = 0
        for m in models:
            # We only want models that support 'generateContent' for the 'ask' command
            if 'generateContent' in m.supported_generation_methods:
                count += 1
                click.echo(f"  Name: {click.style(m.name, fg='green')}")
                click.echo(f"    Display Name: {m.display_name}")
                # click.echo(f"    Description: {m.description}") # Can be verbose
                click.echo(f"    Supported Methods: {m.supported_generation_methods}")
                click.echo("-" * 20)
        if count == 0:
             click.echo(click.style("No models supporting 'generateContent' found for this API key.", fg='yellow'))
        else:
             click.echo(f"Found {count} models supporting 'generateContent'.")
        click.echo(click.style("--- End of List ---", fg='cyan'))

    except google_exceptions.PermissionDenied as e:
         click.echo(click.style(f"\nError: Permission Denied. Is your API key valid and enabled for listing models?", fg='red'))
         click.echo(f"Details: {e}")
    except Exception as e:
        click.echo(click.style(f"\nAn unexpected error occurred: {type(e).__name__}", fg='red'))
        click.echo(f"Details: {e}")

@cli.command()
@click.argument('question', type=str)
@click.option('--model', default=MODEL_NAME, help=f'Which Gemini model to use (default: {MODEL_NAME})')
def ask(question, model):
    """
    Asks a question to the configured Google Gemini model.

    Example:
    aitool ask "How do I write a factorial function in Python?"
    """
    if not API_KEY:
        click.echo(click.style("Error: GEMINI_API_KEY environment variable not set.", fg='red'))
        click.echo("Please set the environment variable with your Google Gemini API key.")
        # Example: export GEMINI_API_KEY='YOUR_API_KEY_HERE' (Linux/macOS)
        #          set GEMINI_API_KEY=YOUR_API_KEY_HERE (Windows CMD)
        #          $env:GEMINI_API_KEY='YOUR_API_KEY_HERE' (Windows PowerShell)
        return # Exit the command

    try:
        # --- Configure Gemini API ---
        client = genai.Client(api_key=API_KEY)
        # --- Create the Model ---
        click.echo(f"Using model: {model}")

        # --- Send Prompt and Get Response ---
        click.echo("Asking Gemini...")
        response = client.models.generate_content_stream(model=model, contents=[question])
        # --- Display Response ---
        click.echo(click.style("\n--- Gemini's Answer ---", fg='green'))
        for chunk in response:
            click.echo(chunk.text)
        click.echo(click.style("--- End of Answer ---", fg='green'))

    except google_exceptions.PermissionDenied as e:
         click.echo(click.style(f"\nError: Permission Denied. Is your API key valid and enabled?", fg='red'))
         click.echo(f"Details: {e}")
    except google_exceptions.InvalidArgument as e:
        click.echo(click.style(f"\nError: Invalid Argument.", fg='red'))
        click.echo(f"Details: {e}")
        click.echo("This might happen if the model name is incorrect or the input is malformed.")
    except google_exceptions.ResourceExhausted as e:
         click.echo(click.style(f"\nError: Resource Exhausted. You might have hit API rate limits.", fg='red'))
         click.echo(f"Details: {e}")
    except AttributeError:
        # Handle cases where the response might not have 'text' (e.g., safety blocks)
        click.echo(click.style("\nWarning: Could not extract text from the response.", fg='yellow'))
        click.echo("The response might have been blocked due to safety settings or other reasons.")
        try:
            click.echo(f"Full response object:\n{response}")
            if response.prompt_feedback:
                 click.echo(f"Prompt Feedback: {response.prompt_feedback}")
        except Exception:
             click.echo("Could not display full response details.") # Avoid potential recursive errors
        raise
    except Exception as e:
        click.echo(click.style(f"\nAn unexpected error occurred: {type(e).__name__}", fg='red'))
        click.echo(f"Details: {e}")


# --- Main Execution ---
if __name__ == '__main__':
    cli()
