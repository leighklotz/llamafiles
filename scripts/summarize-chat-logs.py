#!/usr/bin/env python3
import os
import json
import openai
import argparse
from pathlib import Path

OPENAI_API_KEY = os.getenv('OPENAI_API_KEY')
VIA_API_CHAT_BASE = os.getenv('VIA_API_CHAT_BASE')
MODEL = os.getenv('MODEL', 'gpt-3.5')

def extract_conversation_text(log_data):
    """Builds a text transcript of the conversation from 'internal' messages."""
    conversation = []
    for user_msg, assistant_msg in log_data.get("internal", []):
        if user_msg.strip():
            conversation.append(f"User: {user_msg.strip()}")
        if assistant_msg.strip():
            conversation.append(f"Assistant: {assistant_msg.strip()}")
    return "\n".join(conversation)

def summarize_chat_log(log_file_path, model="gpt-4", max_tokens=200):
    """Summarizes a chat log file using the OpenAI API."""
    try:
        with open(log_file_path, "r", encoding="utf-8") as f:
            log_data = json.load(f)
    except FileNotFoundError:
        print(f"❌ File not found: {log_file_path}")
        return None
    except json.JSONDecodeError:
        print(f"❌ Invalid JSON in: {log_file_path}")
        return None

    chat_text = extract_conversation_text(log_data)
    if not chat_text.strip():
        print(f"⚠️ No content to summarize in {log_file_path}")
        return None

    temperature = 0.5
    messages = [
        {"role": "system", "content": "You are a helpful assistant that summarizes conversations."},
        {"role": "user", "content": f"Summarize the following conversation in 1-2 sentences:\n\n{chat_text}"}
    ]
    return chat_call(log_file_path, model=model, messages=messages, max_tokens=max_tokens, temperature=temperature)

def chat_call(log_file_path, **kwargs):
    """
    Calls the OpenAI chat completion API with the given parameters and any additional keyword arguments.

    Args:
        model (str): The name of the model to use.
        messages (list): The list of messages to send to the chat.
        max_tokens (int): The maximum number of tokens to generate.
        temperature (float): The temperature to use for sampling.
        **kwargs: Any additional keyword arguments to pass to openai.chat.completions.create().

    Returns:
        str: The content of the first message in the response, stripped of whitespace, or None if an error occurred.
    """
    try:
        if VIA_API_CHAT_BASE:
            print(f"📄 {VIA_API_CHAT_BASE=}")
            openai.base_url = VIA_API_CHAT_BASE + "/v1/"
        openai.api_key = OPENAI_API_KEY
        import pdb;pdb.set_trace()
        response = openai.chat.completions.create(**kwargs)
        return response.choices[0].message.content.strip()

    except Exception as e:
        print(f"❌ Error summarizing {log_file_path.name}: {e}")
        raise
        # return None

def summarize_directory(directory, api_key, model="gpt-4", max_tokens=200, base_url=None):
    """Processes all chat logs in a directory and writes summaries to sidecar files."""
    directory_path = Path(directory)
    if not directory_path.exists() or not directory_path.is_dir():
        print(f"❌ Invalid directory: {directory}")
        return

    for json_file in directory_path.glob("*.json"):
        print(f"📄 Summarizing {json_file.name}")
        summary = summarize_chat_log(json_file, model, max_tokens)
        if summary:
            summary_path = json_file.with_name(json_file.stem + "-summary.txt")
            with open(summary_path, "w", encoding="utf-8") as f:
                f.write(summary + "\n")
            print(f"✅ Saved summary to {summary_path.name}")
        else:
            print(f"⚠️ Skipped {json_file.name}")

def main():
    parser = argparse.ArgumentParser(description="Summarize Oobabooga chat logs using OpenAI API.")
    parser.add_argument("directory", help="Directory containing .json chat logs.")
    parser.add_argument("--model", default="gpt-4", help="OpenAI model to use (default: gpt-4)")
    parser.add_argument("--max_tokens", type=int, default=200, help="Max tokens for each summary.")
    args = parser.parse_args()

    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        print("❌ Please set the OPENAI_API_KEY environment variable.")
        return

    via_api_chat_base = os.getenv("VIA_API_CHAT_BASE")
    summarize_directory(args.directory, api_key, args.model, args.max_tokens, via_api_chat_base)

if __name__ == "__main__":
    main()
