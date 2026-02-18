#!/usr/bin/env python3

import argparse
import sys
from transformers import AutoTokenizer

SYSTEM_CONTENT=False

def calculate_prompt(model_name, system_content, user_content, assistant_content):
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    if SYSTEM_CONTENT:
        messages = [
            # {"role": "system", "content": system_content},
            {"role": "user", "content": user_content},
            {"role": "assistant", "content": assistant_content},
        ]
    else:
        messages = [
            {"role": "user", "content": "user content 1"},
            {"role": "assistant", "content" : "assistant response 1"},
            {"role": "user", "content" : "user content 2"},
            {"role": "assistant", "content" : "assistant response 2"},
            {"role": "user", "content" : "user content 3"},
            {"role": "assistant", "content" : "assistant response 3"}
        ]
    try:
        prompt = tokenizer.apply_chat_template(messages, tokenize=False, add_generation_prompt=True)
    except NameError as e:
        raise Exception("maybe you need to install HuggingFace Transformers", e)
    return prompt, tokenizer.chat_template

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("model_name", help="Name of the model to use")
    parser.add_argument("--system-content", type=str, default="system prompt", help="Content for system prompt")
    parser.add_argument("--user-content", type=str, default="user content", help="Content for user prompt")
    parser.add_argument("--assistant-content", type=str, default="assistant content", help="Content for assistant prompt")
    args = parser.parse_args()

    prompt, chat_template = calculate_prompt(args.model_name, args.system_content, args.user_content, args.assistant_content)
    print(f"prompt: {prompt}")
    print(f"chat_template: {chat_template}")
