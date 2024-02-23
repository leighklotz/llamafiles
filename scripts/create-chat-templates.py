#!/usr/bin/env python3

# This Python script is adapted from the Llama.cpp wiki and provides functions to generate Bash templates
# for different LLM model variants. It uses the Hugging Face Transformers library to generate the
# templates, and supports a variety of pre-trained models. The script includes a set of pre-defined
# templates and history arrays for different conversation scenarios.

# Adapted from https://github.com/ggerganov/llama.cpp/wiki/Templates-supported-by-llama_chat_apply_template

import sys
import argparse

from transformers import AutoTokenizer

VARIANTS_TO_TEST = [
    'teknium/OpenHermes-2.5-Mistral-7B',
    'mistralai/Mistral-7B-Instruct-v0.2',
    'TheBloke/FusionNet_34Bx2_MoE-AWQ',
    'bofenghuang/vigogne-2-70b-chat',
    'mlabonne/AlphaMonarch-7B',
    'google/gemma-7b-it',
]

SYSTEM_USER_HISTORY = [
    { 'role': 'system', 'content': '%s' },
    { 'role': 'user', 'content': '%s' },
]

USER_HISTORY = [
    { 'role': 'user', 'content': '%s %s' },
]

SYSTEM_USER_INPUT_HISTORY = [
    { 'role': 'system', 'content': '%s' },
    { 'role': 'user', 'content': '%s %s' },
]

USER_INPUT_HISTORY = [
    { 'role': 'user', 'content': '%s\\n\\n%s\\n\\n%s' },
]


# GemmaTokenizer is not yet support by the time this code is written
GEMMA_TEMPLATE = """{% if messages[0]['role'] == 'system' %}{{ raise_exception('System role not supported') }}{% endif %}{% for message in messages %}{% if (message['role'] == 'user') != (loop.index0 % 2 == 0) %}{{ raise_exception('Conversation roles must alternate user/assistant/user/assistant/...') }}{% endif %}{% if (message['role'] == 'assistant') %}{% set role = 'model' %}{% else %}{% set role = message['role'] %}{% endif %}{{ '<start_of_turn>' + role + '\n' + message['content'] | trim + '<end_of_turn>\n' }}{% endfor %}{% if add_generation_prompt %}{{'<start_of_turn>model\n'}}{% endif %}"""

def get_chat_template(variant, use_input):
    if 'mistral' in variant or 'mixtral' in variant or 'gemma' in variant:
        # no system prompt for mistral and gemma
        history = USER_INPUT_HISTORY if use_input else USER_HISTORY
    else:
        history = SYSTEM_USER_INPUT_HISTORY if use_input else SYSTEM_USER_HISTORY

    if 'gemma' in variant:
        tokenizer = AutoTokenizer.from_pretrained(VARIANTS_TO_TEST[0])
        output = tokenizer.apply_chat_template(history, tokenize=False, chat_template=GEMMA_TEMPLATE)
    else:
        tokenizer = AutoTokenizer.from_pretrained(variant)
        output = tokenizer.apply_chat_template(history, tokenize=False)

    return output

def generate_bash_template_function(name, variant):
    template_no_input = get_chat_template(variant, False)
    template_input = get_chat_template(variant, True)

    print(f'''
function {name}_prompt {{
    if [ "${{INPUT}}" == "" ]; then
	printf -v PROMPT "{template_no_input}" "${{SYSTEM_MESSAGE%$'\\n'}}" "${{QUESTION%$'\\n'}}"
    else
	printf -v PROMPT "{template_input}" "${{SYSTEM_MESSAGE%$'\\n'}}" "${{QUESTION%$'\\n'}}" "${{INPUT%$'\\n'}}"
    fi
}}
''')


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("name", help="Name of the item")
    parser.add_argument("variant", help="Variant of the item")
    args = parser.parse_args()

    generate_bash_template_function(args.name, args.variant)
