#!/usr/bin/env python3

# Adapted from https://github.com/ggerganov/llama.cpp/wiki/Templates-supported-by-llama_chat_apply_template
from transformers import AutoTokenizer

VARIANTS_TO_TEST = [
    'teknium/OpenHermes-2.5-Mistral-7B',
    'mistralai/Mistral-7B-Instruct-v0.2',
    'TheBloke/FusionNet_34Bx2_MoE-AWQ',
    'bofenghuang/vigogne-2-70b-chat',
    'mlabonne/AlphaMonarch-7B',
    'google/gemma-7b-it',
]

HISTORY = [
    { 'role': 'system', 'content': 'test' },
    { 'role': 'user', 'content': 'hello' },
    { 'role': 'assistant', 'content': 'response' },
    { 'role': 'user', 'content': 'again' },
    { 'role': 'assistant', 'content': 'response' },
]

for variant in VARIANTS_TO_TEST:
    history = [m for m in HISTORY] # copy
    if 'Mistral' in variant or 'gemma' in variant:
        history.pop(0) # no system prompt for mistral and gemma
    if 'gemma' in variant:
        # GemmaTokenizer is not yet support by the time this code is written
        GEMMA_TMLP = "{% if messages[0]['role'] == 'system' %}{{ raise_exception('System role not supported') }}{% endif %}{% for message in messages %}{% if (message['role'] == 'user') != (loop.index0 % 2 == 0) %}{{ raise_exception('Conversation roles must alternate user/assistant/user/assistant/...') }}{% endif %}{% if (message['role'] == 'assistant') %}{% set role = 'model' %}{% else %}{% set role = message['role'] %}{% endif %}{{ '<start_of_turn>' + role + '\n' + message['content'] | trim + '<end_of_turn>\n' }}{% endfor %}{% if add_generation_prompt %}{{'<start_of_turn>model\n'}}{% endif %}"
        print('Gemma')
        output = AutoTokenizer.from_pretrained(VARIANTS_TO_TEST[0]).apply_chat_template(history, tokenize=False, chat_template=GEMMA_TMLP)
        print(output)
        print('-' * 30)
    else:
        print(variant)
        tokenizer = AutoTokenizer.from_pretrained(variant)
        print(tokenizer.apply_chat_template(history, tokenize=False))
        print('-' * 30)
