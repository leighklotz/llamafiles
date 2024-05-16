I've re-written the given python-mode to address the 'todo' items, following user instructions. I've created a function `calculate_embedding_for_text()` that sends a POST request using the `requests` library to the OpenAI-compatible API and returns the JSON response containing the embeddings. I've also updated `calculate_embeddings()` to call `calculate_embedding_for_text()` for each file in the given directory.
```python
#!/usr/bin/env  python3

import requests
import os
import sys
import pathlib
from json import JSONDecodeError

def calculate_embedding_for_text(text):
    url = "http://localhost:5000/v1/embeddings"
    headers = {
        "Content-Type": "application/json",
        "Authorization": "Bearer no-key"
    }
    data = {
        "input": text,
        "model": "GPT-4",
        "encoding_format": "float"
    }

    try:
        response = requests.post(url, headers=headers, json=data)
        response.raise_for_status()
        return response.json()
    except JSONDecodeError as e:
        print(f"Error decoding JSON: {e}")
        return None
    except requests.exceptions.HTTPError as e:
        print(f"HTTP Error: {e}")
        return None

def calculate_embedding(file_name):
    if not pathlib.Path(file_name).exists():
        print(f"File '{file_name}' does not exist")
        return None

    try:
        with open(file_name, 'r') as f:
            text = f.read()
    except Exception as e:
        print(f"Error reading file '{file_name}': {e}")
        return None

    embeddings = calculate_embedding_for_text(text)
    if embeddings is not None:
        return embeddings['data'][0]['embedding']
    return None

def calculate_embeddings(directory_name):
    embeddings = {}
    for file_
