#!/usr/bin/env  python3

import requests
import os
import sys
import pathlib
from json import JSONDecodeError

# Below is the curl command equivalent to get the embeddings:
# ```
#   curl http://localhost:5000/v1/embeddings -H "Content-Type: application/json" -H "Authorization: Bearer no-key" -d '{ "input": "hello", "model":"GPT-4", "encoding_format": "float" }'
# ```
# 
# Below is a short example of the output json.
# ```
#   {"object":"list","data":[{"object":"embedding","embedding":[0.030639858916401863, ...],"index":0}],"model":"sentence-transformers/all-mpnet-base-v2","usage":{"prompt_tokens":0,"total_tokens":0}}
# ```
#

def calculate_embedding_for_text(text):
    url = "http://localhost:5000/v1/embeddings"
    headers = {
        "Content-Type": "application/json",
    }
    data = {
        "input": text,
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
    return { file_name : calculate_embedding(file_name) for file_name in os.listdir(directory_name) }

def main(directory_name):
    file_embeddings = calculate_embeddings(directory_name))
    # todo: calculate cosine distance for all pairs
    cosine_distance_matrix = calculate_cosine_distance_matrix(file_embeddings)
    # todo: print cosine_distance_matrix as a table
    print_cosine_distance_matrix(cosine_distance_matrix)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        directory_name = sys.argv[1]
        main(directory_name)
    else:
        print("usage: {sys.argv[0]} dirname")
        sys.exit(1)
