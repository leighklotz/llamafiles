#!/usr/bin/env  python3

import requests
import os
import path

# TODO:
# Write a Python script that uses requests library to compute embedding vectors for a list of files and then computes the cosine similarity of all pairs and prints it out.

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

def calculate_embedding(file_name):
    #todo: implement
    # curl http://localhost:5000/v1/embeddings -H "Content-Type: application/json" -H "Authorization: Bearer no-key" -d '{ "input": "hello", "model":"GPT-4", "encoding_format": "float" }'
    embedding = None
    return embedding

def calculate_embeddings(directory_name):
    return { file_name : calculate_embeddings(file_name) for file_name in os.listdir(directory_name) }

def main(directory_name):
    embeddings = calculate_embeddings(directory_name)
    sim_mat = calculate_similarity_matrix(embeddings)
    print_similarity_matrix(sim_mat)
        

if __name__ == "__main__":
    main(sys.argv[1:])
