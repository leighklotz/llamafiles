#!/usr/bin/env python3

from typing import Optional, Tuple
import logging
import warnings
import argparse
import numpy as np
from sentence_transformers import SentenceTransformer
from pathlib import Path
import sys

MODEL_NAME='all-MiniLM-L6-v2'

### temp
import torch;print(f"Using GPU: {torch.cuda.is_available()}")

with warnings.catch_warnings():
    warnings.filterwarnings("ignore") # simplefilter
    MODEL = SentenceTransformer(MODEL_NAME)

def read_file(filename: str) -> Optional[str]:
    """Reads a file and returns its contents. If an error occurs, logs the error and returns None."""
    try:
        with open(filename, 'r') as file:
            return file.read()
    except Exception as e:
        logging.error("Skipping file: %s (%s)", filename, str(e))
        return None

def generate_embedding(text: str) -> np.ndarray:
    """Generates an embedding for the given text."""
    return MODEL.encode([text])[0]

def find_files(directory: str) -> Tuple[Path, ...]:
    """Finds all files in the given directory and its subdirectories."""
    return sorted((f for f in Path(directory).rglob('*') if f.is_file()), key=lambda f: f.resolve(), reverse=True)

def cosine_similarity(a: np.ndarray, b: np.ndarray) -> float:
    """Calculates the cosine similarity between two vectors."""
    return np.dot(a, b) / (np.linalg.norm(a) * np.linalg.norm(b))

def main(args: argparse.Namespace) -> None:
    if args.search == '-':
        search_text = sys.stdin.read()
    else:
        search_text = args.search

    search_embedding = generate_embedding(search_text)
    files = find_files(args.directory)

    similarities = {
        file: cosine_similarity(search_embedding, generate_embedding(read_file(file)))
        for file in files if read_file(file) is not None
    }

    relevant_files = (
        (file_name, similarity)
        for file_name, similarity in similarities.items()
        if similarity >= args.threshold
    )

    sorted_files = sorted(relevant_files, key=lambda x: x[1], reverse=True)

    for file_name, similarity in sorted_files:
        if args.no_score:
            print(file_name)
        else:
            print(f"{similarity:.4f} {file_name}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Find similar files with a similarity above the threshold.")
    parser.add_argument("search", help="Search input, use '-' for stdin")
    parser.add_argument("directory", help="Directory to search in", nargs='?', default=".", type=str)
    parser.add_argument("--threshold", help="Minimum similarity required for a file to be considered (default: 0.0)", type=float, default=0.0)
    parser.add_argument("--no-score", help="Print only filenames, no scores", action='store_true')

    args = parser.parse_args()

    main(args)
