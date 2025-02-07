#!/usr/bin/env python3

# todo: fix large memory growth
# todo: try this instead: https://sbert.net/examples/applications/semantic-search/README.html

from typing import Optional, Tuple
import logging
import warnings
import argparse
import re
import numpy as np
from sentence_transformers import SentenceTransformer
from pathlib import Path
import sys

MODEL_NAME='all-MiniLM-L6-v2'

with warnings.catch_warnings():
    warnings.filterwarnings("ignore")
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
    return MODEL.encode([text])[0] if text is not None else None

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
        search_text = ' '.join(args.search)

    search_embedding = generate_embedding(search_text)
    files = find_files(args.directory)

    similarities = {
        file: similarity for file in files
        if (embedding := generate_embedding(read_file(file))) is not None
        and (similarity := cosine_similarity(search_embedding, embedding))
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

def parse_args():
    parser = argparse.ArgumentParser(description="Find similar files with a similarity above the threshold.")
    parser.add_argument("search", help="Search input, use '-' for stdin", nargs='+')
    parser.add_argument("--directory", help="Directory to search in", default=".", type=str)
    parser.add_argument("--threshold", help="Minimum similarity required for a file to be considered (default: 0.0)", type=float, default=0.0)
    parser.add_argument("--no-score", help="Print only filenames, no scores", action='store_true')
    return parser.parse_args()

if __name__ == "__main__":
    args = parse_args()
    main(args)

