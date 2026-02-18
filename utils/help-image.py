#!/usr/bin/env python3

import argparse
import base64
import json
import os
import sys
import requests
import subprocess

# USAGE:
# help-image.py --image-file=my_image.jpg "Describe this image"
# help-image.py --image-url=https://example.com/image.jpg "What is in this picture?" "Is it a cat?"
# help-image.py --image-file=image.png  this is a test prompt


def main():
    parser = argparse.ArgumentParser(description="Send image to VIA API and ask a question.")
    parser.add_argument("--image-file", help="Path to the image file.")
    parser.add_argument("--image-url",  help="URL of the image.")
    parser.add_argument("prompt", nargs='+', help="Text prompt to send to the VIA API.")
    args = parser.parse_args()

    VIA_API_CHAT_BASE = os.getenv("VIA_API_CHAT_BASE")

    # Prompt is joined before its first use for clarity
    prompt = " ".join(args.prompt)

    image_data = None

    if args.image_file:
        image_data = process_image_arg(args.image_file)
    elif args.image_url:
        image_data = args.image_url

    data = {
        "messages": []
    }

    if image_data:
        data["messages"].append({
            "role": "user",
            "image_url": image_data
        })
    
    data["messages"].append({
        "role": "user",
        "content": prompt
    })


    url = f"{VIA_API_CHAT_BASE}/v1/chat/completions"

    try:
        print((url, data))
        response = requests.post(url, json=data)
        response.raise_for_status()  # Raise HTTPError for bad responses (4xx or 5xx)
        print(response.text)
    except requests.exceptions.RequestException as e:
        print(f"Error during request: {e}")


def process_image_arg(image_file):
    """
    Handles the loading and encoding of an image file.

    Args:
        image_file: Path to the image file.

    Returns:
        A string containing the base64 encoded image data with the appropriate header,
        or None if the file is not found or the format is unsupported.
    """
    try:
        with open(image_file, "rb") as img_file:
            img_bytes = img_file.read()
            file_extension = os.path.splitext(image_file)[1].lower()

            if file_extension == ".jpg" or file_extension == ".jpeg":
                return f"data:image/jpeg;base64,{base64.b64encode(img_bytes).decode('utf-8')}"
            elif file_extension == ".png":
                return f"data:image/png;base64,{base64.b64encode(img_bytes).decode('utf-8')}"
            else:
                print(f"Error: Unsupported image format: {file_extension}.  Only JPG/JPEG and PNG are supported.")
                return None
    except FileNotFoundError:
        print(f"Error: Image file not found at {image_file}")
        return None



if __name__ == "__main__":
    main()

