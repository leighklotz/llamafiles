#!/usr/bin/env python3

# pip install requests-html markdownify
# See https://medium.com/@tubelwj/requests-html-an-html-parsing-library-in-python-8d182d13ecd2
# Qwen2.5-Coder-32B-Instruct-Q5_K_S.gguf

import argparse
from requests_html import HTMLSession
from markdownify import markdownify as md

def fetch_and_convert_to_markdown(url):
    session = HTMLSession()
    response = session.get(url)
    
    if response.status_code != 200:
        print(f"Failed to fetch the URL. Status code: {response.status_code}")
        return None
    
    # Render JavaScript if necessary
    response.html.render(timeout=16000)
    
    # Convert HTML to Markdown
    markdown_text = md(response.html.html)
    return markdown_text

def main():
    parser = argparse.ArgumentParser(description="Convert a webpage to Markdown.")
    parser.add_argument("url", type=str, help="The URL of the webpage to convert.")
    
    args = parser.parse_args()
    
    markdown_text = fetch_and_convert_to_markdown(args.url)
    if markdown_text:
        print(markdown_text)

if __name__ == "__main__":
    main()
