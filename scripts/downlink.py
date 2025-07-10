#!/usr/bin/env python3

# pip install requests-html markdownify
# See https://medium.com/@tubelwj/requests-html-an-html-parsing-library-in-python-8d182d13ecd2
# Qwen2.5-Coder-32B-Instruct-Q5_K_S.gguf

import os
import argparse
from requests_html import HTMLSession
from markdownify import markdownify as md

USER_AGENT = """Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"""

def fetch_and_convert_to_markdown(url, user_agent):
    session = HTMLSession(browser_args=[f"""--user-agent='{user_agent}'"""])
    headers = { 'User-Agent': user_agent }
    response = session.get(url, headers=headers)
    
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
    parser.add_argument("--user-agent", type=str, help="Optional User Agent header to send.")
    
    args = parser.parse_args()
    
    user_agent = args.user_agent or os.getenv("USER_AGENT", None) or USER_AGENT

    markdown_text = fetch_and_convert_to_markdown(args.url, user_agent)

    if markdown_text:
        print(markdown_text)

if __name__ == "__main__":
    main()
