#!/usr/bin/env python3

# pip install playwright markdownify
# python -m playwright install

import os
import argparse
from playwright.sync_api import sync_playwright, TimeoutError as PlaywrightTimeoutError
from markdownify import markdownify as md

USER_AGENT = """Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"""


def fetch_rendered_html(url: str, user_agent: str) -> str:
    """Fetches the rendered HTML content of a URL using Playwright."""
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        context = browser.new_context(user_agent=user_agent)
        page = context.new_page()
        try:
            page.goto(url, wait_until='domcontentloaded', timeout=10000)
            page.wait_for_timeout(1000)  # let JS settle
        except PlaywrightTimeoutError:
            print(f"Timeout while loading {url}, attempting to extract partial content.")
        except Exception as e:
            print(f"Error navigating to {url}: {e}")
            browser.close()
            return None
        content = page.content()
        browser.close()
    return content
# 
# def fetch_rendered_html(url: str, user_agent: str) -> str:
#     """Fetches the rendered HTML content of a URL using Playwright."""
#     with sync_playwright() as p:
#         browser = p.chromium.launch(headless=True)
#         context = browser.new_context(user_agent=user_agent)
#         page = context.new_page()
#         try:
#             page.goto(url, wait_until='networkidle')
#         except Exception as e:
#             print(f"Error navigating to {url}: {e}")
#             return None
#         content = page.content()
#         browser.close()
#     return content
# 
def fetch_and_convert_to_markdown(url, user_agent):
    """Fetches HTML, renders it with Playwright, and converts to Markdown."""
    html = fetch_rendered_html(url, user_agent)
    if html:
        markdown_text = md(html)
        return markdown_text
    else:
        return None

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
