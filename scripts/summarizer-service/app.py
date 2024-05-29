#!/usr/bin/env python3

from flask import Flask, request, jsonify

app = Flask(__name__)

@app.route("/scuttle")
def summarize_for_scuttle():
      # accept inputs and produce stub output for testing
      # inputs: URL
      url = request.args.get('url')
      if url:
          # results application/json: dict of "url", "title", "summary", "keywords"
          response_dict = {"url": url, "title": "TBD", "summary": "TBD", "keywords": "TBD"}
          return jsonify(response_dict)
      return "<p>URL parameter is missing.</p>"

@app.route("/summarize")
def summarize_with_prompt():
      # accept inputs and produce stub output for testing
      # inputs: URL, prompt
      url = request.args.get('url')
      prompt = request.args.get('prompt')
      if url and prompt:
          # result: text/plain
          response_text = f"{prompt}\nURL: {url}\nSummary: TBD"
          return response_text
      return "<p>URL or prompt parameter is missing.</p>"

