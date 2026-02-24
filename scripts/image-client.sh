#!/bin/bash

FN=$1

IMAGE_BASE64=$(base64 -w 0 "$FN")

cat <<EOF | curl -s -X POST http://localhost:5000/v1/chat/completions \
  -H "Content-Type: application/json" \
  --data-binary @- | jq -r '.choices[0].message.content'
{
  "messages": [
    {
      "role": "user",
      "content": [
        {
          "type": "text",
          "text": "Describe this image in detail."
        },
        {
          "type": "image_url",
          "image_url": {
              "url": "data:image/jpeg;base64,$IMAGE_BASE64"
            }
        }
      ]
    }
  ]
}
EOF
