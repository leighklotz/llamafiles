#!/bin/bash

DOC="<<EOF
Analysis: The email is a promotional advertisement for a product called Ozempic, claiming significant weight loss results. It includes suspicious links and a generic unsubscribe instruction, which are common traits of spam emails.

Classification: SUSPICIOUS
EOF
"
printf "%s\n" "${DOC}" | ask nuextract '{"analysis": "", "classification":""}' 

# {
#    "analysis": "The email is a promotional advertisement for a product called Ozempic, claiming significant weight loss results. It includes suspicious links and a generic unsubscribe instruction, which are common traits of spam emails.",
#    "classification": "SUSPICIOUS"
# }
