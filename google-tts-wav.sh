#!/bin/bash

TEXT="$1"
SPEED="$2"
JSON_REQUEST_TEMPLATE='
{
  "input":{
    "text":"'$TEXT'"
  },
  "voice":{
    "languageCode":"en-US",
    "name":"en-US-Wavenet-E",
    "ssmlGender":"FEMALE"
  },
  "audioConfig":{
    "audioEncoding":"LINEAR16",
    "speakingRate":"'$SPEED'"
  }
}'

curl -X POST \
  -H "Authorization: Bearer "$(gcloud auth application-default print-access-token) \
  -H "Content-Type: application/json; charset=utf-8" \
  -d "$JSON_REQUEST_TEMPLATE" \
  https://texttospeech.googleapis.com/v1/text:synthesize \
| \
jq --raw-output ".audioContent" \
| \
base64 --decode
