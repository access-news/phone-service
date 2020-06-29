#!/bin/sh

# DESCRIPTION
# Generate TTS  audio from input text  ($1) with speed
# ($2) onto the standard input.

# EXAMPLE
# $ ./google-tts-wav.sh "Press 7, for Safeway." 0.87 > prompts/safeway.wav

# Note: All links below have been saved to web.archive.org on 2020/05/23.

# DEPENDENCIES
#
# + gcloud
# + curl
# + jq
# + base64
#
# There is  a `shell.nix` in the  project, and running
# it   with  `nix-shell   ./shell.nix`  will   install
# everthing needed.


# Easiest way to install them is using Nix:
# # straightforward:
# $ nix-env -iA nixpkgs.jq
# $ nix-env -iA nixpkgs.google-cloud-sdk
# # if nixos/nixpkgs is cloned:
# $ nix-env -f path/to/cloned/nixpkgs -iA pkgs.curl

# OTHER REQUIRED STEPS BEFORE USING

# 1. Have a GCE service account JSON key ready,
#
#    Follow steps at
#    https://cloud.google.com/text-to-speech/docs/quickstart-protocol
#    but beware that creating  a service account will not
#    automatically  trigger  a  JSON download.  For  that
#    press "+ CREATE KEY".

# 2. copy it and this script to the machine where needed,
#
#    For example:
#    $ scp google-tts-wav.sh blabla-92618-bec1d0a875c7.json  52.1.2.3:/to/dir

# 3. create environment variable with credentials file
#    (i.e., edit the line below)
#
#    $ export GOOGLE_APPLICATION_CREDENTIALS="$(pwd)/blabla-92618-bec1d0a875c7.json"

# NOTE Hardcoding it here until steps to production are finalized {{-
# Rationale: Was running this in a dev shell from the app, but it
#            was not  working correctly  as the  `gcloud` command
#            below did not find the  credentials - because it has
#            not  been exported  when the  initial ERL  shell was
#            started. This way the  script is self-contained, but
#            requires editing  whenever the service  account JSON
#            file is replaced.
# }}-
export GOOGLE_APPLICATION_CREDENTIALS="$(pwd)/your-service-account.json"

# For the available REST request options, see
# https://cloud.google.com/text-to-speech/docs/reference/rest/v1beta1/text/synthesize

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

# vim: set fdm=marker:
# vim: set foldmarker={{-,}}-:
