#!/bin/sh

# ## 0. PREREQUISITES USING AZURE TTS
#
# [Quickstart: Get started with the Azure Speech CLI](https://learn.microsoft.com/en-us/azure/cognitive-services/speech-service/spx-basics?tabs=linuxinstall%2Cterminal)

# -> [Quickstart: Create a Cognitive Services resource using the Azure portal](https://learn.microsoft.com/en-us/azure/cognitive-services/cognitive-services-apis-create-account?tabs=multiservice%2Canomaly-detector%2Clanguage-service%2Ccomputer-vision%2Clinux)

# ## 1. AZURE TTS REST API ENDPOINTS

# Extracted from the wall of text at [Text-to-speech REST API](https://learn.microsoft.com/en-us/azure/cognitive-services/speech-service/rest-text-to-speech?tabs=streaming):

# 1. Get a list of voices available
#    https://<REGION>.tts.speech.microsoft.com/cognitiveservices/voices/list

# 2. Convert text to speech
#    https://<REGION>.tts.speech.microsoft.com/cognitiveservices/v1

# For `<REGION>`, look up the Cognitive Services resource in the Azure portal. (In our case, everyt resource is in `westus2` at the moment.)

# ### 1.1 Get available voices

# ```text
# curl \
#   --location \
#   --get https://westus2.tts.speech.microsoft.com/cognitiveservices/voices/list \
#   --header "Ocp-Apim-Subscription-Key: $(cat azure-tts-key)"
# ```

# where `azure-tts-key` is a file holding either of the 2 keys provided by the Cognitive Services resource. (TODO It would probably be better to use the Azure CLI to query the key, similar to how `./google-tts-wav.sh` uses `gcloud`.)

# ### 1.2 Convert short-form text to audio

# #### 1.2.1 Get authorization token

# Requests to the TTS endpoint (i.e., `../cognitiveservices/v1`) [require an authorization token](https://learn.microsoft.com/en-us/azure/cognitive-services/speech-service/rest-text-to-speech?tabs=streaming#request-headers-1) instead of the resource key. The token is generated every 10 minutes, and the resource key is needed to query it:

# TODO The token is valid for 10 minutes, but this function issues a request to generate a new one each time it is invoked. Try to make it more efficient **without** impacting the service (as the menus are generate using TTS).
#   Couple of ideas:
#   + caching (menu items rarely change; if they do, sometimes they can still be reused in the future so these can be re-used. See 20221102_0616)
get_token() {
  curl \
    --request POST \
    "https://westus2.api.cognitive.microsoft.com/sts/v1.0/issueToken" \
    --header "Content-type: application/x-www-form-urlencoded" \
    --header "Content-Length: 0" \
    --header "Ocp-Apim-Subscription-Key: $(cat azure-tts-key)" \
    # Use `-v` or `--verbose` if this request needs to be debugged. See `man curl` section `-v, --verbose` if more sophisticated tracing methods are needed.
}







# ----------------------------------------------------
# Everything below pertains to google at the moment,
# but using it as a template for azure.
# ----------------------------------------------------

# DESCRIPTION
# Generate TTS  audio from input text  ($1) with speed
# ($2) onto the standard input.

# EXAMPLE
# $ ./google-tts-wav.sh "Press 7, for Safeway." 0.87 > prompts/safeway.wav

# Note: All links below have been saved to web.archive.org on 2020/05/23.

# DEPENDENCIES
#
# + curl
# + jq
# + base64
#
# There is  a `shell.nix` in the  project, and running
# it   with  `nix-shell   ./shell.nix`  will   install
# everthing needed.


# Easiest way to install them is using Nix:
#
# nix-shell -p jq google-cloud-sdk curl
#
# ( `base64` has  always  been  available on the Linux
#   command line thus far.
# )

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
export GOOGLE_APPLICATION_CREDENTIALS="./IDEAlists-abf8168ce4cc.json"

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
# vim: set nowrap:
# vim: set softtabstop=2 shiftwidth=2 tabstop=2 expandtab:
