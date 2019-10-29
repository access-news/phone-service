#!/bin/bash

function elem {
  POS=$1
  PAIR=$2
  cut -d':' -f$POS <<<$PAIR
}

# Tutorial: Use a Linux VM system-assigned managed identity to access Azure Key Vault
# https://docs.microsoft.com/en-us/azure/active-directory/managed-identities-azure-resources/tutorial-linux-vm-access-nonaad
function get_secret {
  SECRET_NAME=$1
  TOKEN=$(               \
    curl                 \
      'http://169.254.169.254/metadata/identity/oauth2/token?api-version=2018-02-01&resource=https%3A%2F%2Fvault.azure.net' \
      -H Metadata:true   \
    | jq '.access_token' \
    | tr -d '"'          \
    )

  curl \
    https://sftb-vault.vault.azure.net/secrets/"${SECRET_NAME}"?api-version=2016-10-01 \
    -H "Authorization: Bearer ${TOKEN}" \
  | jq '.value'
}

# input:   [ "secret_name_in_vault:path/to/save" ]
# example: "fs-password:freeswitch/passwords.xml lua-conn-string:freeswitch/scripts/conn.lua"
for pair in $@; do

  SECRET_NAME=$(elem 1 $pair)
  TARGET_PATH=$(elem 2 $pair)
  RAW_CONTENT=$(get_secret $SECRET_NAME)

  # caveman debug
  # echo -e $RAW_CONTENT

  # `RAW_CONTENT` now  holds  a  double-quoted
  # string  with newlines  as  `\n` and  inner
  # double-quotes escaped with `\`.
  #
  # `echo   -e`  prints   a  string   so  that
  # meaningful escapes are  converted to their
  # meaning (e.g., `\n`  will become a literal
  # newline).
  #
  # The  inner `echo`  removes first  and last
  # characters  (i.e., the  quotes around  the
  # entire string).
  #
  # `tr -d '\\' deletes all `\` characters (so
  # that `\"` will become `"`).

  echo -e $(echo "${RAW_CONTENT:1:${#RAW_CONTENT}-2}") \
  | tr -d '\\'                     \
  > $TARGET_PATH

  # Restrict access to actual user
  chmod 600 $TARGET_PATH
done
