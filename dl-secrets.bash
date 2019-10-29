#!/bin/bash

function elem {
  POS=$1
  PAIR=$2
  cut -d':' -f$POS <<<$PAIR
}

# input: [ "secret_name_in_vault:path_to_save" ]
#                              ^
#                              |
#                         pair delimiter
#
# example: "fs-password:freeswitch/passwords.xml lua-conn-string:freeswitch/scripts/conn.lua"
for pair in $@; do

  SECRET_NAME=$(elem 1 $pair)
  TARGET_PATH=$(elem 2 $pair)

  # `az keyvault secret  show` returns a JSON,
  # and `jq`  returns  its  `value`  attribute

  RAW_CONTENT=$(az keyvault secret show \
        --vault-name "sftb-vault"       \
        --name $SECRET_NAME             \
      | jq '.value'                     \
    )

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
