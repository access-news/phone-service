#!/bin/bash

sudo service freeswitch stop

# Save vanilla install folder
sudo mv /etc/freeswitch{,_old}

# Download and place secrets
source ./dl-secrets.bash \
  tr2-lua-conn-string:freeswitch/scripts/conn_string.lua \
  tr2-freeswitch-password:freeswitch/passwords.xml

sudo cp -rv ./freeswitch /etc/
sudo chown -R freeswitch:freeswitch /etc/freeswitch

# See rationale in the script itself
source ./lua-fixup.bash

sudo service freeswitch start

# TODO: connect to SignalWire
