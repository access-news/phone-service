#!/bin/bash

sudo service freeswitch stop

# Save vanilla install folder
sudo mv /etc/freeswitch{,_old}

# Download and place secrets
# source ./dl-secrets.bash \
#   tr2-passwords-xml:freeswitch/passwords.xml

sudo cp -rv ./freeswitch /etc/
sudo chown -R freeswitch:freeswitch /etc/freeswitch

# See rationale in the script itself
# source ./lua-fixup.bash

sudo service freeswitch start

echo -e "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\nFROM DEPLOY.BASH\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\nThe Azure Instance Metadata Service is either down or not working on my dev station, hence the downloaded secrets are invalid, and therefore this step is skipped.\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"

# TODO: connect to SignalWire
