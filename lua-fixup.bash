#!/bin/bash

sudo mkdir /usr/share/lua
sudo chown -R freeswitch:freeswitch /usr/share/lua
sudo -u freeswitch ln -s /etc/freeswitch/scripts/ /usr/share/lua/5.2

# ----------------------------------------------------------------------
# WHY IS THIS NEEDED?
# ----------------------------------------------------------------------

# All Lua modules and script are located in
# [`freeswitch/scripts`](./freeswitch/scripts),
# but  couldn't   get  them  to  run.   As  the  error
# message states, the Lua  5.2 interpreter, built into
# Freeswitch, is looking in these paths:

# ```lua
#         no file '/usr/local/share/lua/5.2/conn_string.lua'
#         no file '/usr/local/share/lua/5.2/conn_string/init.lua'
#         no file '/usr/local/lib/lua/5.2/conn_string.lua'
#         no file '/usr/local/lib/lua/5.2/conn_string/init.lua'
#         no file '/usr/share/lua/5.2/conn_string.lua'
#         no file '/usr/share/lua/5.2/conn_string/init.lua'
#         no file './conn_string.lua'
#         no file '/usr/local/lib/lua/5.2/conn_string.so'
#         no file '/usr/lib/x86_64-linux-gnu/lua/5.2/conn_string.so'
#         no file '/usr/lib/lua/5.2/conn_string.so'
#         no file '/usr/local/lib/lua/5.2/loadall.so'
#         no file './conn_string.so'
# ```

# There is an obscure
# [Freeswitch wiki entry](https://freeswitch.org/confluence/display/FREESWITCH/Third+Party+Libraries#ThirdPartyLibraries-WheretoputthirdpartyLuascripts/modules)
# on how to change this, and one could read up on
# [`package.path` file or how Lua modules work](https://www.lua.org/manual/5.3/manual.html#6.3),
# but it  was easier  to just point  a symlink  to the
# repo's `freeswitch/scripts` directory.

# There  is   a  global  FreeSWITCH   variable  called
# `script_dir`   that,  on    Debian   9,  points   to
# `/usr/share/freeswitch/scripts`, but it doesn't even
# show up  in the  errors above.  From the  very first
# FreeSWITCH setup (note the timestamps):

# $ ll /usr/share/freeswitch/scripts
# 2 lrwxrwxrwx 1 freeswitch freeswitch 46 Jul 10 15:26 /usr/share/freeswitch/scripts -> /home/toraritte/clones/TR2/freeswitch/scripts//

# $ ll /usr/share/lua/
# . total 8
# f drwxr-xr-x   2 freeswitch freeswitch 4096 Jul 10 17:42 ./
#   drwxr-xr-x 104 root       root       4096 Sep  6 22:59 ../
#     lrwxrwxrwx   1 freeswitch freeswitch   46 Jul 10 17:42 5.2 -> /home/toraritte/clones/TR2/freeswitch/scripts//
