## `./freeswitch`

Contains all FreeSWITCH-related scripts and configuration files that get symlinked to their actual locations on the production server. For example,

```text
./freeswitch/dialplan/default.xml <- /etc/freeswitch/dialplan/default.xml
./freeswitch/scripts/auth.lua     <- /usr/share/freeswitch/scripts/auth.lua
```

## `./meta`

Out of date project descriptions.

## `./recorder`

Javascript audio recorder experiment pieced together from different sources.
