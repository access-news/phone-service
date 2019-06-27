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

## TODOs

### FreeSWITCH diaplan cleanup

`/etc/freeswitch/freeswitch.xml`:

```xml
  <section name="dialplan" description="Regex/XML Dialplan">
    <X-PRE-PROCESS cmd="include" data="dialplan/*.xml"/>
  </section>
```

and the default FreeSWITCH installation comes with the following `dialplan` directory:

```text
freeswitch/
| autoload_configs/
| chatplan/
| dialplan/
| | default/
| | public/
| | skinny-patterns/
| | default.xml@	 --> /home/toraritte/clones/TR2/freeswitch/dialplan/default.xml
| | default_moved.xml
| | default_old.xml
| | features.xml
| | public.xml
| | skinny-patterns.xml
```

It would be a good time to figure out the relationship between `public`, `features`, `skinny_profiles`, `default` dialplans. The SignalWire in-memory config also makes things a bit more confusing, and it uses the `default` one out of the box.
