# 0. Repo layout

## `./freeswitch`

Contains all FreeSWITCH-related scripts and configuration files that get symlinked to their actual locations on the production server. For example,

```text
./freeswitch/dialplan/default.xml <- /etc/freeswitch/dialplan/default.xml
./freeswitch/scripts/auth.lua     <- /usr/share/freeswitch/scripts/auth.lua
```

See "1. TODOs" about better options.

## `./meta`

Out of date project descriptions.

## `./recorder`

Javascript audio recorder experiment pieced together from different sources.

# 1. TODOs

## 1.0 FreeSWITCH diaplan cleanup

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

### 1.1 Secret management (source control, deployment, etc.)

+ https://www.digitalocean.com/community/tutorials/an-introduction-to-managing-secrets-safely-with-version-control-systems
+ https://news.ycombinator.com/item?id=5178914
+ https://github.com/google/tink
+ https://johnresig.com/blog/keeping-passwords-in-source-control/
+ https://www.agwa.name/projects/git-crypt/
+ https://stackoverflow.com/questions/1436328/how-do-you-avoid-storing-passwords-in-version-control

I may also misunderstanding the "keep them in environment variables argument" because the [`systemd.exec` man page (section "Environment")](https://www.freedesktop.org/software/systemd/man/systemd.exec.html#Environment) states that

> Environment variables  are not suitable  for passing
> secrets (such  as passwords,  key material,  ...) to
> service processes.  Environment variables set  for a
> unit are  exposed to unprivileged clients  via D-Bus
> IPC, and generally not understood as being data that
> requires protection. Moreover, environment variables
> are  propagated  down  the process  tree,  including
> across  security boundaries  (such as  setuid/setgid
> executables), and hence might leak to processes that
> should not have access to the secret data.

, but does not give an alternative.

### 1.2 FreeSWITCH deployment
