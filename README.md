# Phone service to reach Access News content

## Scratch notes

```erlang
erl -eval 'cover:compile_directory("./outbound_erl").' -eval '{lofa, freeswitch@tr2} ! register_event_handler.' -run filog -run user_db -run content -sname access_news -setcookie OldTimeRadio
```
### Current `/etc/freeswitch/dialplan/default.xml` to start the `gen_statem` process on incoming calls

```xml
<?xml version="1.0" encoding="utf-8"?>
<include>
<context name="default">

    <extension name="SignalWire CONNECTORS incoming call">
    <!-- the number you assigned in your dashboard -->
    <condition field="destination_number" expression="^(\+19162510217)$">

        <!-- <action application="set" data="outside_call=true"/> -->
        <!-- <action application="export" data="RFC2822_DATE=${strftime(%a, %d %b %Y %T %z)}"/> -->
        <!-- <action application="answer"/> -->
        <!-- <action application="lua" data="main.lua"/> -->
        <!-- <action application="erlang" data="call_control:start access_news@tr2"/> -->

        <action application="set" data="playback_terminators=none"/>
        <action application="erlang" data="ivr:start access_news@tr2"/>

    </condition>
    </extension>
</context>
</include>
```

### TODOs

1. state diagram

## 0. Layout

```text
freeswitch/
|
|-- autoload_configs/
|   |-- modules.conf.xml
|   |-- pre_load_modules.conf.xml
|   `-- ( ... and whole bunch  of  actual )
|       ( config files, included verbatim )
|       ( from vanilla install.           )
|
|-- dialplan/
|   `-- default.xml
|
|-- lang/ ===================> FreeSWITCH phrases
|   `-- en/
|       |-- en.xml
|       `-- tr2.xml
|
|-- scripts/
|   |-- db_queries.lua
|   |-- ivr.lua
|   |-- login.lua
|   |-- main.lua
|   `-- utility_functions.lua
|
|-- (!) tls/
|
|-- README_IMPORTANT.txt
|-- freeswitch.xml
|-- mime.types
|-- (!) passwords.xml
`-- vars.xml
```

`tls/` and `passwords.xml` is not in the repo as the
store  sensitive data.  See  section "1.3  Secrets".
(Hopefully didn't not forget anything else...)

This list has been culled  from the list provided by
the vanilla install (see
[`conf/vanilla`](https://github.com/signalwire/freeswitch/tree/master/conf/vanilla)
in  the  FreeSWITCH  repo,  but  `conf/`  has  other
predefined  configurations.   Will  link   from  the
FreeSWITCH wiki, but the configuration pages (
[1](https://freeswitch.org/confluence/display/FREESWITCH/Default+Configuration),
[2](https://freeswitch.org/confluence/display/FREESWITCH/Configuring+FreeSWITCH),
[3](https://freeswitch.org/confluence/display/FREESWITCH/Vanilla+installation+files)
) need to be consolidated first.

## 1. Deployment

See TODO [1.2 FreeSWITCH deployment](#user-content-12-freeswitch-deployment) about better options.

### 1.1 Install FreeSWITCH

Follow installation instructions on the FreeSWITCH wiki.
E.g. [Debian 9 instructions](https://freeswitch.org/confluence/display/FREESWITCH/Debian+9+Stretch)

#### 1.1.1 FreeSWITCH sounds (optional)

Basic sounds should be installed during a vanilla install (at `/usr/share/freeswitch/sounds` on Debian 9), but, just in case, here are all the sounds:

https://github.com/access-news/freeswitch-sounds

### 1.2 Clone this repo

```bash
git clone https://github.com/access-news/phone-service.git ~/clones/phone-service
```

### 1.3 Deploy

Run `deploy.bash`.

It will
1. stop FreeSWITCH,
2. rename `/etc/freeswitch` to `/etc/freeswitch_old`,
3. download sensitive files (the Lua connection string,
   and `passwords.xml`,  see notes below)  to specified
   paths (using `dl-secrets.bash`),
4. copy   `./freeswitch`   into  `/etc`,   and   change
   ownership to `freeswitch`,
5. call `lua-fixup.bash`,
6. and restart the service.

#### Note on `passwords.xml`

Following the instructions in the FreeSWITCH wiki,
[Configuring FreeSWITCH](https://freeswitch.org/confluence/display/FREESWITCH/Configuring+FreeSWITCH),
[Security advice](https://freeswitch.org/confluence/display/FREESWITCH/Configuring+FreeSWITCH#ConfiguringFreeSWITCH-Securityadvice).

#### Note on the Lua connection string

The general format:

```lua
local c = {}

c.conn_string =
  "pgsql://hostaddr= <local or public IP"    ..
  " dbname=<database>"                       ..
  " user=<database-username>"                ..
  " password=<password>"                     ..
  " options='-c client_min_messages=NOTICE'" ..
  " application_name='freeswitch'"

return c
```

#### Note on the `tls/` directory

Currently not  included in the Azure  vault, because
each FreeSWITCH installation provided these files so
far.

### 1.4 Connect to telco provider

In this case, SignalWire. Follow the steps at [mod_signalwire](https://freeswitch.org/confluence/display/FREESWITCH/mod_signalwire).

<sup>**Don't forget Step 4!** (That is, assigning a connector/integration to the purchased number, otherwise incoming calls will be dropped with busy signal.)</sup>

## 2. TODOs

- [X] 1.0 FreeSWITCH diaplan cleanup
- [X] 1.1 Secret management (source control, deployment, etc.)
- [X] 1.2 FreeSWITCH deployment
- [X] 1.3 FreeSWITCH configuration cleanup
- [ ] 1.4 Plan for archiving old media
- [ ] 1.5 Figure out dialplans
- [ ] 1.6 Clean up `autoload_configs`
- [ ] 1.7 IVR: implement "leave a message" option
- [ ] 1.8 Per user favourites
- [ ] 1.9 I18n support
- [ ] 1.10 Stats
- [ ] 1.11 Logs
- [ ] 1.12 Create submenus automatically to play recordings
- [ ] 1.13 Save user progress on crash

### [DONE] 1.0 FreeSWITCH diaplan cleanup

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

### 2.1 Secret management (source control, deployment, etc.)

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

**UPDATE** (2019-07-10_1022):
Based on the recommendations in [this article](https://embeddedartistry.com/blog/2018/3/15/safely-storing-secrets-in-git), will use `git-crypt`.

**UPDATE** (2019-07-10_1207):
Apparently I need to learn some cryptography basics, and how to manage keys (it would be bad to loose the keys, and no one would be able to decrypt the project files...).

+ https://www.devdungeon.com/content/gpg-tutorial
+ https://en.wikipedia.org/wiki/Key_management#Key_management_system
+ https://learn.hashicorp.com/vault/
+ https://info.townsendsecurity.com/definitive-guide-to-encryption-key-management-fundamentals

### 2.2 FreeSWITCH deployment

Move to NixOps.

Right now files are symlinked from the the `./freeswitch` folder.

Another option would be to edit the `/lib/systemd/system/freeswitch.service` (found it via `sudo systemctl status freeswitch.service`) and re-define the default folders.
   > bash> fs_cli -x 'global_getvar'| grep _dir
   >
   >  base_dir       = /usr
   >  recordings_dir = /var/lib/freeswitch/recordings
   >  sounds_dir     = /usr/share/freeswitch/sounds
   >  conf_dir       = /etc/freeswitch
   >  log_dir        = /var/log/freeswitch
   >  run_dir        = /var/run/freeswitch
   >  db_dir         = /var/lib/freeswitch/db
   >  mod_dir        = /usr/lib/freeswitch/mod
   >  htdocs_dir     = /usr/share/freeswitch/htdocs
   >  script_dir     = /usr/share/freeswitch/scripts
   >  temp_dir       = /tmp
   >  grammar_dir    = /usr/share/freeswitch/grammar
   >  fonts_dir      = /usr/share/freeswitch/fonts
   >  images_dir     = /var/lib/freeswitch/images
   >  certs_dir      = /etc/freeswitch/tls
   >  storage_dir    = /var/lib/freeswitch/storage
   >  cache_dir      = /var/cache/freeswitch
   >  data_dir       = /usr/share/freeswitch
   >  localstate_dir = /var/lib/freeswitch
   > ```

See also
[Command Line Switches](https://freeswitch.org/confluence/display/FREESWITCH/Command+Line+Switches)
in the FreeSWITCH wiki.

### [DONE] 2.3 FreeSWITCH configuration cleanup

Same as 1.0 but different section:

```xml
  <section name="configuration" description="Various Configuration">
    <X-PRE-PROCESS cmd="include" data="autoload_configs/*.xml"/>
  </section>
```

### 2.4 Plan for archiving old media

Content, that is many months (or years) old, should be moved to a cheaper storage class. See Google's coldline and nearline storage classes [here](https://cloud.google.com/storage/docs/storage-classes#comparison_of_storage_classes), for example.

QUESTION: How to update media file locations in the DB?

### 2.5 Figure out dialplans

With 1.0 done, it would be a good time to figure out the relationship between `public`, `features`, `skinny_profiles`, `default` dialplans. The SignalWire in-memory config also makes things a bit more confusing, and it uses the `default` one out of the box.

### 2.6 Clean up `autoload_configs`

`/etc/freeswitch/autoload_configs` has 87 files in it right now; pretty sure that only a fraction of them are being used.

### 2.7 IVR: implement "leave a message" option

Implement leaving a message (by pressing 0, for example).

Right now, the text says to call the main Access News number.

How would that be sent  to admins? For example email
the audio  as an attachment with  a transcription as
email body.

### 2.8 Per user favourites

### 2.9 I18n support

Add a menu to be able to change languages, and each submenu option will announce itself in the language supported. See [this note](https://github.com/toraritte/knowledge-gaps/blob/master/telephony/freeswitch.md#2-speech-phrase-management-in-docs-vs-language-management-in-demo-config) on the confusion with language suport in FreeSWITCH.

Maybe it isn't even an issue though, if a cloud TTS can be set up.

### 2.10 Stats

### 2.11 Logs

### 2.12 Create submenus automatically to play recordings

Probably the easiest way is [session:setInputCallback](https://freeswitch.org/confluence/display/FREESWITCH/Lua+API+Reference#LuaAPIReference-session:setInputCallback), but see [session:sayPhrase](https://freeswitch.org/confluence/display/FREESWITCH/Lua+API+Reference#LuaAPIReference-session:sayPhrase) for more examples.

Is there an easier way?

### 2.13 Save user progress on crash

When the user calls again, they can choose to continue to listen from the same spot as before.
