# TR2

## 0. Repo layout

### 0.0 `./freeswitch`

Contains all FreeSWITCH-related scripts and configuration files that get symlinked to their actual locations on the production server. For example,

```text
/etc/freeswitch/dialplan/default.xml   -> [this-repo]/freeswitch/dialplan/default.xml
/etc/freeswitch/freeswitch.xml         -> [this-repo]/freeswitch/freeswitch.xml
/etc/freeswitch/lang/                  -> [this-repo]/freeswitch/phrases/lang/

/usr/share/freeswitch/scripts/         -> [this-repo]/freeswitch/scripts/
/usr/share/lua/5.2/                    -> [this-repo]/freeswitch/scripts/

```

Commands used:

```text
$ sudo -u freeswitch ln -s [full_path_to_repo]/freeswitch/dialplan/default.xml /etc/freeswitch/dialplan/default.xml
$ sudo -u freeswitch ln -s [full_path_to_repo]/freeswitch/freeswitch.xml /etc/freeswitch/freeswitch.xml
$ sudo -u freeswitch ln -s [full_path_to_repo]/freeswitch/phrases/lang/ /etc/freeswitch/lang
$ sudo -u freeswitch ln -s [full_path_to_repo]/freeswitch/scripts/ /usr/share/freeswitch/scripts

# see note in 0.0.0
$ sudo mkdir /usr/share/lua
$ sudo chown -R freeswitch:freeswitch /usr/share/lua
$ sudo -u freeswitch ln -s [full_path_to_repo]/freeswitch/scripts/ /usr/share/lua/5.2
```

See TODO [1.2 FreeSWITCH deployment](#user-content-12-freeswitch-deployment) about better options.

#### 0.0.0 Note on Lua module locations (and why `require` failed)

All Lua modules and script are located in [`freeswitch/scripts`](./freeswitch/scripts).

As the error message states, the Lua 5.2 interpreter, built into Freeswitch, is looking in these paths:

```lua
        no file '/usr/local/share/lua/5.2/conn_string.lua'
        no file '/usr/local/share/lua/5.2/conn_string/init.lua'
        no file '/usr/local/lib/lua/5.2/conn_string.lua'
        no file '/usr/local/lib/lua/5.2/conn_string/init.lua'
        no file '/usr/share/lua/5.2/conn_string.lua'
        no file '/usr/share/lua/5.2/conn_string/init.lua'
        no file './conn_string.lua'
        no file '/usr/local/lib/lua/5.2/conn_string.so'
        no file '/usr/lib/x86_64-linux-gnu/lua/5.2/conn_string.so'
        no file '/usr/lib/lua/5.2/conn_string.so'
        no file '/usr/local/lib/lua/5.2/loadall.so'
        no file './conn_string.so'
```

There is an obscure [Freeswitch wiki entry](ttps://freeswitch.org/confluence/display/FREESWITCH/Third+Party+Libraries#ThirdPartyLibraries-WheretoputthirdpartyLuascripts/modules) on how to change this, and one could read up on [`package.path` file or how Lua modules work](https://www.lua.org/manual/5.3/manual.html#6.3), but it was easier to just point a symlink to the `scripts` directory.

#### 0.0.1 DB connection string template for Lua

The contents of the `conn_string.lua` module:

```lua
local c = {}

c.conn_string =
  "pgsql://hostaddr=1.2.3.4"                 ..
  " dbname=db"                               ..
  " user=db_user"                            ..
  " password=db_password"                    ..
  " options='-c client_min_messages=NOTICE'" ..
  " application_name='freeswitch'"

return c
```


### 0.1 `./meta`

Out of date project descriptions.

### 0.2 `./recorder`

Javascript audio recorder experiment pieced together from different sources.

## 1. TODOs

- [X] 1.0 FreeSWITCH diaplan cleanup
- [ ] 1.1 Secret management (source control, deployment, etc.)
- [ ] 1.2 FreeSWITCH deployment
- [X] 1.3 FreeSWITCH configuration cleanup
- [ ] 1.4 Plan for archiving old media
- [ ] 1.5 Figure out dialplans
- [ ] 1.6 Clean up `autoload_configs`
- [ ] 1.7 IVR: implement "leave a message" option
- [ ] 1.8 Per user favourites
- [ ] 1.9 I18n support

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

**UPDATE** (2019-07-10_1022):
Based on the recommendations in [this article](https://embeddedartistry.com/blog/2018/3/15/safely-storing-secrets-in-git), will use `git-crypt`.

**UPDATE** (2019-07-10_1207):
Apparently I need to learn some cryptography basics, and how to manage keys (it would be bad to loose the keys, and no one would be able to decrypt the project files...).

+ https://www.devdungeon.com/content/gpg-tutorial
+ https://en.wikipedia.org/wiki/Key_management#Key_management_system
+ https://learn.hashicorp.com/vault/
+ https://info.townsendsecurity.com/definitive-guide-to-encryption-key-management-fundamentals

### 1.2 FreeSWITCH deployment

Right now files are symlinked from the the `./freeswitch` folder.

Another option would be to edit the `/lib/systemd/system/freeswitch.service` (found it via `sudo systemctl status freeswitch.service`) and re-define the default folders.
   > bash> fs_cli -x 'global_getvar'| grep _dir
   >
   >  base_dir=/usr
   >  recordings_dir=/var/lib/freeswitch/recordings
   >  sounds_dir=/usr/share/freeswitch/sounds
   >  conf_dir=/etc/freeswitch
   >  log_dir=/var/log/freeswitch
   >  run_dir=/var/run/freeswitch
   >  db_dir=/var/lib/freeswitch/db
   >  mod_dir=/usr/lib/freeswitch/mod
   >  htdocs_dir=/usr/share/freeswitch/htdocs
   >  script_dir=/usr/share/freeswitch/scripts
   >  temp_dir=/tmp
   >  grammar_dir=/usr/share/freeswitch/grammar
   >  fonts_dir=/usr/share/freeswitch/fonts
   >  images_dir=/var/lib/freeswitch/images
   >  certs_dir=/etc/freeswitch/tls
   >  storage_dir=/var/lib/freeswitch/storage
   >  cache_dir=/var/cache/freeswitch
   >  data_dir=/usr/share/freeswitch
   >  localstate_dir=/var/lib/freeswitch
   > ```

### [DONE] 1.3 FreeSWITCH configuration cleanup

Same as 1.0 but different section:

```xml
  <section name="configuration" description="Various Configuration">
    <X-PRE-PROCESS cmd="include" data="autoload_configs/*.xml"/>
  </section>
```

### 1.4 Plan for archiving old media

Content, that is many months (or years) old, should be moved to a cheaper storage class. See Google's coldline and nearline storage classes [here](https://cloud.google.com/storage/docs/storage-classes#comparison_of_storage_classes), for example.

QUESTION: How to update media file locations in the DB?

### 1.5 Figure out dialplans

With 1.0 done, it would be a good time to figure out the relationship between `public`, `features`, `skinny_profiles`, `default` dialplans. The SignalWire in-memory config also makes things a bit more confusing, and it uses the `default` one out of the box.

### 1.6 Clean up `autoload_configs`

`/etc/freeswitch/autoload_configs` has 87 files in it right now; pretty sure that only a fraction of them are being used.

### 1.7 IVR: implement "leave a message" option

Implement leaving a message (by pressing 0, for example).

Right now, the text says to call the main Access News number.

How would that be sent  to admins? For example email
the audio  as an attachment with  a transcription as
email body.

### 1.8 Per user favourites

### 1.9 I18n support

Add a menu to be able to change languages, and each submenu option will announce itself in the language supported. See [this note](https://github.com/toraritte/knowledge-gaps/blob/master/telephony/freeswitch.md#2-speech-phrase-management-in-docs-vs-language-management-in-demo-config) on the confusion with language suport in FreeSWITCH.

Maybe it isn't even an issue though, if a cloud TTS can be set up.
