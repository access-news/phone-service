# 0. Repo layout

## `./freeswitch`

Contains all FreeSWITCH-related scripts and configuration files that get symlinked to their actual locations on the production server. For example,

```text
/etc/freeswitch/dialplan/default.xml   -> [this-repo]/freeswitch/dialplan/default.xml
/etc/freeswitch/freeswitch.xml         -> [this-repo]/freeswitch/freeswitch.xml
/etc/freeswitch/lang/                  -> [this-repo]/freeswitch/phrases/lang/

/usr/share/freeswitch/scripts/         -> [this-repo]/freeswitch/scripts/
```

Commands used:

```text
$ sudo -u freeswitch ln -s [full_path_to_repo]/freeswitch/dialplan/default.xml /etc/freeswitch/dialplan/default.xml
$ sudo -u freeswitch ln -s [full_path_to_repo]/freeswitch/freeswitch.xml /etc/freeswitch/freeswitch.xml
$ sudo -u freeswitch ln -s [full_path_to_repo]/freeswitch/phrases/lang/ /etc/freeswitch/lang
$ sudo -u freeswitch ln -s [full_path_to_repo]/freeswitch/scripts/ /usr/share/freeswitch/scripts
```

See [1.2 FreeSWITCH deployment](#user-content-12-freeswitch-deployment) about better options.

## `./meta`

Out of date project descriptions.

## `./recorder`

Javascript audio recorder experiment pieced together from different sources.

# 1. TODOs

## 1.0 FreeSWITCH diaplan cleanup [STATUS: DONE (but see question)]

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

> **QUESTION**:
> Warning: Locale seems not configured
> It   would   be   a   good  time   to   figure   out
> the   relationship  between   `public`,  `features`,
> `skinny_profiles`,    `default`    dialplans.    The
> SignalWire in-memory config also  makes things a bit
> more confusing, and it uses the `default` one out of
> the box.

## 1.1 Secret management (source control, deployment, etc.)

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

## 1.2 FreeSWITCH deployment

Right now files are symlinked from the the `./freeswitch` folder.

Another option would be to edit the `/lib/systemd/system/freeswitch.service` (found it via `sudo systemctl status freeswitch.service`) and re-define the default folders.

 + [`systemd.service` man page](https://www.freedesktop.org/software/systemd/man/systemd.service.html)
 + [Command Line Switches](https://freeswitch.org/confluence/display/FREESWITCH/Command+Line+Switches) in the FreeSWITCH docs

   > ```text
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

## [DONE] 1.3 FreeSWITCH configuration cleanup

Same as 1.0 but different section:

```xml
  <section name="configuration" description="Various Configuration">
    <X-PRE-PROCESS cmd="include" data="autoload_configs/*.xml"/>
  </section>
```

`/etc/freeswitch/autoload_configs` has 87 files in it right now; pretty sure that only a fraction of them are being used.

## 1.3 Plan for archiving old media

Content, that is many months (or years) old, should be moved to a cheaper storage class. See Google's coldline and nearline storage classes [here](https://cloud.google.com/storage/docs/storage-classes#comparison_of_storage_classes), for example.

QUESTION: How to update media file locations in the DB?
