# Setting up TR2

## Attempt 1: Set up TR2 from Debian package (failed)

Didn't work out, because call was never picked up (same as with attempt 2 below).

> IMPORTANT  
> The attempt 1 and attempt 2 steps are missing one: update the paths in `content.erl` and `menus.erl`, and update the FreeSWITCH Erlang C-node's name in `fs.erl`.

0.1 Create a Debian 12 instance

0.2 Install Nix

    Use [Determinate System's Nix installer][nix_installer]:
    ```
    curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
    ```

0.3 Install Erlang
    ```
    sudo apt install erlang
    ```
    This is needed for `mod_erlang_event`, otherwise it won't load properly as it needs the Erlang headers for `epmd`. See [Stackexchange thread][se_mod_erlang_event].

1. [Get SignalWire access token][signalwire_token]

2. Become `root`
   ```
   sudo su
   ```

3. Install FreeSWITCH

   Based on the FreeSWITCH doc's [Debian installation instructions][fs_debian_install]

   ```
   TOKEN=YOURSIGNALWIRETOKEN
   
   apt update && apt install -y curl
   curl -sSL https://freeswitch.org/fsget | bash -s $TOKEN release install
   ```

   + The [vanilla config][fs_vanilla] will be installed into `/etc/freeswitch` (if this directory does not exist beforehand).

   + `freeswitch` user and group will be created.

   > ASIDE
   > The docs page above will warn that `spandsp` and `sofia-sip` has been split out from the FreeSWITCH repo, but that can be ignored safely: `mod_spandsp` and `mod_sofia` (along with `mod_signalwire`) are installed by default. See [Stackexchange thread][se_mod_sofia].
   >
   > `mod_erlang_event` is also installed, but not loaded; it is enabled in the TR2 config though.

4. Add TR2 configuration

   > ASIDE
   > The [Debian install doc][fs_debian_install] recommends adding the configuration **before** installing FreeSWITCH, but then one would have to create everything from scratch, along with the `freeswitch` group and user.
---

It gets fuzzy from here but I'm trying this route (no numbering because the order may change and ordering is a little more than a suggestion at this point...):

* copy the config from the phone-service repo into /etc/ (if there is already an /etc/freeswitch, maybe rename it with postfix `_old`)

  + change hostname in dialplan/default.xml (it shows `tr2`, but it may be something else; see `hostname` command)``

* save `password.xml` from the Azure vault (follow the breadcrumbs of dl-secrets.bash and deploy.bash)

* connect to SignalWire`


[signalwire_token]: https://developer.signalwire.com/freeswitch/FreeSWITCH-Explained/Installation/how-to-create-a-personal-access-token/how-to-create-a-personal-access-token
[fs_debian_install]: https://developer.signalwire.com/freeswitch/FreeSWITCH-Explained/Installation/Linux/Debian_67240088/
[fs_vanilla]: https://developer.signalwire.com/freeswitch/FreeSWITCH-Explained/Installation/Linux/Vanilla-installation-files_27591294
[se_mod_erlang_event]: https://unix.stackexchange.com/questions/794112/why-does-a-successfulload-mod-erlang-event-throw-errors-and-has-its-c-node-eve/794113#794113
[se_mod_sofia]: https://unix.stackexchange.com/questions/794109/how-to-install-the-mod-sofia-and-mod-spandsp-freeswitch-modules-on-debian-12/794110#794110
[nix_installer]: https://determinate.systems/posts/determinate-nix-installer/

## Attempt 2: Erlang latest + FreeSWITCH from source with patch

Didn't work out. Same issue as with attempt 1: the call can't be picked up.

> IMPORTANT  
> The attempt 1 and attempt 2 steps are missing one: update the paths in `content.erl` and `menus.erl`, and update the FreeSWITCH Erlang C-node's name in `fs.erl`.

Giving this another shot, because the latest Erlang and FreeSWITCH have linking issues, so trying compiling FreeSWITCH from source.

* INSTALL NIX
  ==========================================================
  ```
  curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
  ```

  If Nix commands not in path, try
  ```
  . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
  ```
  or calling them directly from `/nix/var/nix/profiles/default/bin/`.

* SET UP BASIC TOOLS
  ==========================================================
  It looks like Nix shells mess up Debian 12's locales, so this is what I'm doing for now:

  * One local terminal window to the server with my opinionated shell:
    ```
    source <(curl https://raw.githubusercontent.com/toraritte/shell.nixes/main/run.sh) -n 23.11
    ```

  * Another terminal window with tmux open. Install basic tools with APT:
    ```
    sudo apt update
    sudo apt install tmux htop git
    # add more when needed
    ```

* INSTALL ERLANG
  ==========================================================
  Settling for `sudo apt install erlang` for now.
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!                                                    !!!
  !!! If this attempt fails, then install version 22 ... !!!
  !!! somehow. (The true solution is to Nix, but right   !!!
  !!! now I'm just get this to work somehow.)            !!!
  !!!                                                    !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

* COMPILE FreeSWITCH
  ==========================================================
  Following the [Compiling Release Branch (production)][fs-compile-branch] guide on Debian.

  ```
  sudo su
  TOKEN=pat_ehr9aonkYYKkoqXDzwKvUCjn
  cd /usr/src

  # continue with the steps from the guide until:
  ```

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!                                                    !!!
  !!! Before running `./bootstrap.sh -j`, uncomment      !!!
  !!! `mod_erlang_event` in `modules.conf`, e.g., with:  !!!
  !!!                                                    !!!
  !!! vim modules.conf                                   !!!
  !!!                                                    !!!
  !!! In general, this is [how to install FreeSWITCH mods][fs-install-mods].!!!
  !!!                                                    !!!
  !!!                                                    !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  Saving the `./configure` output here just in case:
  ```
  -------------------------- FreeSWITCH configuration --------------------------
  
    Locations:
  
        prefix:          /usr/local/freeswitch
        exec_prefix:     /usr/local/freeswitch
        bindir:          ${exec_prefix}/bin
        confdir:         /usr/local/freeswitch/conf
        libdir:          ${exec_prefix}/lib
        datadir:         /usr/local/freeswitch
        localstatedir:   /usr/local/freeswitch
        includedir:      /usr/local/freeswitch/include/freeswitch
  
        certsdir:        /usr/local/freeswitch/certs
        dbdir:           /usr/local/freeswitch/db
        grammardir:      /usr/local/freeswitch/grammar
        htdocsdir:       /usr/local/freeswitch/htdocs
        fontsdir:        /usr/local/freeswitch/fonts
        logfiledir:      /usr/local/freeswitch/log
        modulesdir:      /usr/local/freeswitch/mod
        pkgconfigdir:    ${exec_prefix}/lib/pkgconfig
        recordingsdir:   /usr/local/freeswitch/recordings
        imagesdir:       /usr/local/freeswitch/images
        runtimedir:      /usr/local/freeswitch/run
        scriptdir:       /usr/local/freeswitch/scripts
        soundsdir:       /usr/local/freeswitch/sounds
        storagedir:      /usr/local/freeswitch/storage
        cachedir:        /usr/local/freeswitch/cache
  
  ------------------------------------------------------------------------------
  ```

* CREATE FREESWITCH GROUP AND USER
  ==========================================================
  Follow [this section][fs-post-install-group-user] from the FreeSWITCH docs "Post-installation" guide.

  ```
  # create user 'freeswitch'
  # add it to group 'freeswitch'
  # change owner and group of the freeswitch installation
  sudo su
  pushd /usr/local
  groupadd freeswitch
  adduser --quiet --system --home /usr/local/freeswitch --gecos "FreeSWITCH open source softswitch" --ingroup freeswitch freeswitch --disabled-password
  chown -R freeswitch:freeswitch /usr/local/freeswitch/ 
  chmod -R ug=rwX,o= /usr/local/freeswitch/
  chmod -R u=rwx,g=rx /usr/local/freeswitch/bin/*
  ```

* OTHER POST-INSTALL STUFF
  ==========================================================
  
  * SYSTEMD - Don't worry with it. The supplied file in the source doesn't seem to work, so just use:
    ```
    sudo -u freeswitch  /usr/local/freeswitch/bin/freeswitch -nc
    sudo -u freeswitch  /usr/local/freeswitch/bin/fs_cli

    sudo -u freeswitch  /usr/local/freeswitch/bin/freeswitch -stop
    sudo -u freeswitch  /usr/local/freeswitch/bin/freeswitch -help
    ```

* COPY OVER THE `phone-service` AND `/etc/freeswitch` DIRECTORIES
  ==========================================================
  that are used in production from ... wherever they may be at the time. (Maybe [by using MC][mc-ssh-copy].)

* ...  AND "INSTALL" THE PRODUCTION CONFIGURATION
  ==========================================================
  !!!
  !!! Ditch `password.xml` by appending its contents to `vars.xml`.
  !!! (It doesn't work. Not sure how it was working in prod.)
  !!!
  ```
  sudo cp -rv ~/.../freeswitch-prod /etc/
  sudo chown -R freeswitch:freeswitch  <fs_conf_dir>
  sudo chmod 600 /etc/freeswitch/password.xml
  ```
  
  `fs_conf_dir` will probably be:
  + `/usr/local/freeswitch/conf` - if compiled from source
  + `/etc/freeswitch` - if installed from package

* INSTALL ERLANG
  ==========================================================
* INSTALL ERLANG
  ==========================================================
* INSTALL ERLANG
  ==========================================================

[mc-ssh-copy]: https://unix.stackexchange.com/questions/794062/how-to-connect-and-browse-files-of-remote-server-via-midnight-commanders-shell/794063#794063
[fs-post-install-group-user]: https://developer.signalwire.com/freeswitch/FreeSWITCH-Explained/Installation/Linux/Deprecated-Installation-Instructions/Debian-Post-Install-Tasks_13172868/#set-owner-and-permissions
[fs-compile-branch]: https://developer.signalwire.com/freeswitch/FreeSWITCH-Explained/Installation/Linux/Debian_67240088/#compiling-release-branch-production
[fs-install-mods]: https://unix.stackexchange.com/questions/794219/how-compile-and-install-module-mod-erlang-event-if-freeswitch-was-compiled-fro

## Attempt 2: Everything from source (Erlang 22 + FreeSWITCH 1.10.1)

Compiling everything from source because no packages are available anymore for these...

* INSTALL NIX
  ==========================================================
  ```
  curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
  ```

  If Nix commands not in path, try
  ```
  . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
  ```
  or calling them directly from `/nix/var/nix/profiles/default/bin/`.

* SET UP BASIC TOOLS
  ==========================================================
  It looks like Nix shells mess up Debian 12's locales, so this is what I'm doing for now:

  * One local terminal window to the server with my opinionated shell:
    ```
    source <(curl https://raw.githubusercontent.com/toraritte/shell.nixes/main/run.sh) -n 23.11
    ```

  * Another terminal window with tmux open. Install basic tools with APT:
    ```
    sudo apt update
    sudo apt install tmux htop git
    # add more when needed
    ```

* COMPILE FreeSWITCH
  ==========================================================
  Following the [Compiling Release Branch (production)][fs-compile-branch] guide on Debian - more or less, becuase this is version v1.10.1.

  ```
  sudo su
  TOKEN=pat_ehr9aonkYYKkoqXDzwKvUCjn

  curl -sSL https://freeswitch.org/fsget | bash -s $TOKEN
   
  # Install dependencies required for the build
  apt-get build-dep freeswitch
   
  # then let's get the source. Use the -b flag to get a specific branch
  cd /usr/src/
  git clone --depth 1 --branch v1.10.1 https://github.com/signalwire/freeswitch.git
  cd freeswitch
   
  # ... and do the build
  ./bootstrap.sh -j
  ./configure
  make
  make install



  # continue with the steps from the guide until:
  ```

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!                                                    !!!
  !!! Before running `./bootstrap.sh -j`, uncomment      !!!
  !!! `mod_erlang_event` in `modules.conf`, e.g., with:  !!!
  !!!                                                    !!!
  !!! vim modules.conf                                   !!!
  !!!                                                    !!!
  !!! In general, this is [how to install FreeSWITCH mods][fs-install-mods].!!!
  !!!                                                    !!!
  !!!                                                    !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* COMPILE FreeSWITCH
  ==========================================================
  Following the [Compiling Release Branch (production)][fs-compile-branch] guide on Debian.

  ```
  sudo su
  TOKEN=pat_ehr9aonkYYKkoqXDzwKvUCjn
  cd /usr/src

  # continue with the steps from the guide until:
  ```

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!                                                    !!!
  !!! Before running `./bootstrap.sh -j`, uncomment      !!!
  !!! `mod_erlang_event` in `modules.conf`, e.g., with:  !!!
  !!!                                                    !!!
  !!! vim modules.conf                                   !!!
  !!!                                                    !!!
  !!! In general, this is [how to install FreeSWITCH mods][fs-install-mods].!!!
  !!!                                                    !!!
  !!!                                                    !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

* INSTALL ERLANG 22
  =========================================================

  ```
  sudo su
  cd /usr/src
  cd otp

  ```



* INSTALL ERLANG
  ==========================================================
* INSTALL ERLANG
  ==========================================================

