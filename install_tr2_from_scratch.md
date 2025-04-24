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




[signalwire_token]: https://developer.signalwire.com/freeswitch/FreeSWITCH-Explained/Installation/how-to-create-a-personal-access-token/how-to-create-a-personal-access-token
[fs_debian_install]: https://developer.signalwire.com/freeswitch/FreeSWITCH-Explained/Installation/Linux/Debian_67240088/
[fs_vanilla]: https://developer.signalwire.com/freeswitch/FreeSWITCH-Explained/Installation/Linux/Vanilla-installation-files_27591294
[se_mod_erlang_event]: https://unix.stackexchange.com/questions/794112/why-does-a-successfulload-mod-erlang-event-throw-errors-and-has-its-c-node-eve/794113#794113
[se_mod_sofia]: https://unix.stackexchange.com/questions/794109/how-to-install-the-mod-sofia-and-mod-spandsp-freeswitch-modules-on-debian-12/794110#794110
[nix_installer]: https://determinate.systems/posts/determinate-nix-installer/
