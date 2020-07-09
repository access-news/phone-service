####################################################################
# Importing a cloned Nixpkgs repo  (from my home directory), because
# the latest channels don't have Elixir 1.9.
# See https://nixos.org/nix/manual/#idm140737317975776 for the meaning
# of `<nixpkgs>` and `~` in Nix expressions (towards the end of that
# section).
####################################################################
#               VVVVVVVVVVVVVVVV
{ pkgs ? import ~/clones/nixpkgs {} }:

pkgs.mkShell {

  buildInputs = with pkgs; [
    # No clue which package has this command, and not sure
    # how  to figure  it out.  Asked it  on NixOS  IRC, no
    # response, so TODO: figure it out.
    # base64
    beam.packages.erlangR22.erlang
    beam.packages.erlangR22.rebar3
    curl
    ffmpeg
    git
    google-cloud-sdk
    jq
  ];

  # Where would be the best place for this?
  shellHook = ''
    export ERL_AFLAGS="-kernel shell_history enabled"
  '';

  ####################################################################
  # Without  this, almost  everything  fails with  locale issues  when
  # using `nix-shell --pure` (at least on NixOS).
  # See
  # + https://github.com/NixOS/nix/issues/318#issuecomment-52986702
  # + http://lists.linuxfromscratch.org/pipermail/lfs-support/2004-June/023900.html
  ####################################################################

  LOCALE_ARCHIVE = if pkgs.stdenv.isLinux then "${pkgs.glibcLocales}/lib/locale/locale-archive" else "";
}
