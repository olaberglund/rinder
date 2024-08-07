{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    mission-control.url = "github:Platonic-Systems/mission-control";
    flake-root.url = "github:srid/flake-root";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.mission-control.flakeModule
        inputs.flake-root.flakeModule
      ];

      perSystem = { self', pkgs, config, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          basePackages = pkgs.haskell.packages.ghc92;

          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          packages = {
            # aeson.source = "1.5.0.0";      # Override aeson to a custom version from Hackage
            # shower.source = inputs.shower; # Override shower to a custom source path
          };

          settings = {
            servant-event-stream = {
              broken = false;
              jailbreak = true;
            };

            language-ecmascript = { broken = false; };
          };

          devShell = {
            # Enabled by default
            # enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            tools = hp: {
              haskell-language-server = pkgs.haskell-language-server.override {
                supportedGhcVersions = [ "92" ];
              };
            };

            mkShellArgs = {

              buildInputs = with pkgs; [ postgresql inotify-tools git ];

              shellHook = ''
                mkdir -p .nix-shell
                export NIX_SHELL_DIR=$PWD/.nix-shell
                export PGDATA="$NIX_SHELL_DIR/db"

                trap "pg_ctl -D "$PGDATA" stop" EXIT

                if ! test -d "$PGDATA"
                then
                  pg_ctl initdb -D  "$PGDATA"
                  HOST_COMMON="host\s\+all\s\+all"
                  sed -i "s|^$HOST_COMMON.*127.*$|host all all 0.0.0.0/0 trust|" "$PGDATA/pg_hba.conf"
                  sed -i "s|^$HOST_COMMON.*::1.*$|host all all ::/0 trust|"      "$PGDATA/pg_hba.conf"
                fi

                pg_ctl                                                  \
                  -D $PGDATA                                            \
                  -l $PGDATA/postgres.log                               \
                  -o "-c unix_socket_directories='$PGDATA'"             \
                  -o "-c listen_addresses='*'"                          \
                  -o "-c log_destination='stderr'"                      \
                  -o "-c logging_collector=on"                          \
                  -o "-c log_directory='log'"                           \
                  -o "-c log_filename='postgresql-%Y-%m-%d_%H%M%S.log'" \
                  -o "-c log_min_messages=info"                         \
                  -o "-c log_min_error_statement=info"                  \
                  -o "-c log_connections=on"                            \
                  start
              '';

              LOCALE_ARCHIVE = if pkgs.stdenv.isLinux then
                "${pkgs.glibcLocales}/lib/locale/locale-archive"
              else
                "";

            };

            hlsCheck.enable =
              pkgs.stdenv.isDarwin; # On darwin, sandbox is disabled, so HLS can use the network.
          };
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell

        };

        mission-control.scripts = {
          is_ready = {
            description = "Check if the postgres server is ready";
            exec = ''pg_isready -h "$PGDATA"'';
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.rinder;

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.mission-control.devShell
          ];
        };
      };
    };
}
