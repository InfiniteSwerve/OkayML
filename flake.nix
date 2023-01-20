{
  description = "A very basic flake";

  inputs =
    {
      nixpkgs.url = "github:nix-ocaml/nix-overlays";
      flake-utils.url = "github:numtide/flake-utils";
    };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in
        rec {
          packages.default = with pkgs.ocaml-ng.ocamlPackages_5_0;
            buildDunePackage {
              pname = "starter";
              version = "n/a";
              src = ./.;
              propagatedBuildInputs = [
              ];
            };
          devShells.default = pkgs.mkShell
            {
              inputsFrom =
                [
                  packages.default
                ];
              buildInputs = with pkgs.ocaml-ng.ocamlPackages_5_0;
                [
                  ocaml-lsp
                  ocamlformat
                  ppx_deriving
                  alcotest
                  graphics
                ];
            };
        }

      );
  nixConfig = {
    extra-trusted-substituters = "https://anmonteiro.nix-cache.workers.dev";
    extra-trusted-public-keys = "ocaml.nix-cache.com-1:/xI2h2+56rwFfKyyFVbkJSeGqSIYMC/Je+7XXqGKDIY=";
  };

}
