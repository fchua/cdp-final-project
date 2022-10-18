{ source-repo-override ? { } }:
########################################################################
# default.nix -- The top-level nix build file for donation-contract.
#
# This file defines various attributes that are used for building and
# developing donation-contract.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   donation-contract: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix { inherit source-repo-override; };

  inherit (packages) pkgs donation-contract;
  project = donation-contract.haskell.project;
in
{
  inherit pkgs donation-contract;

  inherit project;
}
