# clean-as-app
A starter app with Purescript + Halogen + Bulma + Nix + Flake

* Purescript - https://www.purescript.org/
* Halogen - https://github.com/purescript-halogen/purescript-halogen
* Bulma - https://bulma.io/
* Nix - https://nixos.org/
* Flake - https://nixos.wiki/wiki/Flakes

It uses [Purescript realworld application](https://github.com/thomashoneyman/purescript-halogen-realworld) as the starter.

Once you've got Nix and Flake setup locally,

* Enter development environment with `nix develop`
* Bundle the app with `nix run .#bundle`
* Serve the app locally with `nix run .#serve`
