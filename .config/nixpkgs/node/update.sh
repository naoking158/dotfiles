#!/bin/bash

# nix-shell -p nodePackages.node2nix --command "node2nix -i ./node-packages.json --supplement-input supplement.json"
# nix-shell -p nodePackages.node2nix --command "node2nix -i ./package.json -o package.nix"
nix-shell -p nodePackages.node2nix --command "node2nix -i ./node-packages.json -o node-packages.nix"
