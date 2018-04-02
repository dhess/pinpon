# pinpon changelog

Fixes:

  - Bump some package upper bounds.
  
  - Pin some packages in Nix for compatibility with recent nixpkgs.
  
Changes:

  - Now requires `servant` 0.13.
  
  - Drop Stack support until
    `conduit-1.3`/`resourcet-1.2`/`servant`/`amazonka` issues are
    resolved.
  
  - Disable Travis builds until Stack is supported again.

## 0.2.0.2

Fixes:

  - Fix hlint 2.1 issue.

  - Fix a bounds typo in package.yaml.

Changes:

  - Bump hlint upper bound.

## 0.2.0.1

Changes:

  - This package now uses Protolude.

  - Switch to hpack.

  - We only support GHC 8.0.2 and 8.2.2 now.

  - The `test-hlint` cabal flag is now disabled by default.

  - Updated copyright year.

  - Requires `hlint` 2.0.*.

  - Much improved Nix support, including a default fixed nixpkgs
    revision, Hydra jobsets, and Nix/Hydra builds against LTS package
    sets.

  - Test on `armv7l`.

Fixes:

  - All dependencies should now have PVP bounds.

  - `swagger.json` must be a `data-file` for Stack tests.

  - Remove an unused `mellon-core` dependency from stack.yaml.

## 0.2.0.0

- Requires Servant 0.11+.

- Other minor fixes.

## 0.1.0.0

Initial release.
