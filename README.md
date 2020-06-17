# TaskTest
Usage: `test-task <path/to/taskConfig> <path/to/solution>`  

`test-task` expects the presence of `TestHelper.hs` and `TestHarness.hs` in `$HOME/.test-task`.

Depending on the way GHC is installed the you might need to configure the location of a package-db containing appropriate versions of `QuickCheck`, `HUnit`, `random`, `syb` and `haskell-src-exts` plus any additional dependencies you need for your tasks.
Either set the `GHC_PACKAGE_PATH` variable or place a [package environment file](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/packages.html#package-environments) at `$HOME/.test-task/pkg-env`.

If the solution module is called `Main`, `test-task` will run `Main.main` assuming it is a local test-suite, as long as the task is not a CodeWorld task, i.e., no `import CodeWorld` directive is present.
