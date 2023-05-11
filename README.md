# TaskTest
Usage: `test-task <path/to/taskConfig> <path/to/solution>`  

Depending on the way GHC is installed the you might need to configure the location of a package-db containing appropriate versions of `QuickCheck`, `HUnit`, `random`, `syb` and `haskell-src-exts` plus any additional dependencies you need for your tasks.
Either set the `GHC_PACKAGE_PATH` variable or place a [package environment file](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/packages.html#package-environments) at `$HOME/.test-task/pkg-env` (`%APPDATA%/test-task/pkg-env` on Windows).

If the solution module has a `main` function, `test-task` will run it assuming it is a local test-suite, as long as the task is not a CodeWorld task, i.e., no `import CodeWorld` directive is present.

### Type holes
In case tasks require the definition of new datatypes, compilation of the template may fail if these types are, for example, mentioned in type signatures but not given as stub-definitions in the template. Adding the `--type-holes` flag followed by the names of the missing types adds empty type definitions to the template. For example `test-task <path/to/taskConfig> <path/to/solution> --type-holes A B` will add the two lines `data A` and `data B` at the end of the template.  
