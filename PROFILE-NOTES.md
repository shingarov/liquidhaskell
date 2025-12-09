# Running the legacy exe under profiling

1. Checkout:
```
git clone git@github.com:shingarov/liquidhaskell.git
cd liquidhaskell
git checkout profile-legacy-exe
git submodule update --init
```
2. Build with profiling:
```
stack build liquidhaskell --fast --profile
```
3. Do whatever changes, e.g. add a `traceStack` to `src/Language/Haskell/Liquid/Constraint/Generate.hs`
4. Build your changes:
```
LIQUID_DEV_MODE=true  stack build liquidhaskell --fast --profile
```
5. Run:
```
.stack-work/dist/x86_64-linux-tinfo6/ghc-9.2.5/build/liquid/liquid tests/pos/Foldr.hs +RTS -p -RTS
```
Note that I tested this with Z3 4.13.0.

