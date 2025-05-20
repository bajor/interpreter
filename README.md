### Finished at
https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

Now: Exercise 5th

#### Build
`stack build`

#### Run with arg
`stack run <arguments>`

#### Altenratively compile with ghc
```haskell
ghc -o my_program --make app/Main.hs
```
and run it:
```bash
./my_program
```

#### `hoogle`
`stack hoogle -- --generate --local          # one-off, ~2–5 min`

After index exist:
```
stack hoogle mapM                    # CLI lookup
stack hoogle -- server --local       # starts http://localhost:8080
```

