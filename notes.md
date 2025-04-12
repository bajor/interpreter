`(expr:_) <- getArgs` This is pattern matching on the list returned by getArgs. It extracts the first command-line argument (expr) and ignores the rest (_). If no arguments are given, it will crash—so it's not safe without checks.

<br>

`>>` in Haskell is a sequencing operator used in monads. Its behavior depends on the monad: in the Parser monad, it runs the first parser, then the second on the remaining input, failing if either fails. In the IO monad, it runs actions in order, discarding the first result. Always check the monad's docs to understand its exact behavior.

<br>

In Haskell, `Either a b` represents a value that is either `Left a` (an error) or `Right b` (a success). You handle it with pattern matching:

```haskell
case result of  
  Left err   -> handle the error  
  Right val  -> use the successful value
```

Parsec uses `Left` for parse errors and `Right` for successful parses.

<br>


