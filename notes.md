`(expr:_) <- getArgs` This is pattern matching on the list returned by getArgs. It extracts the first command-line argument (expr) and ignores the rest (_). If no arguments are given, it will crash—so it's not safe without checks.

<br>

`>>` in Haskell is a sequencing operator used in monads. Its behavior depends on the monad: in the Parser monad, it runs the first parser, then the second on the remaining input, failing if either fails. In the IO monad, it runs actions in order, discarding the first result. Always check the monad's docs to understand its exact behavior.
<br>
In general, use >> if the actions don't return a value, >>= if you'll be immediately passing that value into the next action, and do-notation otherwise.

<br>

**`do`‑notation** is just syntactic sugar for chaining monadic operations with **`>>=`** (bind) and **`>>`** (sequence/discard). It lets you write “imperative‑looking” code while still living inside any monad.


| In `do`‑notation | Desugared form |
|------------------|----------------|
| `x <- m1`        | `m1 >>= \x -> …` (binds the result of `m1` to `x`) |
| `m1`             | `m1 >> …` (discard `m1`’s result and continue) |
| last line `e`    | `return e` (implicit) |

**How it plays out in different monads**

* **`IO`** – each line is an IO action executed in order.  
```haskell
do name <- getLine      -- wait for user input
 putStrLn ("Hi " ++ name)  -- then greet
```

<br>

In Haskell, `Either a b` represents a value that is either `Left a` (an error) or `Right b` (a success). You handle it with pattern matching:

```haskell
case result of  
  Left err   -> handle the error  
  Right val  -> use the successful value
```

Parsec uses `Left` for parse errors and `Right` for successful parses.

<br>

Function Application sith `$`:
```haskell
-- Without $
print (sum (map (+1) [1,2,3]))

-- With $
print $ sum $ map (+1) [1,2,3]

-- Same result, less parentheses
```

<br>

**(<|>)** tries the left option, if it fails, tries the right one
```haskell
-- Works with Maybe, Parsers, etc.

Just 1 <|> Just 2     -- Result: Just 1
Nothing <|> Just 2    -- Result: Just 2

-- Chained <|>: picks the first Just, skips the rest

Nothing <|> Nothing <|> Just 3 <|> Just 4  -- Result: Just 3
```


