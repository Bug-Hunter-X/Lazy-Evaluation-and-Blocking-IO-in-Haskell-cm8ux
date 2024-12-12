The solution involves ensuring that the infinite list is only evaluated as needed. In this case, we can force evaluation before the blocking IO operation. We can also avoid using infinite lists unless absolutely necessary. Here's a corrected version:

```haskell
import System.IO

main :: IO ()
main = do
  let infiniteList = [1..]
  let result = take 5 $! infiniteList -- Strict evaluation using $!

  print result

  putStrLn "Press Enter to continue..."
  getLine

  let finiteList = take 1000 infiniteList  -- Creating a finite list from an infinite one
  print (head finiteList) 
```

By using the `$!` operator, we force the evaluation of `take 5 infiniteList`, ensuring that only the necessary portion of the list is computed before the program continues. Alternatively, generating a finite list ahead of time prevents the infinite list from causing any further problems.