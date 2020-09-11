# FooLang

Interpreter for a small ⚡functional language⚡.

## Demo

![Alt Text](https://j.gifs.com/2xM1jK.gif)

## Characteristics

- It's statically typed, but types are inferred rather than manually specifed.

- Haskell-like lambdas and function application:

  - ```
      let identity x = x

      let lambdaId   = (\x -> x)

      let foo = 12 + (identity 1)
    ```

- It uses Hindley–Milner type system for type inference and polymorhphic types(✔)

  - `let id = (\x -> x)` - type of id: forall a => a -> a

  - `let flip f x y = f y x` - type of f: forall a b c => (b -> a -> c) -> (a -> b -> c).
    If you run that you will note that brackets are omitted, but since arrow is right-associative it's all the same.

- Two base types:

  - Int - as big as it needs to be,
  - Bool - True or False.

- Supports reccursion(✨):

```
    let rec fib n =
        if (n <= 0) then
            0
        else if (n == 1) then
            1
        else
            (fib (n - 1)) + (fib (n - 2))
```

- Interpreter supports few commands:
  - `:type fibb` - checks type of given identifier (in this case `fibb`),
  - `:browse` - prints list of defined functions with their respective types,
  - `:quit` - leave shell or (same as CTRL-D),
  - `:load path` - loads module from a file,
  - `:paste` - enter multline mode.

For a code snippet look at [🔥🔥example](./foo-scripts/script.foo).

## Try it yourself

If you have stack (haskell build tool) installed on your machine just
clone the repo and enter `stack run`.

You can also run `stack install` which will install the binaries in ~/usr/bin/.
Then to run shell, simply eneter `FooLang-exe`.
