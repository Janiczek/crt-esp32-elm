Compiling: Instead of `elm make ...` use `lamdera make ...`.
Testing: Instead of `elm-test ...` use `elm-test --compiler=lamdera ...`.
Formatting: Instead of `elm-format ...` use `elm-format --yes ...`.

Never compile without `--output=elm.js`. That would overwrite the `index.html` file and break our JS-Elm ports integration.

In `case..of` expressions over custom types that are first-party (defined by us), always mention all cases - don't do `_` ->`.

Test cases must use format
```elm
actual
|> Expect.foo expected
```
instead of
```elm
Expect.foo expected actual
```