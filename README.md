
<!-- badges: start -->

[![R-CMD-check](https://github.com/pacmandoh/typedr/actions/workflows/package-check.yaml/badge.svg)](https://github.com/pacmandoh/typedr/actions/workflows/package-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/pacmandoh/typedr/branch/main/graph/badge.svg)](https://codecov.io/gh/pacmandoh/typedr?branch=main)
<!-- badges: end -->

# typedr <img src='man/figures/logo.png' align="right" height="139" />

*{typedr}* is a modernized refactor of
[*{typed}*](https://github.com/moodymudskipper/typed), the R package
created by moodymudskipper. It keeps the same core idea: make
lightweight runtime type constraints feel native in R code.

It has several main features:

- set variable types in a script or the body of a function, so they
  can’t be assigned illegal values
- set argument types in a function definition
- set return type of a function
- combine and condition function argument types using compact typedr
  syntax

The user can define their own types, or leverage assertions from other
packages.

Under the hood variable types use active bindings, so once a variable is
restricted by an assertion, it cannot be modified in a way that would
not satisfy it.

Compared with *{typed}*, this package focuses on modern internals and
developer experience:

- expression handling, environments, and condition plumbing have been
  migrated toward *{rlang}*
- errors are emitted through *{cli}* with typed condition classes such
  as `typedr_type_error`, `typedr_assign_error`, and
  `typedr_return_error`
- printed output for typed functions, assertion factories, assertions,
  and typed values is structured, color-aware, and easier to inspect
- helper printers such as `print_typedr()`, `print_all_args()`,
  `print_whole_fn()`, `print_whole_value()`, and `print_stats()` expose
  the richer print layer when you need more detail
- typed function print output shows the rewritten body, return/argument
  types, optional truncation hints, and (once per session) a note when
  **prettycode** is not installed; install **prettycode** for fuller
  syntax highlighting

## Installation

Install CRAN version with:

``` r
install.packages("typedr")
```

or development version with :

``` r
remotes::install_github("pacmandoh/typedr")
```

And attach with :

``` r
# masking warning about overriding `?`
library(typedr, warn.conflicts = FALSE)
```

## Set variable type

### Question mark notation and `declare`

Here are examples on how we would set types

``` r
Character() ? x # restrict x to "character" type
x <- "a"
x
#> value: <character> [1] "a"
#> • assertion: <Character()>
#> • const: FALSE

Integer(3) ? y <- 1:3 # restrict y to "integer" type of length 3
y
#> value: <integer> [3] 1, 2, 3
#> • assertion: <Integer(3)>
#> • const: FALSE
```

We cannot assign values of the wrong type to `x` and `y` anymore.

``` r
x <- 2
#> Error:
#> ! Assigned value to `x` doesn't satisfy the assertion.
#> Caused by error in `Character()`:
#> ! type mismatch
#> ✖ `typeof(value)`: "double", `expected`: "character"

y <- 4:5
#> Error:
#> ! Assigned value to `y` doesn't satisfy the assertion.
#> Caused by error in `Integer()`:
#> ! length mismatch
#> ✖ `length(value)`: 2L, `expected`: 3L
```

But the right type will work.

``` r
x <- c("b", "c")

y <- c(1L, 10L, 100L)
```

`declare` is a strict equivalent, slightly more explicit, which looks
like `base::assign`.

``` r
declare("x", Character())
x <- "a"
x
#> value: <character> [1] "a"
#> • assertion: <Character()>
#> • const: FALSE

declare("y", Integer(3), 1:3)
y
#> value: <integer> [3] 1, 2, 3
#> • assertion: <Integer(3)>
#> • const: FALSE
```

Declaring a variable without an initial value (`Character() ? x` or
`declare("x", Character())`) leaves it unset until you assign. In an
interactive session at the top level, typedr informs you that the
variable was declared but still unset. After assignment, reading the
variable at the REPL shows typedr’s value printer (data plus assertion
metadata).

### Assertion factories and assertions

`Integer` and `Character` are function factories (functions that return
functions), thus `Integer(3)` and `Character()` are functions.

The latter functions check a value and, on success, return it generally
unmodified. For instance:

``` r
Integer(3)(1:2)
#> Error in `Integer()`:
#> ! length mismatch
#> ✖ `length(value)`: 2L, `expected`: 3L

Character()(3)
#> Error in `Character()`:
#> ! type mismatch
#> ✖ `typeof(value)`: "double", `expected`: "character"
```

We call `Integer(3)` and `Character()` assertions, and we call `Integer`
and `Character` assertion factories. They are also called types, but
should not be confused with the atomic types returned by `typeof()`.

The package contains many assertion factories (see
`?assertion_factories`), the main ones are:

- `Any` (No default restriction)
- `Logical`
- `Integer`
- `Double`
- `Character`
- `List`
- `Environment`
- `Factor`
- `Matrix`
- `Data.frame`
- `Date`
- `Time` (POSIXct)

Assertions can be combined with `|` and `&`.

``` r
Number <- Integer() | Double()
Number(1L)
#> [1] 1
Number(1)
#> [1] 1
Number("a")
#> Error in `Number()`:
#> ! Value does not satisfy any allowed <Type()>.
#> ✖ Expected one of: <Integer() | Double()>.

PositiveInteger <- Integer() & Any(... = ~ . > 0L)
PositiveInteger(1L)
#> [1] 1
PositiveInteger(0L)
#> Error in `PositiveInteger()`:
#> ! Value does not satisfy all required <Type()> constraints.
#> ✖ Failed constraint: Any(... = ~ . > 0L).
```

The `|` operator is a union: a value is accepted if any assertion
accepts it. The `&` operator is an intersection: every assertion must
accept the value, in order. For compatibility with R’s usual combining
idiom, `c(Integer(), Double())` is also accepted and means the same as
`Integer() | Double()`. The `|` notation is usually clearer in function
signatures.

### Advanced type restriction using arguments

As we’ve seen with `Integer(3)`, passing arguments to an assertion
factory restricts the type.

For instance `Integer` has arguments `length` `allow_null` and `...`. We
already used `length`, `allow_null` is convenient to allow a default
`NULL` value in addition to the `"integer"` type.

The arguments can differ between assertion factories, for instance
`Data.frame` has `nrow`, `ncol`, `each`, `allow_null` and `...`

``` r
Data.frame() ? x <- iris
Data.frame(ncol = 2) ? x <- iris
#> Error in `declare()`:
#> ! Invalid initial value for `x`.
#> Caused by error in `Data.frame()`:
#> ! Column number mismatch
#> ✖ `ncol(value)`: 5L, `expected`: 2L
Data.frame(each = Double()) ? x <- iris
#> Error in `declare()`:
#> ! Invalid initial value for `x`.
#> Caused by error in `Data.frame()`:
#> ! column 5 ("Species") failed assertion.
#> Caused by error in `Double()`:
#> ! type mismatch
#> ✖ `typeof(value)`: "integer", `expected`: "double"
```

In the dots we can use arguments named as functions and with the value
of the expected result.

``` r
# Integer has no anyNA arg but we can still use it because a function named
# this way exists
Integer(anyNA = FALSE) ? x <- c(1L, 2L, NA)
#> Error in `declare()`:
#> ! Invalid initial value for `x`.
#> Caused by error in `Integer()`:
#> ! `anyNA` mismatch
#> ✖ `anyNA(value)`: TRUE, `expected`: FALSE
```

Useful arguments might be for instance, `anyDuplicated = 0L`,
`names = NULL`, `attributes = NULL`… Any available function can be used.

That makes assertion factories very flexible. If that is still not
flexible enough, arguments named `...` can add custom restrictions. For
repeated use, this is usually better expressed as a wrapper. The example
below assigns an invalid value on purpose to show the custom message
from the `...` check:

``` r
Character(1, ... = "`value` is not a fruit!" ~ . %in% c("apple", "pear", "cherry")) ? 
  x <- "potatoe"
#> Error in `declare()`:
#> ! Invalid initial value for `x`.
#> Caused by error in `Character()`:
#> ! `value` is not a fruit!
#> ✖ `value %in% c("apple", "pear", "cherry")`: FALSE, `expected`: TRUE
```

This is often better done by defining a wrapper as shown below.

### Concise diagnostics

typedr keeps generated errors focused on the user-facing assertion.
Internal wrapper names such as `f()` are replaced by calls such as
`Double()` or a custom factory name. Repeated container failures show
only the first failed item or column and the number of remaining
failures. Long names, expressions, values, and union candidate lists are
shortened in diagnostic bullets. Exceptionally long union or
intersection parent calls use the neutral `Type()` label; long
single-factory calls fall back to the factory name (for example
`Character()`).

``` r
many_columns <- as.data.frame(setNames(
  rep(list(1L), 4),
  c("first", "a very long second column name", "third", "fourth")
))
Data.frame(each = Double()) ? compact_example <- many_columns
#> Error in `declare()`:
#> ! Invalid initial value for `compact_example`.
#> Caused by error in `Data.frame()`:
#> ! 4 columns failed assertion.
#> ✖ First failure: column 1 ("first"); and 3 more.
#> Caused by error in `Double()`:
#> ! type mismatch
#> ✖ `typeof(value)`: "integer", `expected`: "double"
```

The first underlying assertion error remains attached as the parent
condition, so `rlang::last_trace()` still contains the useful root cause
without expanding every repeated failure.

### Performance

typedr prioritizes clear runtime errors over zero-cost checks, but
native assertion factories such as `Integer()` and `Double()` also use
two internal optimizations on the success path:

1.  **Native fast path.** When a factory call is simple (no `...` dots,
    no `each` or `levels`, and not `List(data_frame_ok = FALSE)`), the
    generated assertion can return immediately after a lightweight type
    check. Failures still fall back to the full slow path, so error
    classes and messages stay the same.

2.  **Definition-time caching.** When you write `? function(...)`,
    typedr evaluates assertion factories such as `Integer()` once while
    building the function, then reuses the cached assertion on every
    call. This removes repeated factory work from hot paths such as
    `check_arg()` and `check_output()`.

**What this means in practice:**

| Cost | When | Typical order |
|----|----|----|
| Factory creation (`Integer()`, etc.) | Once, when a typed function is defined | ~1–2 ms |
| Cached typed function call (native types) | Every call after definition | ~30 µs |
| Direct assertion call (`Integer()(x)`) | Every call | ~3 µs on success |
| Custom `new_type()`, combinators, dotted factories | Every call | Slow path (ms) |

Definition-time caching trades a **one-time** compile cost when a typed
function is created for much lower **per-call** overhead. Each cached
assertion is a single function object stored in the typed function’s
enclosing environment; memory use is usually negligible compared with
typical data or model objects.

Optimizations apply to native factories and simple typed functions. They
do **not** replace the slow path for custom assertions,
union/intersection combinators, dependent arguments, or factories with
extra named `...` dots (see *Advanced type restriction using arguments*
above).

### Constants

To define a constant, we just surround the variable by parentheses
(think of them as a protection)

``` r
Double() ? (x) <- 1
x <- 2
#> Error:
#> ! Can't assign to a constant `x`.

# defining a type is optional
? (y) <- 1
y <- 2
#> Error:
#> ! Can't assign to a constant `y`.
```

### Set a function’s argument type

We can set argument types this way :

``` r
add <- ? function (x= ? Double(), y= 1 ? Double()) {
  x + y
}
```

Note that we started the definition with a `?`, and that we gave a
default to `y`, but not `x`. Note also the `=` sign next to `x`,
necessary even when we have no default value. If you forget it you’ll
have an error “unexpected `?` in …”.

This created the following function, by adding checks at the top of the
body

``` r
add
#> <typedr function>
#> function (x, y = 1) 
#> {
#>   check_arg(x, Double())
#>   check_arg(y, Double())
#>   x + y
#> }
#> Return: <Any()>
#> Arguments:
#> • `x`: <Double()>
#> • `y`: <Double()> (default: 1)
#> 
#> ! {prettycode} is not installed, using basic {typedr} syntax highlighting.
#> ℹ Install it with `install.packages('prettycode')` for fuller R syntax
#>   highlighting.
#> This message is displayed once per session.
```

Let’s test it by providing a right and wrong type.

``` r
add(2, 3)
#> [1] 5
add(2, 3L)
#> Error in `add()`:
#> ! Invalid <Type()> of `y` to `add()`.
#> Caused by error in `Double()`:
#> ! type mismatch
#> ✖ `typeof(value)`: "integer", `expected`: "double"
```

If we want to restrict `x` and `y` to the type “integer” in the rest of
the body, so they cannot be overwritten by character for instance,we can
use the `?+` notation :

``` r
add <- ? function (x= ?+ Double(), y= 1 ?+ Double()) {
  x + y
}

add
#> <typedr function>
#> function (x, y = 1) 
#> {
#>   check_arg(x, Double(), .bind = TRUE)
#>   check_arg(y, Double(), .bind = TRUE)
#>   x + y
#> }
#> Return: <Any()>
#> Arguments:
#> • `x`: <Double()>
#> • `y`: <Double()> (default: 1)
```

We see that it is translated into a `check_arg` call containing a
`.bind = TRUE` argument.

### Combine and link argument types

Union and intersection types can be used directly in function
signatures.

``` r
as_number <- ? function(x = ? Integer() | Double()) {
  x
}

as_number(1L)
#> [1] 1
as_number(1)
#> [1] 1
as_number("a")
#> Error in `as_number()`:
#> ! Invalid <Type()> of `x` to `as_number()`.
#> Caused by error in `Integer() | Double()`:
#> ! Value does not satisfy any allowed <Type()>.
#> ✖ Expected one of: <Integer() | Double()>.
```

Arguments can also depend on other arguments. Use a two-sided formula
after `?`: the left side is a guard, and the right side is the assertion
that applies to the current argument when the guard matches.

``` r
scale_value <- ? function(
  x = ? Integer() | Character(),
  scale = ? x:Integer() ~ Double()
) {
  TRUE
}

scale_value(1L, 2)
#> [1] TRUE
scale_value(1L, 2L)
#> Error in `scale_value()`:
#> ! Invalid dependent <Type()> of `scale`.
#> ℹ Guard `x:Integer()` matched, so `scale` must satisfy <Double()>.
#> Caused by error in `Double()`:
#> ! type mismatch
#> ✖ `typeof(value)`: "integer", `expected`: "double"
scale_value("a", "scale is ignored")
#> [1] TRUE
```

Guards use `arg:Type()` and can be combined with `|`, `&`, parentheses,
and `!`.

``` r
dependent <- ? function(
  a1 = ? Any(),
  a2 = ? Any(),
  out = ? a1:Integer() | a2:Character() ~ Double()
) {
  TRUE
}

dependent(1L, FALSE, 1)
#> [1] TRUE
dependent(FALSE, "x", 1L)
#> Error in `dependent()`:
#> ! Invalid dependent <Type()> of `out`.
#> ℹ Guard `a1:Integer() | a2:Character()` matched, so `out` must satisfy
#>   <Double()>.
#> Caused by error in `Double()`:
#> ! type mismatch
#> ✖ `typeof(value)`: "integer", `expected`: "double"
```

By default, a guard that does not match simply leaves the dependent
argument alone. Add `/ Warning()` or `/ Error()` to report when the
dependent argument is supplied but inactive.

``` r
optional_scale <- ? function(
  x = NULL ? (Null() | Integer()),
  scale = ? !x:Missing() ~ Double() / Warning()
) {
  TRUE
}

optional_scale()
#> [1] TRUE
optional_scale(x = NULL, scale = 2)
#> Warning in optional_scale(x = NULL, scale = 2): ! Argument `scale` is inactive.
#>   ℹ Guard `!x:Missing()` did not match, so `scale` will not take effect.
#> [1] TRUE
optional_scale(x = 1L, scale = 2)
#> [1] TRUE
optional_scale(x = 1L, scale = 2L)
#> Error in `optional_scale()`:
#> ! Invalid dependent <Type()> of `scale`.
#> ℹ Guard `!x:Missing()` matched, so `scale` must satisfy <Double()>.
#> Caused by error in `Double()`:
#> ! type mismatch
#> ✖ `typeof(value)`: "integer", `expected`: "double"
```

`Missing()` is a guard-only helper: it matches when the argument was not
supplied or when its value is `NULL`. It is intentionally not a
standalone exported type. `Warning(Type())` can also be used on the
right side to warn rather than error when the guard matches but the
dependent type check fails.

## Set a function’s return type

To set a return type we use `?` before the function definition as in the
previous section, but we type an assertion on the left hand side.

``` r
add_or_subtract <- Double() ? function (x, y, subtract = FALSE) {
  if(subtract) return(x - y)
  x + y
}
add_or_subtract
#> <typedr function>
#> function (x, y, subtract = FALSE) 
#> {
#>   if (subtract) 
#>     return(check_output(x - y, Double()))
#>   check_output(x + y, Double())
#> }
#> Return: <Double()>
```

The function body is rewritten so return expressions go through
`check_output()`. Callers still receive plain R values after the check
passes; internal `declare()` variables keep their active bindings only
inside the function.

We see that the returned values have been wrapped inside `check_output`
calls.

## Use typedr in a package and define your own types

See `vignette("typedr-in-packages", "typedr")` or the Article section if
you’re browsing the pkgdown website.

## Relationship with {typed}

*{typedr}* is derived from, and deeply indebted to,
[*{typed}*](https://github.com/moodymudskipper/typed). The original
package established the public syntax and the central model used here:
`?` for declaring typed variables, typed function arguments, and typed
return values; assertion factories such as `Integer()` and
`Character()`; and active bindings for runtime assignment checks.

The goal of *{typedr}* is not to erase that lineage. It is to carry the
same idea forward with a codebase that leans on the modern tidyverse
infrastructure available today, especially *{rlang}* and *{cli}*. Many
concepts, examples, and interfaces will therefore feel familiar to users
of *{typed}*, while error objects and print output are intentionally
more structured in *{typedr}*.

## Acknowledgements

This package would not exist without
[*{typed}*](https://github.com/moodymudskipper/typed). Thank you to
moodymudskipper for designing and releasing the original package, and
for making such an imaginative experiment in R runtime typing available
to the community. *{typedr}* is a grateful continuation of that work.

The original *{typed}* README also acknowledged Jim Hester and Gabor
Csardi’s work and many great efforts on static typing, assertions, or
annotations in R. We keep that acknowledgement here with appreciation:

- Gabor Csardy’s [*argufy*](https://github.com/gaborcsardi/argufy)
- Richie Cotton’s
  [*assertive*](https://bitbucket.org/richierocks/assertive/)
- Tony Fishettti’s [*assertr*](https://github.com/tonyfischetti/assertr)
- Hadley Wickham’s [*assertthat*](https://github.com/hadley/assertthat)
- Michel Lang’s [*checkmate*](https://github.com/mllg/checkmate)
- Joe Thorley’s [*checkr*](https://github.com/poissonconsulting/checkr)
- Joe Thorley’s [*chk*](https://github.com/poissonconsulting/chk/)
- Aviral Goel’s [*contractr*](https://github.com/aviralg/contractr)
- Stefan Bache’s [*ensurer*](https://github.com/smbache/ensurer)
- Brian Lee Yung Rowe’s
  [*lambda.r*](https://github.com/zatonovo/lambda.r)
- Kun Ren’s [*rtype*](https://github.com/renkun-ken/rtype)
- Duncan Temple Lang’s
  [*TypeInfo*](https://bioconductor.org/packages/TypeInfo/)
- Jim Hester’s [*types*](https://github.com/jimhester/types)
