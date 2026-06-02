## Test environments

* local R installation, R 4.5.1
* GitHub Actions, R release

## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: R CMD check reports internal NSE-style uses of `...` in the `?` syntax
  and assertion factory generation.

## Comments

This is the first typedr release after modernizing the original typed project
with rlang and cli internals, structured typedr error classes, and richer print
output. The documentation now explicitly describes typedr's relationship to
moodymudskipper/typed and acknowledges the original project with gratitude.
