## modeltests

This is a resubmission for the initial release of modeltests.

## Resubmission comments

During my last submission, I recieved the following feedback:

> Please provide small executable examples in all your exported functions' Rd files to illustrate the use of the exported function but also enable automatic testing.

I would prefer not to for two reasons:

1. The function exports tests to be used in other packages. Demonstrating interactive usage doesn't help users as this is not the context in which the functions will be called.

2. Any informative example would result in throwing an error, and would thus need to be wrapped in \donttest{} anyway, precluding automatic testing. Additionally, I have written extensive tests in the tests/ directory and believe tests/ is the appropriate place for automated tests, rather than relying on examples in function documentation.

## Test environments

* local Windows 8 install: R 3.5
* win-builder: oldrel, rel, devel
* rhub: oldrel, rel, devel

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Reverse dependencies

There are no reverse dependencies at this time.
