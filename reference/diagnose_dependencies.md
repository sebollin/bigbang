# Diagnose implicit dependencies of local packages

Scans local packages for references to the recommended packages 'Matrix'
and 'class', which can cause `R CMD check` failures when they are used
implicitly but not declared as dependencies.

## Usage

``` r
diagnose_dependencies(packages, pkg_dir = NULL, ext = ".tar.gz")
```

## Arguments

- packages:

  Character vector. Archive paths or stems to examine, e.g.
  `"conexiones_0.8.3"`.

- pkg_dir:

  Character. Directory or directories containing local archives
  (`.tar.gz`, `.zip`, etc.).

- ext:

  Character. Archive extension. Defaults to `".tar.gz"`.

## Value

A named list with one entry per local package, each a list with two
elements:

- matrix_refs:

  Character vector of references to 'Matrix', with file and line.

- class_refs:

  Character vector of references to 'class', with file and line.

## Details

Extracts and scans the R source of each package for patterns that
suggest implicit use of 'Matrix' or 'class'. Useful for debugging
`R CMD check` errors such as "there is no package called 'Matrix'" even
when the package does not appear to use it directly.

## Examples

``` r
archives <- system.file("extdata", package = "bigbang")
res <- diagnose_dependencies(
  packages = "toycomponent_0.1.0",
  pkg_dir = archives
)
res[["toycomponent_0.1.0"]]
#> $matrix_refs
#> character(0)
#> 
#> $class_refs
#> character(0)
#> 
lapply(res, function(x) x$matrix_refs)
#> $toycomponent_0.1.0
#> character(0)
#> 
```
