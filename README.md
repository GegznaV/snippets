
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Package `snippets`

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/GegznaV/snippets.svg?branch=master)](https://travis-ci.com/GegznaV/snippets)
[![Codecov test
coverage](https://codecov.io/gh/GegznaV/snippets/branch/master/graph/badge.svg)](https://codecov.io/gh/GegznaV/snippets?branch=master)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/GegznaV/snippets?branch=master&svg=true)](https://ci.appveyor.com/project/GegznaV/snippets)
<!-- badges: end -->

## Installation

<!-- You can install the released version of snippets from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("snippets") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("GegznaV/snippets")
```

## Quick Example

``` r
# Replace current R and Markdown snippets with those in package "snippets"
snippets::install_snippets_from_package("snippets", type = "r",        backup = TRUE)
#> ✔ Directory exists: 'D:/Dokumentai/R/win-library/3.6/snippets/snippets/'✔
#> Directory contains 2 file(s) with extension '.snippet'✔ Back-up created: 'D:/
#> Dokumentai/.R/snippets/r.snippets--backup-2020-01-05-032904'✔ Snippets updated:
#> 'D:/Dokumentai/.R/snippets/r.snippets'
snippets::install_snippets_from_package("snippets", type = "markdown", backup = TRUE)
#> ✔ Directory exists: 'D:/Dokumentai/R/win-library/3.6/snippets/snippets/'✔
#> Directory contains 2 file(s) with extension '.snippet'✔ Back-up created: 'D:/
#> Dokumentai/.R/snippets/markdown.snippets--backup-2020-01-05-032904'✔ Snippets
#> updated: 'D:/Dokumentai/.R/snippets/markdown.snippets'
```

## Example

``` r
library(snippets)
```

``` r
# Get the name of directory with RStudio snippets
get_rs_snippets_dir()
```

``` r
# Open directory with RStudio snippets
open_rs_snippets_dir()
```

``` r
# Replace your R snippets with those in package "snippets"
install_snippets_from_package("snippets", type = "r", backup = TRUE)
list_snippet_file_backups(type = "r")
```

``` r
# View and edit file with snippets of certain type
edit_rstudio_snippets(type = "r")
```

``` r
# Replace your Markdown snippets with those in package "snippets"
install_snippets_from_package("snippets", type = "markdown", backup = TRUE)
list_snippet_file_backups(type = "markdown")
```

## Revert to certain version of back-up

1.  List all back-up files and select the one of inerest.
    
    ``` r
    list_snippet_file_backups(type = "r")
    ```
    
    ``` r
    #> c:/.R/snippets/r.snippets
    #> c:/.R/snippets/r.snippets--backup-2019-11-12-033948
    #> c:/.R/snippets/r.snippets--backup-2019-10-31-015042
    ```

2.  Restore the back-up of interest, e.g.:
    
    ``` r
    restore_snippets_backup("r.snippets--backup-2019-10-31-015042")
    ```
    
    ``` r
    #> v Back-up file was found: 'r.snippets--backup-2019-10-31-015042'
    #> i Snippets' type: r
    #> v Current file was backed up: 
    #>   'r.snippets' â†’ 'r.snippets--backup-2020-01-05-012602'
    #> v Previous back-up was restored 'r.snippets--backup-2019-10-31-015042' â†’ 'r.snippets'.
    ```
