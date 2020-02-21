
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Package `snippets`

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/snippets)](https://CRAN.R-project.org/package=snippets)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.3-brightgreen.svg)](https://github.com/GegznaV/snippets)
[![Travis build
status](https://travis-ci.com/GegznaV/snippets.svg?branch=master)](https://travis-ci.com/GegznaV/snippets)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/GegznaV/snippets?branch=master&svg=true)](https://ci.appveyor.com/project/GegznaV/snippets)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2020--02--21-yellowgreen.svg)]()
<!-- [![Codecov test coverage](https://codecov.io/gh/GegznaV/snippets/branch/master/graph/badge.svg)](https://codecov.io/gh/GegznaV/snippets?branch=master) -->
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

## Quickstart

### Install Snippets from “snippets”

``` r
# Replace current R and Markdown snippets with those in package "snippets"
snippets::install_snippets_from_package("snippets", type = c("r", "markdown"), backup = TRUE)
```

### More Examples

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
# Replace your R and Markdown snippets with those in package "snippets"
install_snippets_from_package("snippets", type = c("r", "markdown"), backup = TRUE)
list_snippet_file_backups()
```

``` r
# View and edit file with snippets of certain type: r
edit_rstudio_snippets(type = "r")
```

``` r
# View and edit file with snippets of certain type: markdown
edit_rstudio_snippets(type = "markdown")
```

### Create and Clean-up Back-ups

``` r
# Create several back up files
backup_rs_snippets(type = "r")
Sys.sleep(1)
backup_rs_snippets(type = "r")
Sys.sleep(1)
backup_rs_snippets(type = "r")
```

``` r
list_snippet_file_backups(type = "r")
```

``` r
# Remove duplicated back-up files
remove_snippet_backup_duplicates()
```

<!-- 

### Revert to a Certain Version of Back-up 

1. List all back-up files and select the one of interest.
    
    ```r
    list_snippet_file_backups(type = "r")
    ```
    ```r
    #> c:/.R/snippets/r.snippets
    #> c:/.R/snippets/r.snippets--backup-2019-11-12-033948
    #> c:/.R/snippets/r.snippets--backup-2019-10-31-015042
    ```
2. Restore the back-up of interest, e.g.:
    
    ```r
    restore_snippets_from_backup("r.snippets--backup-2019-10-31-015042")
    ```
    ```r
    #> v Back-up file was found: 'r.snippets--backup-2019-10-31-015042'
    #> i Snippets' type: r
    #> v Current 'r.snippets' file was backed up:
    #>   'r.snippets' -> 'r.snippets--backup-2020-01-05-012602'
    #> v Snippets were restored from the back-up file:
    #>   'r.snippets--backup-2019-10-31-015042' -> 'r.snippets'.
    ```
-->
