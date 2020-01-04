
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Package `snippets`

<!-- badges: start -->

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

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(snippets)
```

``` r
rs_snippets_dir <- get_rs_snippets_dir()
rs_snippets_dir
```

``` r
browseURL(rs_snippets_dir)
```

``` r
# Replace your R snippets with those in package "snippets"
replace_snippets_file("r", backup = TRUE)
list_snippet_file_backups("r")
```

``` r
# Replace your Markdown snippets with those in package "snippets"
replace_snippets_file("markdown", backup = TRUE)
list_snippet_file_backups("markdown")
```
