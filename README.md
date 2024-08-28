
<!-- README.md is generated from README.Rmd. Please edit that file -->

# grow97

<!-- badges: start -->
<!-- badges: end -->

This R package adds some quality-of-life functionality to J.
Engelstaedter’s [grow96](https://github.com/JanEngelstaedter/grow96)
package, and requires it (and understanding of it) to run. Its primary
purpose is processing multiple growth assay projects at once in a
systematic and consistent way; under the hood it essentially calls
grow96 functions in the appropriate order on each project, collates the
data, and outputs everything in a combined dataset for downstream
analysis and plotting. In the process, it also allows easy fixing of
common inconsistencies that occur between assays (e.g. “Drug” vs
“Drugs”, or typos in strain names) by means of a CSV key, as well as
flexibility in what kind of blank OD values to use for each project, and
how the final combined dataset is presented.

grow97 was originally built because I had several related assays spread
over years that needed to be presented together; its advantage over
simply using grow96 directly increases the more projects you need to
process, and the more inconsistent they are.

## Setup

To install the package, run the following lines:

``` r
install.packages("devtools")
devtools::install_github("pseudonoma/grow97")
```

## Use

Under construction! Please check back later.
