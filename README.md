
# joint

<!-- badges: start -->
<!-- badges: end -->

The goal of joint is to implement methods to approximate joint distributions from multiple marginal distributions.

## Installation

You can install the development version of joint from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jokroese/joint")
```

## Example

You can approximate a joint distribution from several marginal distributions.

``` r
library(joint)
targets <- list(age_lsoa, age_disability_la, age_nssec_la)
empty_joint <- tidyr::crossing(age_lsoa$lsoa,
                         age_disability_la$disability_3_categories,
                         age_nssec_la$nssec_10_categories)
joint <- approximate_joint(empty_joint, targets)
```

