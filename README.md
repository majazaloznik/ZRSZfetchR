
# ZRSZfetchR

<!-- badges: start -->
[![R-CMD-check](https://github.com/majazaloznik/ZRSZfetchR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/majazaloznik/ZRSZfetchR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/majazaloznik/ZRSZfetchR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/majazaloznik/ZRSZfetchR?branch=main)
<!-- badges: end -->

The goal of ZRSZfetchR is to check for new ZRSZ data tables somewhere (online or on the network), import, and parse the data, and prepare it for the UMAR data model.


## Installation

You can install the development version of ZRSZfetchR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("majazaloznik/ZRSZfetchR")
```

## Parsers

The following parsers are available for publicly available Excel files on the ZRSZ website:

- BO: "Gibanje registrirane brezposelnosti po mesecih"
- BO-SO: "Stopnja registrirane brezposelnosti po občinah in statističnih regijah"
- BO_SLO_izob: "Gibanje registrirane brezposelnosti po mesecih - izobrazba"
- BO_SLO_starost: "Gibanje registrirane brezposelnosti po mesecih - starost"
- BO_SLO_trajanje: "Gibanje registrirane brezposelnosti po mesecih - trajanje"
- BO_SLO_spol: "Gibanje registrirane brezposelnosti po mesecih - spol"
- DN: "Število prejemnikov denarnega nadomestila"V
- TOK: "Novoprijavljene in odjavljene brezposelne osebe"
- DD: "Veljavna delovna dovoljenja"

Additionally, there are parsers for the MIN-1 and MIN-6 tables that are not publicly
available. 


