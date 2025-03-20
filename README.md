
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SpFut.flexiSDM

<!-- badges: start -->
<!-- badges: end -->

The goal of SpFut.flexiSDM is to …

## Functions

SpFut.flexiSDM contains the following functions:

### Helpers

- get_range - (CLS) cleaned and commented
- make_region - Is this actually a helper? I’d move it to a different
  section (ROM); Yes I could agree (CLS) - (CLS) cleaned and commented
- fix_codes
- scale_this - (ROM) documented
- id_dup_records
- get_covs.PO
- make_spatkey - Is this actually a helper? I’d move it to a different
  section (ROM)
- get_cpus_per_task - (ROM) documented

### Prep covariate data

- load_covar
- plot_covar
- cor_covar
- select_covar
- add_int_cols

### Prep species data

- load_species_data
- PO_filter
- count_filter
- DND_filter
- PO_for_nimble
- survey_for_nimble
- sppdata_for_nimble

### Combine all data

- z_for_nimble
- data_for_nimble

### Set up NIMBLE

- nimble_code
- nimble_inits
- nimble_params

### Run NIMBLE

- run_nimbleMCMC - (ROM) outline added
- nimbleParallel - (ROM) outline added
- check_progress_MCMC

### Process NIMBLE output

- get_projections
- get_derived
- chain_summary
- summarize_samples

### Plot NIMBLE output

- plot_chains
- plot_convergence
- plot_pars
- plot_effects
- plot_auc
- map_species_data

## Installation

You can install the development version of SpFut.flexiSDM from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("rileymummah/SpFut.flexiSDM")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SpFut.flexiSDM)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
