
# SpFut.flexiSDM

<!-- badges: start -->
<!-- badges: end -->

The goal of SpFut.flexiSDM is to …

## Functions

SpFut.flexiSDM contains the following functions:

### Helpers
- scale_this - DONE (ROM)
- get_cpus_per_task - DONE (ROM)

- id_dup_records


### Setup range and spatial grid

- get_range - (CLS) cleaned and commented - COMPLETE (ROM)
- make_region - (CLS) cleaned and commented - COMPLETE (ROM)
- make_spatkey - NEEDS DOCUMENTATION (ROM)

### Prep covariate data

- load_covar - I'm not sure we need this anymore (CLS)
- plot_covar - (CLS) commmented
- cor_covar - (CLS) commented
- select_covar
- add_int_cols

### Prep species data

- load_species_data - (CLS) cleaned and commented - NEEDS DOCUMENTATION (ROM)
- PO_filter - (CLS) this function is unnecessary and can be removed - DONE
- count_filter
- DND_filter - (CLS) cleaned and commented - NEEDS DOCUMENTATION (ROM)
- PO_for_nimble - (CLS) cleaned and commented - NEEDS DOCUMENTATION (ROM)
- survey_for_nimble - (CLS) cleaned and commented - NEEDS DOCUMENTATION (ROM)
- sppdata_for_nimble - (CLS) halfway cleaned and commented

### Combine all data

- z_for_nimble - (CLS) cleaned and commented - NEEDS DOCUMENTATION (ROM)
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
