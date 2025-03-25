
# SpFut.flexiSDM

<!-- badges: start -->
<!-- badges: end -->

The goal of SpFut.flexiSDM is to ...


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
- plot_covar - (CLS) commmented - added documentation (CLS)
- cor_covar - (CLS) commented - added documentation (CLS)
- select_covar 
- add_int_cols

### Prep species data

- load_species_data - (CLS) cleaned and commented - NEEDS DOCUMENTATION (ROM) - (CLS) added documentation
- PO_filter - (CLS) this function is unnecessary and can be removed - DONE
- count_filter
- DND_filter - (CLS) cleaned and commented - NEEDS DOCUMENTATION (ROM) - (CLS) added documentation 
- PO_for_nimble - (CLS) cleaned and commented - NEEDS DOCUMENTATION (ROM) - (CLS) added documentation
- survey_for_nimble - (CLS) cleaned and commented - NEEDS DOCUMENTATION (ROM) - (CLS) added documentation
- sppdata_for_nimble - (CLS) halfway cleaned and commented (see comments in code)

### Combine all data

- z_for_nimble - (CLS) cleaned and commented - NEEDS DOCUMENTATION (ROM) - (CLS) added documentation
- data_for_nimble - (CLS) started this but it needs some work (see comments in code)

### Set up NIMBLE

- nimble_code - (CLS) cleaned, commented, and documented
- nimble_inits - (CLS) cleaned, commented, and documented
- nimble_params - (CLS) cleaned, commented, and documented

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

You can install the development version of SpFut.flexiSDM from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("rileymummah/SpFut.flexiSDM")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SpFut.flexiSDM)
## basic example code
```

