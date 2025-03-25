
# SpFut.flexiSDM

<!-- badges: start -->
<!-- badges: end -->

The goal of SpFut.flexiSDM is to ...


## Functions

SpFut.flexiSDM contains the following functions:

### Helpers
- scale_this - DONE (ROM)
- get_cpus_per_task - DONE (ROM)
- id_dup_records - (CLS) cleaned, commented, and documented


### Setup range and spatial grid

- get_range - (CLS) cleaned and commented - COMPLETE (ROM)
- make_region - (CLS) cleaned and commented - COMPLETE (ROM)
- make_spatkey - NEEDS DOCUMENTATION (ROM)

### Prep covariate data

- load_covar - (CLS) cleaned, commented, and documented - this requires the species-futures directory right now. Revisit it.
- plot_covar - (CLS) cleaned, commented, and documented
- cor_covar - (CLS) cleaned, commented, and documented
- select_covar - (CLS) cleaned, commented, and documented
- add_int_cols - this is for interactions which we haven't actually done any of. Don't include it for now?

### Prep species data

- load_species_data - (CLS) cleaned, commented, and documented
- PO_filter - (CLS) this function is unnecessary and can be removed - DONE
- count_filter - (CLS) cleaned, commented, and documented
- DND_filter - (CLS) cleaned, commented, and documented 
- PO_for_nimble - (CLS) cleaned, commented, and documented
- survey_for_nimble - (CLS) cleaned, commented, and documented
- sppdata_for_nimble - (CLS) cleaned, commented, and documented

### Combine all data

- z_for_nimble - (CLS) cleaned, commented, and documented
- data_for_nimble - (CLS) cleaned, commented, and documented

### Set up NIMBLE

- nimble_code - (CLS) cleaned, commented, and documented
- nimble_inits - (CLS) cleaned, commented, and documented
- nimble_params - (CLS) cleaned, commented, and documented

### Run NIMBLE

- run_nimbleMCMC - (ROM) outline added
- nimbleParallel - (ROM) outline added
- check_progress_MCMC - (CLS) can delete

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
- map_species_data - (CLS) cleaned, commented, and documented



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

