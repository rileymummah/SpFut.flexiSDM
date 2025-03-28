# SpFut.flexiSDM

<!-- badges: start -->

<!-- badges: end -->

The goal of SpFut.flexiSDM is to ...

## Functions

SpFut.flexiSDM contains the following functions:

### Helpers

-   scale_this - DONE (ROM)
-   get_cpus_per_task - DONE (ROM)
-   id_dup_records - (CLS) cleaned, commented, and documented

### Setup range and spatial grid

-   get_range - COMPLETE (ROM)
-   make_region - COMPLETE (ROM)
-   make_spatkey - NEEDS DOCUMENTATION (ROM)

### Prep covariate data

-   load_covar - this requires the species-futures directory right now. Revisit it (CLS)
-   plot_covar - DONE
-   cor_covar - DONE
-   select_covar - DONE
-   add_int_cols - EXCLUDED FOR NOW

### Prep species data

-   load_species_data - DONE
-   PO_filter - REMOVED
-   count_filter - DONE
-   DND_filter - DONE
-   PO_for_nimble - DONE
-   survey_for_nimble - DONE
-   sppdata_for_nimble - (CLS) cleaned, commented, and documented
-   map_species_data - (CLS) cleaned, commented, and documented

### Combine all data

-   z_for_nimble - DONE
-   data_for_nimble - (CLS) cleaned, commented, and documented

### Set up NIMBLE

-   nimble_code - DONE
-   nimble_inits - DONE
-   nimble_params - DONE

### Run NIMBLE

-   run_nimbleMCMC - (ROM) outline added
-   nimbleParallel - (ROM) outline added
-   check_progress_MCMC - (CLS) can delete

### Process NIMBLE output

-   get_projections
-   get_derived
-   chain_summary
-   summarize_samples

### Plot NIMBLE output

-   plot_chains - (CLS) cleaned, commented, and documented
-   plot_posteriors - (CLS) cleaned, commented, and documented
-   plot_convergence - (CLS) cleaned, commented, and documented
-   plot_pars - (CLS) cleaned, commented, and documented
-   plot_effects - (CLS) cleaned, commented, and documented
-   plot_auc

## Installation

You can install the development version of SpFut.flexiSDM from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
remotes::install_github("rileymummah/SpFut.flexiSDM")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SpFut.flexiSDM)
## basic example code
```
