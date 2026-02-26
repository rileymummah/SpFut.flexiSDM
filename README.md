# SpFut.flexiSDM

<!-- badges: start -->

<!-- badges: end -->

# Abstract

The purpose of this R software package is to build and fit an integrated species distribution model. Specifically, this package loads species and covariate data, builds a species region, sets up code, parameters, and specification for a NIMBLE model, and processes and plots NIMBLE output. This framework allows the inclusion of any number of count, detection/non-detection, and presence-only species datasets. Dataset-specific observation models account for any sampling biases the data introduce.

# Overview

SpFut.flexiSDM contains the following functions:

### To setup the species region and spatial grid

- `get_range`
- `get_state_grid`
- `make_region`
- `make_spatkey`

### To select and plot covariate data

- `select_covar`
- `cor_covar`
- `plot_covar`

### Prep species data

- `make_CV_blocks`
- `load_species_data`
- `count_filter`
- `DND_filter`
- `map_species_data`

### To combine all data for NIMBLE

- `add_state_ind`
- `data_for_nimble`
- `PO_for_nimble`
- `sppdata_for_nimble`
- `survey_for_nimble`
- `z_for_nimble`

### To set up NIMBLE code, initial values, and parameters

- `nimble_code`
- `nimble_inits`
- `nimble_params`

### To process NIMBLE MCMC output

- `get_AUC`
- `get_projections`
- `get_derived`
- `chain_summary`
- `summarize_samples`

### To plot NIMBLE output

- `plot_auc`
- `plot_chains`
- `plot_convergence`
- `plot_effects`
- `plot_pars`
- `plot_posteriors`

### Other helper functions

- `get_cpus_per_task`
- `scale_this`
- `id_dup_records`



## Installation

You can install the development version of SpFut.flexiSDM from [GitLab](https://code.usgs.gov/) with:

``` r
# install.packages("SpFut.flexiSDM")
remotes::install_git("code.usgs.gov/SpFut-flexiSDM")
```
# Usage

See the vignette for a demonstration on how to use the functions in the `SpFut.flexiSDM` package.

# Authors

- C. Lane Scher, Department of Ecosystem Science and Management, Pennsylvania State University; USGS Eastern Ecological Science Center Visiting Scientist - cls7052@psu.edu
- Riley O. Mummah, USGS Eastern Ecological Science Center - rmummah@usgs.gov
- David A.W. Miller,  Department of Ecosystem Science and Management, Pennsylvania State University - dxm84@psu.edu


# Citation

Please use the following citation when using this software:


# Acknowledgments

Funding was provided by the USGS Amphibian Research Monitoring Initiative (ARMI). Any use of trade, firm, or product names is for descriptive purposes only and does not imply endorsement by the U.S. Government.
