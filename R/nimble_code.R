#' Write code for nimble
#'
#' @description Write code for nimble based on contents of data and constants
#'
#' @param data (list) data formatted for nimble
#' @param constants (list) constants formatted for nimble
#' @param sp.auto (logical) whether model should have spatial model (T) or not (F)
#' @param coarse.grid (logical) whether model uses the coarse spatial grid (T) or not (F)
#' @param path (character) where to save the nimble code
#' @param min.visits.incl (numeric) minimum number of median visits per site (inclusive) to use occupancy or N-mixture model; default is 3
#' @param Bpriordist (character) distribution for beta priors; defaults to "dnorm"
#' @param Bpriorvar1 (numeric) first parameter for beta priors; defaults to 0
#' @param Bpriorvar2 (numeric) second parameter for beta priors; defaults to 1
#' @param block.out (character or numeric) which block is excluded
#' @param zero_mean (logical) whether spatial parameter should include zero_mean restriction (T) or not (F); defaults to T
#' @param rm.state (logical) whether to remove state-specific effort (T) or not (F); defaults to F
#' @param occupancy (logical) whether to use occupancy models (T) or replace them with Bernoullis (F)
#'
#' @returns nimble code object
#' @export
#'
#' @importFrom nimble nimbleCode
#'
#' @examples


nimble_code <- function(data,
                        constants,
                        sp.auto,
                        coarse.grid,
                        path,
                        min.visits.incl = 3,
                        Bpriordist = "dnorm",
                        Bpriorvar1 = 0,
                        Bpriorvar2 = 1,
                        block.out,
                        zero_mean = T,
                        rm.state = F,
                        occupancy = T) {

  if (occupancy == F) min.visits.incl <- Inf

# Set up main parts ----

## Process model ----
  pro.mod <- "
# Process Model

# _NCELLS cells
for (i in 1:nCell) {

    # log intensity
    XB0[i] <- inprod(B[1:nCovZ], Xz[i,1:nCovZ])
    log(lambda0[i]) <- XB0[i] # no residual spatial effect

}

"

  pro.mod <- gsub("_NCELLS", constants$nCell, pro.mod)




  ## Priors ----
  prior <- "
# Process priors
for (a in 1:nCovZ) {
  B[a] ~ _DIST(_PAR1,_PAR2)
}

# Dataset intercepts
for (a in 1:nD) {
  alpha[a] <- exp(w[a])
  w[a] ~ dnorm(0,1)
}"

prior <- gsub("_DIST", Bpriordist, prior)
prior <- gsub("_PAR1", Bpriorvar1, prior)
prior <- gsub("_PAR2", Bpriorvar2, prior)

## Observation models ----
obs.all <- ""

obs.mod <- "
# Observation Model _NUM: _TYPE, _NAME
# _NOBS observations, _NVIS median visits per site
for (j in _LOOP) {

  # Make dataset-specific lambda (and N and Z if needed)
  lambdaD_NUM[j] <- lambda0[_IND] * alpha[_NUM]

  # Observation model
  _OBS

  _DEin
}
_DEout
  "


# Now fill out each section for each dataset ----
for (d in 1:constants$nD) {

  name <- constants[[paste0("name", d)]]
  
  if (paste0("Wcells", d) %in% names(constants)) {
    type <- "PO"
    #loop <- paste0("asRow(Wcells", d, ")")
    #ind <- [j]
    loop <- "1:nW_NUM"
    ind <- "Wcells_NUM[j]"
    de <- "Effort"
    dore <- "E"
    ncov <- paste0("nCovW", d)
    nv <- 1
    nobs <- length(constants[[paste0("Wcells", d)]])
    letter <- "W"
    lowletter <- "w"
    param <- "A"
    state <- ifelse(paste0("states", d) %in% names(constants) & rm.state == F, T, F)

    mod <- "poisPO"
    obs <- "W_NUM[j] ~ dpois(lambdaD_NUM[j] * E_NUM[j]) # Poisson"
    
    if (name == "iNaturalist") {
      link <- "logit"
    } else {
      link <- "log"
    }

    obs.mod1 <- gsub("# _NOBS observations, _NVIS median visits per site", paste0("# ", nobs, " cells"), obs.mod)


  } else if (paste0("Vcells", d) %in% names(constants)) {
    type <- "DND"
    loop <- paste0("1:nV", d)
    ind <- "Vcells_NUM[j]"
    de <- "Detection"
    dore <- "p"
    ncov <- paste0("nCovV", d)
    nv <- constants[[paste0("nVisitsV", d)]]
    nobs <- constants[[paste0("nV", d)]]
    letter <- "V"
    lowletter <- "v"
    param <- "D"
    state <- F

    if (constants[[paste0("nVisitsV", d)]] < min.visits.incl) {
      mod <- "bern1"
      obs <- "V_NUM[j] ~ dbern(1-exp(-lambdaD_NUM[j] * p_NUM[j])) # Bernoulli"
      link <- "log"
    } else {
      mod <- "bern2"
      obs <- "V_NUM[j] ~ dbern(ZD_NUM[j] * p_NUM[j]) # Occupancy"
      link <- "logit"
    }

    obs.mod1 <- obs.mod

  } else if (paste0("Ycells", d) %in% names(constants)) {
    type <- "count"
    loop <- paste0("1:nY", d)
    ind <- "Ycells_NUM[j]"
    de <- "Detection"
    dore <- "p"
    ncov <- paste0("nCovY", d)
    nv <- constants[[paste0("nVisitsY", d)]]
    nobs <- constants[[paste0("nY", d)]]
    letter <- "Y"
    lowletter <- "y"
    param <- "C"
    state <- F

    if (constants[[paste0("nVisitsY", d)]] < min.visits.incl) {
      mod <- "poisC"
      obs <- "Y_NUM[j] ~ dpois(lambdaD_NUM[j] * p_NUM[j]) # Poisson"
      link <- "log"
    } else {
      mod <- "binom"
      obs <- "Y_NUM[j] ~ dbinom(p_NUM[j], ND_NUM[j]) # N-mixture"
      link <- "logit"
    }

    obs.mod1 <- obs.mod

  } else {
    next
  }



  obs.mod1 <- gsub("_TYPE", type, obs.mod1)
  obs.mod1 <- gsub("_NAME", name, obs.mod1)
  obs.mod1 <- gsub("_LOOP", loop, obs.mod1)
  obs.mod1 <- gsub("_IND", ind, obs.mod1)
  obs.mod1 <- gsub("_OBS", obs, obs.mod1)
  obs.mod1 <- gsub("_NVIS", nv, obs.mod1)
  obs.mod1 <- gsub("_NOBS", nobs, obs.mod1)



  # Fill in ND and ZD if needed
  if (mod == "binom") {
    obs.mod1 <- gsub("_ND", "\n  ND_NUM[j] ~ dpois(lambdaD_NUM[j])", obs.mod1)
    obs.mod1 <- gsub("_ZD", "", obs.mod1)
  } else if (mod == "bern2") {
    obs.mod1 <- gsub("_ND", "\n  ND_NUM[j] ~ dpois(lambdaD_NUM[j])", obs.mod1)
    obs.mod1 <- gsub("_ZD", "\n  ZD_NUM[j] <- step(ND_NUM[j] - 0.0000001)", obs.mod1)
  } else {
    obs.mod1 <- gsub("_ND", "", obs.mod1)
    obs.mod1 <- gsub("_ZD", "", obs.mod1)
  }



  # Fix detection/effort equation
  if (name == "iNaturalist") {
    # not fixed, goes on the inside
    obs.mod1 <- gsub("_DEout", "", obs.mod1)
    obs.mod1 <- gsub("_DEin", "# _LABDE \n  _EQN", obs.mod1)
    
    eqn <- "_LINK(eff_NUM[j]) <- _PARAM_NUM[1] * X_LOWLETTER_NUM[j,1]
  E_NUM[j] <- eff_NUM[j] * S_NUM[j]

  # Prior for X imputation
  X_LOWLETTER_NUM[j, 1] ~ dnorm(0, 1)"
    
    prior1 <- "
# Observation priors, _TYPE _NUM: _NAME
for (b in 1:nCov_LETTER_NUM) {
  _PARAM_NUM[b] ~ dnorm(0,1)
}"
    
  } else if (constants[[ncov]] == 0) {
    # fixed, goes on the outside

    # This will only ever happen when link == log, because if link == logit,
    # there is an intercept so ncov >= 1

    obs.mod1 <- gsub("_DEout", "# _LABDE \n_EQN", obs.mod1)
    obs.mod1 <- gsub("_DEin", "", obs.mod1)

    eqn <- "log(p_NUM) <- 0"

    prior1 <- ""



    obs.mod1 <- gsub("p_NUM[j]", "p_NUM", obs.mod1, fixed = T)



  } else if (constants[[ncov]] == 1) {

    if (link == "log") {
      # not fixed, goes on the inside
      obs.mod1 <- gsub("_DEout", "", obs.mod1)
      obs.mod1 <- gsub("_DEin", "# _LABDE \n  _EQN", obs.mod1)
      
        eqn <- "_LINK(_DORE_NUM[j]) <- _PARAM_NUM[1] * X_LOWLETTER_NUM[j,1]

  # Prior for X imputation
  X_LOWLETTER_NUM[j, 1] ~ dnorm(0, 1)"
      
      prior1 <- "
# Observation priors, _TYPE _NUM: _NAME
for (b in 1:nCov_LETTER_NUM) {
  _PARAM_NUM[b] ~ dnorm(0,1)
}"
    } else if (link == "logit") {

      # if ncov = 1 and link = logit, the only covariate is an intercept
      # so it is fixed and goes on the outside

      obs.mod1 <- gsub("_DEout", "# _LABDE \n_EQN", obs.mod1)
      obs.mod1 <- gsub("_DEin", "", obs.mod1)


      eqn <- "logit(p_NUM) <- _PARAM_NUM[1] * 1"

      prior1 <- "
# Observation priors, _TYPE _NUM: _NAME
for (b in 1) {
  _PARAM_NUM[b] ~ dnorm(0,1)
}"
      obs.mod1 <- gsub("p_NUM[j]", "p_NUM", obs.mod1, fixed = T)


    }



  } else {
    # not fixed, goes on the inside
    obs.mod1 <- gsub("_DEout", "", obs.mod1)
    obs.mod1 <- gsub("_DEin", "# _LABDE \n  _EQN", obs.mod1)

    if (state == T) {
      #eqn <- "_LINK(_DORE_NUM[j]) <- inprod(_PARAM_NUM[1:nCov_LETTER_NUM], X_LOWLETTER_NUM[j,1:nCov_LETTER_NUM]) * S_NUM[j]"
      eqn <- "_LINK(eff_NUM[j]) <- inprod(_PARAM_NUM[1:nCov_LETTER_NUM], X_LOWLETTER_NUM[j,1:nCov_LETTER_NUM])
  _DORE_NUM[j] <- eff_NUM[j] * S_NUM[j]"


    } else {
      eqn <- "_LINK(_DORE_NUM[j]) <- inprod(_PARAM_NUM[1:nCov_LETTER_NUM], X_LOWLETTER_NUM[j,1:nCov_LETTER_NUM])

  # Prior for X imputation
  for (c in 1:nCov_LETTER_NUM) {
    X_LOWLETTER_NUM[j,c] ~ dnorm(0, 1)
  }"
    }

    prior1 <- "
# Observation priors, _TYPE _NUM: _NAME
for (b in 1:nCov_LETTER_NUM) {
  _PARAM_NUM[b] ~ dnorm(0,1)
}"

  }
  obs.mod1 <- gsub("_EQN", eqn, obs.mod1)
  obs.mod1 <- gsub("_LABDE", de, obs.mod1)
  obs.mod1 <- gsub("_LETTER", letter, obs.mod1)
  obs.mod1 <- gsub("_LOWLETTER", lowletter, obs.mod1)
  obs.mod1 <- gsub("_DORE", dore, obs.mod1)
  obs.mod1 <- gsub("_LINK", link, obs.mod1)
  obs.mod1 <- gsub("_PARAM", param, obs.mod1)
  obs.mod1 <- gsub("_NUM", d, obs.mod1)

  obs.all <- paste0(obs.all, "\n", obs.mod1)


  prior1 <- gsub("_PARAM", param, prior1)
  prior1 <- gsub("_NUM", d, prior1)
  prior1 <- gsub("_LETTER", letter, prior1)
  prior1 <- gsub("_TYPE", type, prior1)
  prior1 <- gsub("_NAME", name, prior1)

  prior <- paste0(prior, "\n", prior1)
}

# Add spatial component ----

if (sp.auto == T) {

  if (zero_mean == T && coarse.grid == T) {
    pro.mod <- gsub("# no residual spatial effect", "+ spat[spatCells[i]] # residual spatial effect", pro.mod, fixed = T)
    prior <- paste0(prior, "\n\ntau <- 1\nspat[1:nSpatCell] ~ dcar_normal(adj[1:L], weights[1:L], num[1:nSpatCell], tau, zero_mean = 1)")
    # prior <- paste0(prior, "\n\ntau ~ dnorm(1, 0.01)\nspat[1:nCell] ~ dcar_normal(adj[1:L], weights[1:L], num[1:nCell], tau, zero_mean = 1)")

  } else if (zero_mean == T && coarse.grid == F) {
    pro.mod <- gsub("# no residual spatial effect", "+ spat[i] # residual spatial effect", pro.mod, fixed = T)
    prior <- paste0(prior, "\n\ntau <- 1\nspat[1:nCell] ~ dcar_normal(adj[1:L], weights[1:L], num[1:nCell], tau, zero_mean = 1)")
    # prior <- paste0(prior, "\n\ntau ~ dnorm(1, 0.01)\nspat[1:nCell] ~ dcar_normal(adj[1:L], weights[1:L], num[1:nCell], tau, zero_mean = 1)")

  } else if (zero_mean == F && coarse.grid == T) {
    pro.mod <- gsub("# no residual spatial effect", "+ spat[spatCells[i]] # residual spatial effect", pro.mod, fixed = T)
    prior <- paste0(prior, "\n\ntau <- 1\nspat[1:nSpatCell] ~ dcar_normal(adj[1:L], weights[1:L], num[1:nSpatCell], tau)")
    # prior <- paste0(prior, "\n\ntau ~ dnorm(1, 0.01)\nspat[1:nCell] ~ dcar_normal(adj[1:L], weights[1:L], num[1:nCell], tau, zero_mean = 1)")

  } else {
    pro.mod <- gsub("# no residual spatial effect", "+ spat[i] # residual spatial effect", pro.mod, fixed = T)
    prior <- paste0(prior, "\n\ntau <- 1\nspat[1:nCell] ~ dcar_normal(adj[1:L], weights[1:L], num[1:nCell], tau)")
    # prior <- paste0(prior, "\n\ntau ~ dnorm(1, 0.01)\nspat[1:nCell] ~ dcar_normal(adj[1:L], weights[1:L], num[1:nCell], tau)")
  }

}





# Combine ----
breaker <- "\n# ---------------------------------------"

full.mod <- paste0(pro.mod, breaker, obs.all, breaker, prior, "\n\n")


cat(full.mod)

if (block.out == "none") {
  blockname <- "full"
} else {blockname <- block.out}


# write r script and save to wd
code <- c("code <- nimbleCode({\n", full.mod, "})")
writeLines(code, paste0(path, "model", blockname, ".R"))

# source r script
code <- source(paste0(path, "model", blockname, ".R"))

return(code$value)


}
