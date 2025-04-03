#' Summarize chain output from NIMBLE
#'
#' @param samples (list) output from NIMBLE
#' @param code (nimbleCode) output from nimble_code
#' @param data (list) data created and formatted for NIMBLE by data_for_NIMBLE()$data
#' @param constants (list) constants created and formatted for NIMBLE by data_for_NIMBLE()$constants
#' @param inits (function) created from nimble_inits() to set initial values for NIMBLE
#' @param project (numeric) the number of projections to calculate
#' @param WAIC
#' @param all.chains
#' @param coarse.grid
#' @param cutoff
#' @param block.out
#' @param gridkey
#' @param spatkey
#' @param SLURM
#'
#' @returns
#' @export
#'
#' @importFrom dplyr bind_rows mutate slice mutate inner_join select group_by ungroup left_join row_number
#' @importFrom coda as.mcmc
#' @importFrom parallel makeCluster parLapply stopCluster
#' @import nimble
#'
#' @examples


summarize_samples <- function(samples,
                              code,
                              data,
                              constants,
                              inits,
                              project = 0,
                              coarse.grid,
                              WAIC = F,
                              cutoff = 0,
                              block.out,
                              gridkey,
                              spatkey = NULL,
                              SLURM = F) {

  # Get WAIC ----
  if (WAIC == T) {
    cat("Assessing fit...\n")

    # get WAIC
    cat("Calculating WAIC...\n")
    mcmc <- lapply(samples, as.matrix)
    mcmc <- lapply(mcmc, as.data.frame)
    mcmc <- dplyr::bind_rows(mcmc)
    mcmc <- coda::as.mcmc(mcmc)

    # set up model
    mod <- nimble::nimbleModel(code = code,
                               name = "model",
                               constants = constants,
                               data = data,
                               inits = inits)

    mod1 <- nimble::compileNimble(mod)

    waic <- nimble::calculateWAIC(mcmc = mcmc, model = mod1)

  } else {
    waic <- NA
  }

  # Summarize chains in parallel ----

  # select which chains to use
  chains <- 1:length(samples)

  cat("Summarizing chains...\n")

  if (SLURM) {
    cores <- get_cpus_per_task() - 1
  } else {
    cores <- 5
  }

  start <- Sys.time()
  this_cluster <- parallel::makeCluster(cores)
  out <- parallel::parLapply(cl = this_cluster,
                             X = 1:ncol(samples[[1]]),
                             fun = chain_summary,
                             samples = samples,
                             chains = chains,
                             cutoff = cutoff)
  parallel::stopCluster(this_cluster)


  out <- dplyr::bind_rows(out)
  print(Sys.time() - start)

  cat('Chain summarization complete \n')

  dat <- list(out = out)

  # Reformat estimated parameters ----
  # lambda
  keep <- grep("lambda0", out$param)
  lambda0 <- out %>%
              dplyr::slice(keep) %>%
              dplyr::mutate(grid.id = as.numeric(gsub("lambda0", "", param)), block.out = block.out) %>%
              dplyr::inner_join(gridkey, by = "grid.id")  %>%
              dplyr::select(conus.grid.id, group, block.out, mean, lo, hi,
                            lotail, hitail,unc.range, unc.rel, rhat, ESS)

  dat$lambda0 <- lambda0

  # psi
  keep <- grep("psi0", out$param)
  psi0 <- out %>%
            dplyr::slice(keep) %>%
            dplyr::mutate(grid.id = as.numeric(gsub("psi0", "", param)), block.out = block.out) %>%
            dplyr::inner_join(gridkey, by = "grid.id") %>%
            dplyr::select(conus.grid.id, group, block.out, mean, lo, hi,
                          lotail, hitail, unc.range, unc.rel, rhat, ESS)

  dat$psi0 <- psi0

  # XB
  keep <- grep("XB0", out$param)
  XB0 <- out %>%
          dplyr::slice(keep) %>%
          dplyr::mutate(grid.id = as.numeric(gsub("XB0", "", param)), block.out = block.out) %>%
          dplyr::inner_join(gridkey, by = "grid.id") %>%
          dplyr::select(conus.grid.id, group, block.out, mean, lo, hi,
                        lotail, hitail, unc.range, unc.rel, rhat, ESS)

  dat$XB0 <- XB0

  if (project > 0) {
    for (z in 1:project) {
      # lambda
      keep <- grep(paste0("lambda", z), out$param)
      lambda0 <- out %>%
                  dplyr::slice(keep) %>%
                  dplyr::mutate(grid.id = as.numeric(gsub(paste0("lambda", z), "", param)), block.out = block.out) %>%
                  dplyr::inner_join(gridkey, by = "grid.id") %>%
                  dplyr::select(conus.grid.id, group, block.out, mean, lo, hi,
                                lotail, hitail, unc.range, unc.rel, rhat, ESS)

      dat[[paste0("lambda", z)]] <- lambda0

      # psi
      keep <- grep(paste0("psi", z), out$param)
      psi0 <- out %>%
                dplyr::slice(keep) %>%
                dplyr::mutate(grid.id = as.numeric(gsub(paste0("psi", z), "", param)), block.out = block.out) %>%
                dplyr::inner_join(gridkey, by = "grid.id") %>%
                dplyr::select(conus.grid.id, group, block.out, mean, lo, hi,
                              lotail, hitail, unc.range, unc.rel, rhat, ESS)

      dat[[paste0("psi", z)]] <- psi0

      # XB
      keep <- grep(paste0("XB", z), out$param)
      XB0 <- out %>%
              dplyr::slice(keep) %>%
              dplyr::mutate(grid.id = as.numeric(gsub(paste0("XB", z), "", param)), block.out = block.out) %>%
              dplyr::inner_join(gridkey, by = "grid.id") %>%
              dplyr::select(conus.grid.id, group, block.out, mean, lo, hi,
                            lotail, hitail, unc.range, unc.rel, rhat, ESS)

      dat[[paste0("XB", z)]] <- XB0
    }
  }

  if (coarse.grid == T) {
    key <- dplyr::left_join(gridkey, spatkey, by = 'conus.grid.id')
  } else {
    key <- dplyr::mutate(gridkey, spat.grid.id = grid.id)
  }

  # spat
  keep <- grep("spat", out$param)
  if (length(keep) > 0) {
    spat <- out %>%
              dplyr::slice(keep) %>%
              dplyr::mutate(grid.id = as.numeric(gsub("spat", "", param)), block.out = block.out) %>%
              dplyr::left_join(key, ., by = c("spat.grid.id" = "grid.id")) %>%
              dplyr::select(conus.grid.id, group, block.out, mean, lo, hi,
                            lotail, hitail, unc.range, unc.rel, rhat, ESS)

    dat$spat <- spat
  }

  # B
  keep <- grep("B", out$param)
  dontkeep <- grep("XB", out$param)
  keep <- keep[-which(keep %in% dontkeep)]
  process.coef <- out %>%
                  dplyr::slice(keep) %>%
                  dplyr::mutate(covariate = colnames(data$Xz), block.out = block.out) %>%
                  dplyr::select(covariate, block.out, mean, lo, hi, lotail, hitail,
                                unc.range, unc.rel, rhat, ESS)

  dat$process.coef <- process.coef

  # Dataset intercept
  datasets <- data.frame(
    name = unlist(constants[grep("name", names(constants))]),
    number = names(constants)[grep("name", names(constants))],
    ncov = names(constants)[grep("nCovW|nCovV|nCovY", names(constants))]
  ) %>%
    dplyr::mutate(
      type = substr(ncov, 5, 5),
      data.type = dplyr::case_when(type == "V" ~ "DND", type == "Y" ~ "Count", type == "W" ~ "PO"),
      number = gsub("name", "", number)
    ) %>%
    dplyr::select(name, number, data.type)

  # alpha
  keep <- grep("alpha", out$param)
  alpha <- out %>%
            dplyr::slice(keep) %>%
            dplyr::mutate(number = gsub("alpha", "", param), block.out = block.out) %>%
            dplyr::left_join(datasets, by = "number") %>%
            dplyr::select(name, data.type, block.out, mean, lo, hi, lotail, hitail,
                          unc.range, unc.rel, rhat, ESS)

  dat$alpha <- alpha

  # Dataset covariates
  Xs <- grep("X", names(data))
  rm <- grep("Xz", names(data)) # get rid of Xzs
  Xs <- Xs[-which(Xs %in% rm)]
  obs.covs <- c()
  for (x in 1:length(Xs)) {
    if (ncol(data[[Xs[x]]]) == 0)
      next

    df <- data.frame(
      number = substr(names(data)[Xs[x]], 3, nchar(names(data)[Xs[x]])),
      covariate = colnames(data[[Xs[x]]]),
      colnumber = 1:ncol(data[[Xs[x]]])
    )
    obs.covs <- dplyr::bind_rows(obs.covs, df)
  }

  keep <- grep("A|C|D", out$param)
  obs.coef <- out %>%
                dplyr::slice(keep) %>%
                dplyr::mutate(number = substr(param, 2, nchar(param) - 1)) %>%
                dplyr::group_by(number) %>%
                dplyr::mutate(colnumber = dplyr::row_number(), block.out = block.out) %>%
                dplyr::ungroup() %>%
                dplyr::left_join(datasets, by = "number") %>%
                dplyr::left_join(obs.covs, by = c("number", "colnumber")) %>%
                dplyr::select(name, data.type, covariate, block.out, mean, lo, hi,
                              lotail, hitail, unc.range, unc.rel, rhat, ESS)

  dat$obs.coef <- obs.coef

  # Return
  dat$waic <- waic

  return(dat)
}
