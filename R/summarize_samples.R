# summarize_samples <- function(samples, code, data, constants, inits, project = 0,
#                               WAIC = F, all.chains = T, coarse.grid,
#                               cutoff = 0, block.out, gridkey, spatkey = NULL,
#                               local) {
#
#   # Get WAIC ----
#   if (WAIC == T) {
#
#
#     cat("Assessing fit...\n")
#
#     # get WAIC
#     cat("Calculating WAIC...\n")
#     mcmc <- lapply(samples, as.matrix)
#     mcmc <- lapply(mcmc, as.data.frame)
#     mcmc <- bind_rows(mcmc)
#     mcmc <- coda::as.mcmc(mcmc)
#
#     # set up model
#     mod <- nimbleModel(code = code, name = "model", constants = constants,
#                        data = data, inits = inits)
#     mod1 <- nimble::compileNimble(mod)
#
#     waic <- nimble::calculateWAIC(mcmc = mcmc, model = mod1)
#
#   } else {
#     waic <- NA
#   }
#
#   # Summarize chains ----
#
#   # select which chains to use
#   if (all.chains == T) {
#     chains <- 1:length(samples)
#   } else {
#     bind <- grep("B", colnames(samples[[1]]))
#
#     chains <- c()
#     tmp1 <- as.data.frame(samples[[1]][,bind])
#     if (F %in% (colMeans(tmp1[,2:ncol(tmp1)]) < 4) == F &
#         F %in% (colMeans(tmp1[,2:ncol(tmp1)]) > -4) == F) {
#       chains <- c(chains, 1)
#     }
#     tmp2 <- as.data.frame(samples[[2]][,bind])
#     if (F %in% (colMeans(tmp2[,2:ncol(tmp2)]) < 4) == F &
#         F %in% (colMeans(tmp2[,2:ncol(tmp2)]) > -4) == F) {
#       chains <- c(chains, 2)
#     }
#     tmp3 <- as.data.frame(samples[[3]][,bind])
#     if (F %in% (colMeans(tmp3[,2:ncol(tmp3)]) < 4) == F &
#         F %in% (colMeans(tmp3[,2:ncol(tmp3)]) > -4) == F) {
#       chains <- c(chains, 3)
#     }
#   }
#
#   if (length(chains) == 0) chains <- 1:3
#
#
#   cat("Summarizing chains...\n")
#
#   # This is probably slower but avoids having a huge object
#   # out <- c()
#   # pb <- txtProgressBar(min = 0, max = length(1:ncol(samples[[1]])), initial = 0)
#
#   if (local == 1) {
#     cores <- 5
#   } else {
#     cores <- get_cpus_per_task() - 1
#   }
#
#   start <- Sys.time()
#   this_cluster <- parallel::makeCluster(cores)
#   out <- parallel::parLapply(cl = this_cluster,
#                              X = 1:ncol(samples[[1]]),
#                              fun = chain_summary,
#                              samples = samples,
#                              chains = chains,
#                              cutoff = cutoff)
#   parallel::stopCluster(this_cluster)
#
#   out <- bind_rows(out)
#   print(Sys.time() - start)
#   cat('Chain summarization complete \n')
#
#   dat <- list(out = out)
#
#   # Make out objects better ----
#   # lambda
#   keep <- grep("lambda0", out$param)
#   lambda0 <- out %>%
#     slice(keep) %>%
#     mutate(grid.id = as.numeric(gsub("lambda0", "", param)),
#            block.out = block.out) %>%
#     inner_join(gridkey, by = "grid.id")  %>%
#     select(conus.grid.id, group, block.out, mean, lo, hi,
#            lotail, hitail, unc.range, unc.rel, rhat, ESS)
#   dat$lambda0 <- lambda0
#
#   # psi
#   keep <- grep("psi0", out$param)
#   psi0 <- out %>%
#     slice(keep) %>%
#     mutate(grid.id = as.numeric(gsub("psi0", "", param)),
#            block.out = block.out) %>%
#     inner_join(gridkey, by = "grid.id") %>%
#     select(conus.grid.id, group, block.out, mean, lo, hi, lotail, hitail, unc.range, unc.rel, rhat, ESS)
#   dat$psi0 <- psi0
#
#   # XB
#   keep <- grep("XB0", out$param)
#   XB0 <- out %>%
#     slice(keep) %>%
#     mutate(grid.id = as.numeric(gsub("XB0", "", param)),
#            block.out = block.out) %>%
#     inner_join(gridkey, by = "grid.id") %>%
#     select(conus.grid.id, group, block.out, mean, lo, hi, lotail, hitail, unc.range, unc.rel, rhat, ESS)
#   dat$XB0 <- XB0
#
#   if (project > 0) {
#     for (z in 1:project) {
#       # lambda
#       keep <- grep(paste0("lambda", z), out$param)
#       lambda0 <- out %>%
#         slice(keep) %>%
#         mutate(grid.id = as.numeric(gsub(paste0("lambda", z), "", param)),
#                block.out = block.out) %>%
#         inner_join(gridkey, by = "grid.id")  %>%
#         select(conus.grid.id, group, block.out, mean, lo, hi, lotail, hitail, unc.range, unc.rel, rhat, ESS)
#       dat[[paste0("lambda", z)]] <- lambda0
#
#       # psi
#       keep <- grep(paste0("psi", z), out$param)
#       psi0 <- out %>%
#         slice(keep) %>%
#         mutate(grid.id = as.numeric(gsub(paste0("psi", z), "", param)),
#                block.out = block.out) %>%
#         inner_join(gridkey, by = "grid.id") %>%
#         select(conus.grid.id, group, block.out, mean, lo, hi, lotail, hitail, unc.range, unc.rel, rhat, ESS)
#       dat[[paste0("psi", z)]] <- psi0
#
#       # XB
#       keep <- grep(paste0("XB", z), out$param)
#       XB0 <- out %>%
#         slice(keep) %>%
#         mutate(grid.id = as.numeric(gsub(paste0("XB", z), "", param)),
#                block.out = block.out) %>%
#         inner_join(gridkey, by = "grid.id") %>%
#         select(conus.grid.id, group, block.out, mean, lo, hi, lotail, hitail, unc.range, unc.rel, rhat, ESS)
#       dat[[paste0("XB", z)]] <- XB0
#     }
#   }
#
#   if (coarse.grid == T) {
#     key <- left_join(gridkey, spatkey, by = 'conus.grid.id')
#   } else {
#     key <- gridkey %>%
#       mutate(spat.grid.id = grid.id)
#   }
#
#   keep <- grep("spat", out$param)
#   if (length(keep) > 0) {
#     spat <- out %>%
#       slice(keep) %>%
#       mutate(grid.id = as.numeric(gsub("spat", "", param)),
#              block.out = block.out) %>%
#       left_join(key,., by = c("spat.grid.id" = "grid.id")) %>%
#       select(conus.grid.id, spat.grid.id, group, block.out, mean, lo, hi, lotail, hitail, unc.range, unc.rel, rhat, ESS)
#
#     # if (coarse.grid = T)
#     dat$spat <- spat
#   }
#
#   # B
#   keep <- grep("B", out$param)
#   dontkeep <- grep("XB", out$param)
#   keep <- keep[-which(keep %in% dontkeep)]
#   process.coef <- out %>%
#     slice(keep) %>%
#     mutate(covariate = colnames(data$Xz),
#            block.out = block.out) %>%
#     select(covariate, block.out, mean, lo, hi, lotail, hitail, unc.range, unc.rel, rhat, ESS)
#   dat$process.coef <- process.coef
#
#   # Dataset intercept
#   datasets <- data.frame(name = unlist(constants[grep("name", names(constants))]),
#                          number = names(constants)[grep("name", names(constants))],
#                          ncov = names(constants)[grep("nCovW|nCovV|nCovY", names(constants))]) %>%
#     mutate(type = substr(ncov, 5, 5),
#            data.type = case_when(type == "V" ~ "DND",
#                                  type == "Y" ~ "Count",
#                                  type == "W" ~ "PO"),
#            number = gsub("name", "", number)) %>%
#     select(name, number, data.type)
#
#   keep <- grep("alpha", out$param)
#   alpha <- out %>%
#     slice(keep) %>%
#     mutate(number = gsub("alpha", "", param),
#            block.out = block.out) %>%
#     left_join(datasets, by = "number") %>%
#     select(name, data.type, block.out, mean, lo, hi, lotail, hitail, unc.range, unc.rel, rhat, ESS)
#   dat$alpha <- alpha
#
#   # Dataset covariates
#   Xs <- grep("X", names(data))
#   rm <- grep("Xz", names(data)) # get rid of Xzs
#   Xs <- Xs[-which(Xs %in% rm)]
#   obs.covs <- c()
#   for (x in 1:length(Xs)) {
#     if (ncol(data[[Xs[x]]]) == 0) next
#
#     df <- data.frame(number = substr(names(data)[Xs[x]], 3, nchar(names(data)[Xs[x]])),
#                      covariate = colnames(data[[Xs[x]]]),
#                      colnumber = 1:ncol(data[[Xs[x]]]))
#     obs.covs <- bind_rows(obs.covs, df)
#   }
#
#   keep <- grep("A|C|D", out$param)
#   obs.coef <- out %>%
#     slice(keep) %>%
#     mutate(number = substr(param, 2, nchar(param)-1)) %>%
#     group_by(number) %>%
#     mutate(colnumber = row_number(),
#            block.out = block.out) %>%
#     ungroup() %>%
#     left_join(datasets, by = "number") %>%
#     left_join(obs.covs, by = c("number", "colnumber")) %>%
#     select(name, data.type, covariate, block.out, mean, lo, hi,
#            lotail, hitail, unc.range, unc.rel, rhat, ESS)
#   dat$obs.coef <- obs.coef
#
#   # Return
#   dat$waic <- waic
#   return(dat)
# }
