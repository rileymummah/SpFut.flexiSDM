#' Get CPUs per task (SLURM)
#'
#' @returns The number of CPUs allocated to the SLURM task
#'

get_cpus_per_task <- function() {
  ncpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
  return(ncpus)
}
