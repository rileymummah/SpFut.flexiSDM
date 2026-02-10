#' Get CPUs per task (SLURM)
#'
#' @description
#' Returns the number of CPUs available on a SLURM-based system. It is only used when running the code in parallel on a SLURM-based high performance computing cluster.
#'
#' @returns The number of CPUs allocated to the SLURM task


get_cpus_per_task <- function() {
  ncpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
  return(ncpus)
}
