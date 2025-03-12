# check_progress_mcmc <- function(outfile, nchain, sleep_length = 5){
#
#   cat(paste0("File started at ", Sys.time(), "\n"))
#   setup <- TRUE
#   cat("Setting up data\n")
#   while(setup) {
#     if (file.exists(outfile) == T) {
#       setup <- FALSE
#       is_running <- TRUE
#     }
#     # Sys.sleep(5)
#     # next
#   }
#
#   cat("_______\n")
#   cat(paste0("Model started at ", Sys.time(), "\n"))
#   is_running <- TRUE
#   iter = 1
#   total_run <- (nchain * 57) - nchain * 2
#   pb <- txtProgressBar(0, total_run)
#
#   while(is_running){
#     tmp <- readLines(outfile, warn = FALSE)
#     tmp <- tmp[grep("^\\|-",tmp)]
#     if(length(tmp) == 0){
#       cat(
#         paste(
#           "Nimble model getting set up. Checking again in",
#           sleep_length * 6,"seconds...\n"
#         )
#       )
#       Sys.sleep(sleep_length * 6)
#       next
#     } else {
#       nran <- nchar(tmp[length(tmp)])
#       setTxtProgressBar(pb, nran)
#       Sys.sleep(sleep_length)
#
#       if(substr(tmp[length(tmp)],nran,nran) == "|"){
#         is_running <- FALSE
#         cat("\n")
#         cat("_______\n")
#         cat(paste0("Model is complete at ", Sys.time()))
#       }
#     }
#   }
# }
