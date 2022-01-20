run_script <- function(path) {
  message(cli::col_cyan(paste(path, "is beginning:")))
  out <- system.time({
    err <- suppressWarnings(
      system2(command = "Rscript", args = here::here(path), 
                   stdout = TRUE, stderr = TRUE)
    )
  })
  
  if(!is.null(attr(err, "status"))) {
    message(cli::col_red("There was an error in the script!"))
    message(cat(err, sep = "\n"))
  } else {
    message(cli::col_cyan(paste(path, "completed in", round(out["elapsed"]/60, 4), "minutes.")))
  }
}

