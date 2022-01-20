# Analysis pipeline pre-java with time estimates ----

library(cli)
library(here)

# Helper function to run scripts in isolated environment
source("R/utils.R")

# set this to false if you want to run the heavy steps
fast <- FALSE

to_run <- list.files("trees_se", full.names = TRUE, recursive = TRUE)
to_run <- to_run[grep(".R", to_run, fixed = TRUE)]
to_run <- to_run[-grep("pipeline.R", to_run, fixed = TRUE)] # don't run this script will make infinite loop!

if(fast) {
  # step 1 takes about 3 hrs!
  to_run <- to_run[-grep("01|03", to_run)]
}

# Run the scripts in order ----
# (will take 1 - 2 hours depending on cores & memory
# especially if fast = FALSE & running steps 1 & 3, then may take abt 8 hrs!)
system.time(lapply(to_run, run_script))

# to just run the figures run this ----
# the animation script to generate the gif + movie takes a bit
# system.time(lapply(to_run[grep("fig_scripts", to_run, fixed = TRUE)], 
#                    run_script))

