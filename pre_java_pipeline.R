# Analysis pipeline pre-java with time estimates ----
# using Rscript to run scripts in an isolated environment
library(cli)
library(here)

# Helper function to run scripts in isolated environment
source("R/utils.R")

# 1. Clean raw data from Wise Monkey (takes abt 2 mins)
run_script("01_Data_Cleaning.R")

# 2. Generate step length & dispersal distribution estimates (takes abt 1.5 mins)
run_script("02_Make_Step_Length_Distribution.R")

# 3. Generate incubation & infectious period estimates (takes abt 30 secs)
run_script("03_Make_Incubation_Infectious_Periods.R")

# 4. Run transmission trees (this should take abt 4 minutes, longest step)
run_script("04_build_trees.R")

# 5. Make the list of incursions for java (20 secs, outs a plot to Rplots.pdf)
run_script("05_Make_Incursions_For_Java.R")

# 6. Make the overall case list for java (1 min, outs a plot to Rplots.pdf)
run_script("06_Make_Case_List.R")
