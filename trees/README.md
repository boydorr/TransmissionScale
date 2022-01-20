## Running transmission tree sensitivity analysis for Mancy et al.

### To reproduce results ---

```
source("trees_se/tree_se_pipeline.R")

```

Note that this could take a few hours depending on the number of cores and memory
you have.

These steps rely on the data cleaning steps in `pre_java_pipeline.R` 
having run successfully. 

To just reproduce the figures in `trees_se/tree_se_pipeline.R`,
uncomment lines 26 - 27, and comment out line 22. 

To run all the steps including generating the full sensitivity analysis for the 
transmission tree reconstruction (warning: this will take a long! time, 
maybe ~ 6 hrs w/ 3 cores) in `trees_se/tree_se_pipeline.R` set `fast` to `FALSE`. 