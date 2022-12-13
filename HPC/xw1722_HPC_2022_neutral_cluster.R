# CMEE 2022 HPC exercises R code pro forma
# For neutral model cluster run

# clear the workspace and turn off graphics
rm(list=ls())
graphics.off()

# load the functions needed by sourcing the main R code
source("xw1722_HPC_2022_main.R")

# read in the job number from the cluster
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
# local testing
# iter <- 1

# random number seeds
set.seed(iter)

# select the community size being used
sizes <- c(500, 1000, 2500, 5000)
iter_sets <- list(1:25, 26:50, 51:75, 76:100)
for (i in 1:4){
    if (iter >= (min(as.numeric(iter_sets[[i]]))) &&
    iter <= (max(as.numeric(iter_sets[[i]])))){
        size <- sizes[i]
    }
}

# filename to store the results
results <- paste("neutral_cluster_", iter, ".rda", sep = "")

# simulation and results saving
neutral_cluster_run(speciation_rate = 0.3,
    size, wall_time = 690,
    interval_rich = 1, interval_oct = size/10,
    burn_in_generations = 8 * size,
    output_file_name = results)
