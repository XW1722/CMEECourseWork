# CMEE 2022 HPC exercises R code pro forma
# For stochastic demographic model cluster run

# clear the workspace and turn off graphics
rm(list=ls())
graphics.off()

# load the functions needed by sourcing the main R code
source("/rds/general/user/xw1722/home/xw1722_HPC_2022_main.R")

# read in the job number from the cluster
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
# local testing
# iter <- 251

# random number seeds
set.seed(iter)

# select the initial condition being used
clutch_distribution <- c(0.06, 0.08, 0.13, 0.15, 0.16, 0.18, 0.15, 0.06, 0.03)
projection_matrix <- matrix(c(0.1, 0.6, 0.0, 0.0,
                            0.0, 0.4, 0.4, 0.0,
                            0.0, 0.0, 0.7, 0.25,
                            2.6, 0.0, 0.0, 0.4), nrow = 4, ncol = 4)
simulation_length <- 120
initial_list <- list(state_initialise_adult(4, 100),
                    state_initialise_adult(4, 10),
                    state_initialise_spread(4, 100),
                    state_initialise_spread(4, 10))
# getting the initial population vector for each quarter of simulations
iter_sets <- list(1:250, 251:500, 501:750, 751:1000)
for (i in 1:4){
    if (iter >= (min(as.numeric(iter_sets[[i]]))) &&
    iter <= (max(as.numeric(iter_sets[[i]])))){
        init_size <- initial_list[[i]]
    }
}

# filename to store the results
results <- paste("demographic_cluster_", iter, ".rda", sep = "")

# simulation and results saving
stochastic_simulation(init_size,
                    projection_matrix, 
                    clutch_distribution, simulation_length)
# save the results directly by the output of the function
save(pop_size, file = results)

