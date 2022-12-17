# CMEE 2022 HPC exercises R code main pro forma
# You don't HAVE to use this but it will be very helpful.
# If you opt to write everything yourself from scratch please ensure you use
# EXACTLY the same function and parameter names and beware that you may lose
# marks if it doesn't work properly because of not using the pro-forma.

name <- "Xuan Wang"
preferred_name <- "Xuan Wang"
email <- "xuan.wang22@imperial.ac.uk"
username <- "xw1722"

# Please remember *not* to clear the workspace here, or anywhere in this file.
# If you do, it'll wipe out your username information that you entered just
# above, and when you use this file as a 'toolbox' as intended it'll also wipe
# away everything you're doing outside of the toolbox.  For example, it would
# wipe away any automarking code that may be running and that would be annoying!

# Question 1
species_richness <- function(community){
  return(length(unique(community)))
}

# Question 2
init_community_max <- function(size){
  return(seq(from = 1, to = size))
}

# Question 3
init_community_min <- function(size){
  return(rep(1, times = size))
}

# Question 4
choose_two <- function(max_value){
  x <- seq(from = 1, to = max_value)
  return(sample(x, size = 2, replace = FALSE))
}

# Question 5
neutral_step <- function(community){
  # randomly pick two individuals from the community vector
  ran <- choose_two(length(community))
  # replace
  community[ran[1]] <- community[ran[2]]
  return(community)
}

# Question 6
neutral_generation <- function(community){
  # check if the number of individuals is even
  if (length(community) %% 2 == 0){
    for (i in 1:(length(community) / 2)){
      community <- neutral_step(community)
    }
  } else {
    # randomly round up or down
    n <- floor(length(community) / 2) + sample(0:1, size = 1)
    for (i in 1:n){
      community <- neutral_step(community)
    }
  }
  return(community)
}

# Question 7
neutral_time_series <- function(community,duration){
  richness <- c()
  richness[1] <- species_richness(community)
  for (i in 2:(duration + 1)){
    # current community state vector
    community <- neutral_generation(community)
    richness[i] <- species_richness(community)
  }
  return(richness)
}

# Question 8
question_8 <- function() {
  # initial conditions set up
  community <- init_community_max(100)
  duration <- 200
  # compute the richness
  richness <- neutral_time_series(community, duration)

  png(filename="question_8.png", width = 600, height = 400)
  # plot your graph here
  plot(richness, main = "Neutral model", 
        xlab = "generations", ylab = "Species richness")
  Sys.sleep(0.1)
  dev.off()
  return("The system will always converge to 1. 
  This is because that when the number of a species increases while there is 
  no speciation, it will increase the probability of reproduction of this
  species. Therefore, the system will end in having one single species.")
}

# Question 9
neutral_step_speciation <- function(community, speciation_rate){
  i <- choose_two(length(community))
  rate <- runif(1, min = 0, max = 1)
  if (rate < speciation_rate){
    # replace with speciation
    community[i[1]] <- max(community) + 1
  } else {
    # replace without speciation
    community[i[1]] <- community[i[2]]
  }
  return(community)
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate = 0.3)  {
  # check if the number of individuals is even
  if (length(community) %% 2 == 0){
    for (i in 1:(length(community) / 2)){
      community <- neutral_step_speciation(community, speciation_rate)
    }
  } else {
    # randomly round up or down
    n <- floor(length(community) / 2) + sample(0:1, size = 1)
    for (i in 1:n){
      community <- neutral_step_speciation(community, speciation_rate)
    }
  }
  return(community)
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  richness <- c()
  # species richness of intial condition community
  richness[1] <- species_richness(community)
  for (i in 2:(duration + 1)){
    community <- neutral_generation_speciation(community, speciation_rate)
    richness[i] <- species_richness(community)
  }
  return(richness)
}

# Question 12
question_12 <- function()  {
  # initialise
  speciation_rate <- 0.1
  community_max <- init_community_max(100)
  community_min <- init_community_min(100)
  duration <- 200
  # simulations
  richness_max <- neutral_time_series_speciation(community_max, 
                                                speciation_rate, duration)
  richness_min <- neutral_time_series_speciation(community_min, speciation_rate,
                                                duration)

  png(filename="question_12.png", width = 600, height = 400)
  # plot your graph here
  plot(richness_max, type = "l", col = "blue", 
    main = "Neutral theory simulation with speciation",
    xlab = "Generation", ylab = "Species richness")
  lines(richness_min, col = "red")
  legend("topright", legend = c("max_initial_condition", 
    "min_initial_condition"), fill = c("blue", "red"))
  Sys.sleep(0.1)
  dev.off()

  return("The initial condition of number of species does not have a great 
  influence on the final convergence, given that the number of generation 
  and the speciation rate is held constant. 
  This is because the convergence of the species richness is mainly determined
  by the rate of extinction and the rate of speciation. Since the initial rate
  of speciation is the same, it means that when equilibrium is reached, the 
  level of equilibrium would be similar.
  Though the initial number of species is different, this is not a factor of the
  species richness.")
}

# Question 13
species_abundance <- function(community)  {
  return(as.numeric(sort(table(community), decreasing = TRUE)))
}

# Question 14
octaves <- function(abundance_vector) {
  # transforms the expression
  new_vector <- floor(log(abundance_vector) / log(2) + 1)
  return(tabulate(new_vector))
}

# Question 15
sum_vect <- function(x, y) {
  if (length(x) > length(y)) {
    zeros <- rep(0, times = length(x) - length(y))
    y <- c(y, zeros)
  } else if (length(x) < length(y)) {
    zeros <- rep(0, times = length(y) - length(x))
    x <- c(x, zeros)
  }
  sum <- x + y
  return(sum)
}

# Question 16 
question_16 <- function() {

  # setting up the initial state
  graphics.off()
  count <- 1
  community_max <- init_community_max(100)
  community_min <- init_community_min(100)
  sum_max <- c()
  sum_min <- c()

  for (i in 1:2200){
    # updating the community for current generation
    community_max <- neutral_generation_speciation(
      community = community_max,
      speciation_rate = 0.1
    )
    community_min <- neutral_generation_speciation(
      community = community_min,
      speciation_rate = 0.1
    )
    # record the species abundance octave vector after the burn-in period
    if (count == 200){
      octave_max <- octaves(species_abundance(community_max))
      octave_min <- octaves(species_abundance(community_min))
      sum_max <- sum_vect(sum_max, octave_max)
      sum_min <- sum_vect(sum_min, octave_min)
    } else if (count > 200 & count <= 2200) {
      # record every 20 generations
      if ((count - 200) %% 20 == 0){
        octave_max <- octaves(species_abundance(community_max))
        octave_min <- octaves(species_abundance(community_min))
        sum_max <- sum_vect(sum_max, octave_max)
        sum_min <- sum_vect(sum_min, octave_min)
      }
    }
    count <- count + 1
  }
  # calculation of the mean
  mean_max <- sum_max / 101
  mean_min <- sum_min / 101

  # naming the columns
  names(mean_max) <- c("1", "2~3", "4~7", "8~15", "16~31", "32~63")
  names(mean_min) <- c("1", "2~3", "4~7", "8~15", "16~31", "32~63")

  png(filename="question_16_min.png", width = 600, height = 400)
  barplot(mean_min, xlab = "Number of individuals per species", 
    ylab = "Number of species",
    main = "Mean species abundance with minimum initial condition")
  Sys.sleep(0.1)
  dev.off()

  png(filename="question_16_max.png", width = 600, height = 400)
  barplot(mean_max, xlab = "Number of individuals per species",
    ylab = "Number of species",
    main = "Mean species abundance with maximum initial condition")
  Sys.sleep(0.1)
  dev.off()
  
  return("The initial condition does not matter.
  Though slight difference exist between the two graphs, the general pattern
  of the distribution is the same.
  The initial species richness is not a factor of the species abundance.
  No matter how large the initial species richness is, the species are equally
  likely to replicate or die.
  When the abundance is small, individuals are likely to replicate; when the 
  abundance is great enough, individuals will be likely to die.")
}

# Question 17
neutral_cluster_run <- function(speciation_rate = 0.3, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name) {
  
  # starting community
  community <- init_community_min(size)

  # neutral generations for predefined amount of computing time
  start_time <- as.numeric(proc.time()[3]) / 60
  computing_time <- 0
  count <- 1
  time_series <- c(species_richness(community))
  abundance_list <- c(list(octaves(species_abundance(community))))

  while (computing_time <= wall_time){
    # current community
    community <- neutral_generation_speciation(community, 0.3) 
    
    # storing the species richness and abundances during burn-in
    if ((count <= burn_in_generations) && (count %% interval_rich == 0)){
      # species richness
      time_series <- c(time_series, species_richness(community))
    }
    # record species abundances every interval_oct generations
    if (count %% interval_oct == 0){
      abundance_list <- c(abundance_list, list(octaves(species_abundance(community))))
    }
    current_time <- as.numeric(proc.time()[3]) / 60
    computing_time <- current_time - start_time
    count <- count + 1
  }

  # total amount of time
  total_time <- current_time - start_time

  #saving the simulation results
  save(time_series, abundance_list, community, total_time, speciation_rate,
  size, wall_time, interval_rich, interval_oct, burn_in_generations,
  file = output_file_name)
}

# Questions 18 and 19 involve writing code elsewhere to run your simulations on
# the cluster

# Question 20 
process_neutral_cluster_results <- function() {
  # initialise
  mean_octave_1 <- list()
  mean_octave_2 <- list()
  mean_octave_3 <- list()
  mean_octave_4 <- list()
  sum_size_1 <- vector()
  sum_size_2 <- vector()
  sum_size_3 <- vector()
  sum_size_4 <- vector()
  # for size 500
  for (i in 1:25){
    # initialise
    sum_octave_1 <- 0
    # read in all the output files
    load(paste("neutral_cluster_", i, ".rda", sep = ""))

    # To determine the number of records during burn-in generations,
    # we divide the burn-in generations by the inerval_oct,
    # which should always be 80.
    # Determine the data used (exclude the first 80, which is during
    # burn-in generations)
    abundance_data <- abundance_list[81:length(abundance_list)]

    # calculate the sum for each octave
    for (j in 1:(length(abundance_data))){
      sum_octave_1 <- sum_vect(sum_octave_1, abundance_data[[j]])
    }
    # mean for each octave
    mean_octave_1[[i]] <- sum_octave_1 / length(abundance_data)

    # sum for this community size
    sum_size_1 <- sum_vect(sum_size_1, mean_octave_1[[i]])
  }
  # calculate the mean for this community size
  mean_size_500 <- sum_size_1 / 25

  # repeat the same procedure for size 1000
  for (i in 26:50){
    sum_octave_2 <- 0
    load(paste("neutral_cluster_", i, ".rda", sep = ""))

    abundance_data <- abundance_list[81:length(abundance_list)]

    for (j in 1:(length(abundance_data))){
      sum_octave_2 <- sum_vect(sum_octave_2, abundance_data[[j]])
    }
    mean_octave_2[[i-25]] <- sum_octave_2 / length(abundance_data)

    sum_size_2 <- sum_vect(sum_size_2, mean_octave_2[[i-25]])
  }
  mean_size_1000 <- sum_size_2 / 25

  # size 2500
  for (i in 51:75){
    sum_octave_3 <- 0
    load(paste("neutral_cluster_", i, ".rda", sep = ""))

    abundance_data <- abundance_list[81:length(abundance_list)]

    for (j in 1:(length(abundance_data))){
      sum_octave_3 <- sum_vect(sum_octave_3, abundance_data[[j]])
    }
    mean_octave_3[[i-50]] <- sum_octave_3 / length(abundance_data)

    sum_size_3 <- sum_vect(sum_size_3, mean_octave_3[[i-50]])
  }
  mean_size_2500 <- sum_size_3 / 25

  # size 5000
  for (i in 76:100){
    sum_octave_4 <- 0
    load(paste("neutral_cluster_", i, ".rda", sep = ""))

    abundance_data <- abundance_list[81:length(abundance_list)]

    for (j in 1:(length(abundance_data))){
      sum_octave_4 <- sum_vect(sum_octave_4, abundance_data[[j]])
    }
    mean_octave_4[[i-75]] <- sum_octave_4 / length(abundance_data)

    sum_size_4 <- sum_vect(sum_size_4, mean_octave_4[[i-75]])
  }
  mean_size_5000 <- sum_size_4 / 25

  # save the results to an .rda file
  combined_results <- list(mean_size_500, mean_size_1000,
                    mean_size_2500, mean_size_5000) #create your list output here to return
  save(combined_results, file = "neutral_cluster_results.rda")
}

plot_neutral_cluster_results <- function(){
  # load combined_results from your rda file
  load("neutral_cluster_results.rda")
  sizes <- c(500, 1000, 2500, 5000)

  png(filename="plot_neutral_cluster_results.png", width = 600, height = 400)
  par(mfrow = c(2, 2))
  for (i in 1:4){
    pics <- combined_results[[i]]
    names(pics) <- c("1", "2~3", "4~7", "8~15", "16~31", "32~63")
    barplot(pics, 
            main = paste("Mean abundance octave for size ", sizes[i], sep = ""),
            xlab = "Number of individuals", ylab = "Number of species")
  }
  Sys.sleep(0.1)
  dev.off()
  
  return(combined_results)
}


# Question 21
state_initialise_adult <- function(num_stages,initial_size){
  return(c(rep(0, times = num_stages-1), initial_size))
}

# Question 22
state_initialise_spread <- function(num_stages,initial_size){
  if (initial_size %% num_stages == 0){
    vec <- c(rep(initial_size / num_stages, times = num_stages))
  } else {
    n <- initial_size %% num_stages
    s <- floor(initial_size / num_stages)
    vec <- c(rep(s + 1, times = n), rep(s, times = (num_stages - n)))
  }
  return(vec)
}

# Question 23
deterministic_step <- function(state,projection_matrix){
  return(projection_matrix %*% state)
}

# Question 24
deterministic_simulation <- function(initial_state,projection_matrix,simulation_length){
  population_size <- vector()
  # the first entry is the initial population size
  population_size[1] <- sum(initial_state)
  # initialising
  state <- initial_state
  for (i in 1:simulation_length){
    # current state vector
    state <- deterministic_step(state, projection_matrix)
    population_size[i + 1] <- sum(state)
  }
  return(population_size)
}

# Question 25
question_25 <- function(){
  simulation_length <- 24
  projection_matrix <- matrix(c(0.1, 0.6, 0.0, 0.0,
                                0.0, 0.4, 0.4, 0.0,
                                0.0, 0.0, 0.7, 0.25,
                                2.6, 0.0, 0.0, 0.4), nrow = 4, ncol = 4)
  init_adults <- state_initialise_adult(num_stages = 4, initial_size = 100)
  init_spread <- state_initialise_spread(num_stages = 4, initial_size = 100)
  series_adults <- deterministic_simulation(init_adults, projection_matrix, simulation_length)
  series_spread <- deterministic_simulation(init_spread, projection_matrix, simulation_length)
  data <- rbind(series_adults, series_spread)
  png(filename="question_25.png", width = 600, height = 400)
  plot(series_adults, type = "l", col = "red", xlab = "Time step", ylab = "Population size")
  lines(series_spread, col = "#46469a")
  legend("topright", fill = c("red", "#46469a"), legend = c("adults", "spread"))
  Sys.sleep(0.1)
  dev.off()
  
  return("In general, an spread distribution of population in different stages
  can lead to a smaller eventual population growth. An uneven distribution of
  initial population which only gathers in the final stage can have a greater
  population growth. Though the initial population growth of both situations are
  similar, the difference increases at the end. This is because if all the
  individuals are mature, there will be greater number of recruitment, which
  causes a larger population size in general.")
}

# Question 26
trinomial <- function(pool,probs) {
  # checks whether probs[1]==1
  if (probs[1] ==1){
    result <- c(pool, 0, 0)
  } else {
    # the number of individuals assgined to the first event
    individuals_first <- rbinom(n = 1, size = pool, prob = probs[1])
    # the number of individuals left in the pool
    remaining_pool <- pool - individuals_first
    # conditional prob of the second event given the first didn't occur
    prob_event_2 <- probs[2] / (1 - probs[1])
    # the number of individuals assigned to the second event
    individuals_second <- rbinom(n = 1, size = remaining_pool,
                                prob = prob_event_2)
    # the number of individuals assigned to the third event
    individuals_third <- pool - individuals_first - individuals_second
    result <- c(individuals_first, individuals_second, individuals_third)
  }
  return(result)
}

# Question 27
survival_maturation <- function(state,projection_matrix) {
  # intialise new state vector
  new_state <- rep(0, times = length(state))
  # loop through all life stages except the final stage
  for (i in 1:(length(state) - 1)){
    # individuals in the current life stage
    pops <- state[i]
    # derive the probability vector
    prob_stay <- projection_matrix[i, i]
    prob_tran <- projection_matrix[i + 1, i]
    prob <- c(prob_stay, prob_tran)
    # apply the trinomial function to derive vector of possible outcomes
    t <- trinomial(pops, prob)
    # individuals remain in stage i
    pops_remain <- t[1]
    # individuals transition to stage i+1
    pops_trans <- t[2]
    # add the entries to new_state
    new_state[i] <- new_state[i] + pops_remain
    new_state[i+1] <- new_state[i+1] + pops_trans
  }
  # individuals survive in the final life stage
  pops_final <- tail(state, n = 1)
  pops_survive <- rbinom(n = 1, size = pops_final,
                  prob = projection_matrix[length(state), length(state)])
  new_state[length(state)] <- new_state[length(state)] + pops_survive
  return(new_state)
}

# Question 28
random_draw <- function(probability_distribution) {
  number <- runif(n = 1, min = 0, max = 1)
  cumulate <- cumsum(probability_distribution)
  for (i in 1:(length(probability_distribution-1))){
    if (cumulate[1] >= number){
      value <- 1
    } else if (cumulate[i] <= number && cumulate[i+1] >= number){
      value <- i+1
    } 
  }
  return(value)
}

# Question 29
stochastic_recruitment <- function(projection_matrix,clutch_distribution){
  # expected mean clutch size
  expectation <- (seq(from = 1, 
                  to = length(clutch_distribution))) %*% 
                  clutch_distribution
  # recuitment probability
  prob <- projection_matrix[1, sqrt(length(projection_matrix))] / expectation
  return(as.numeric(prob))
}

# Question 30
offspring_calc <- function(state,clutch_distribution,recruitment_probability){
  # initialise
  size <- vector()
  # adults in state
  adults <- tail(state, n = 1)
  # number of adults which recruit
  adults_recruit <- rbinom(n = 1, size = adults, prob = recruitment_probability)
  # draw the clutch size for each clutch
  if (adults_recruit == 0){
    total_offspring <- 0
  } else {
    for (i in seq(from = 1, to = adults_recruit)){
      size[i] <- random_draw(clutch_distribution)
    }
    total_offspring <- sum(size)
  }
  return(total_offspring)
}

# Question 31
stochastic_step <- function(state,projection_matrix,clutch_distribution,recruitment_probability){
  new_state <- survival_maturation(state, projection_matrix)
  new_state[1] <- new_state[1] + offspring_calc(state,
          clutch_distribution, recruitment_probability)
  return(new_state)
}

# Question 32
stochastic_simulation <- function(initial_state,projection_matrix,clutch_distribution,simulation_length){
  # individual recruitment probability
  indiv_recruit <- stochastic_recruitment(projection_matrix, clutch_distribution)
  # initialising
  state <- initial_state
  population_size <- vector()
  # first entry which is the initial population size
  population_size[1] <- sum(initial_state)
  for (i in 1:simulation_length){
    # current state vector
    state <- stochastic_step(state, projection_matrix, clutch_distribution,
                            indiv_recruit)
    population_size[i + 1] <- sum(state)
    if (population_size[i] == 0){
      population_size <- c(population_size, rep(0, times = simulation_length - i))
      break
    }
  }
  pop_size <<- population_size
  return(population_size)
}

# Question 33
question_33 <- function(){
  # setting the parameters
  clutch_distribution <- c(0.06, 0.08, 0.13, 0.15, 0.16, 0.18, 0.15, 0.06, 0.03)
  projection_matrix <- matrix(c(0.1, 0.6, 0.0, 0.0,
                                0.0, 0.4, 0.4, 0.0,
                                0.0, 0.0, 0.7, 0.25,
                                2.6, 0.0, 0.0, 0.4), nrow = 4, ncol = 4)
  simulation_length <- 24
  init_adults <- state_initialise_adult(num_stages = 4, initial_size = 100)
  init_spread <- state_initialise_spread(num_stages = 4, initial_size = 100)
  # stochastic simulation
  simu_adults <- stochastic_simulation(init_adults, projection_matrix,
                                      clutch_distribution, simulation_length)
  simu_spread <- stochastic_simulation(init_spread, projection_matrix,
                                      clutch_distribution, simulation_length)
  # plotting graphs
  png(filename="question_33.png", width = 600, height = 400)
  plot(simu_adults, type = "l", col = "red", 
      xlab = "Time step", ylab = "Population size",
      main = "Population size variation with stochastic simulation")
  lines(simu_spread, col = "#46469a")
  legend("topright", fill = c("red", "#46469a"), legend = c("adults", "spread"))
  Sys.sleep(0.1)
  dev.off()
  
  return("The previous graph has a much smoother pattern compared to the 
  stochastic simulations. This is because there are a lot of uncertainties
  not taken into account in the deterministic approach, while it happens in
  reality, leading to greater fluctuations. This is taken into account in the
  stochastic simulation, which explains why the stochastic simulation shows a
  less smooth pattern in general.")
}

# Questions 34 and 35 involve writing code elsewhere to run your simulations on the cluster

# Question 36
question_36 <- function(){
  # first initial condition
  count_1 <- 0
  for (i in 1:250){
    # read in the output files
    load(paste("demographic_cluster_", i, ".rda", sep = ""))
    # work out how many extinctions occurred for this initial condition
    if (tail(pop_size, 1) == 0){
      count_1 <- count_1 + 1
    }
  }
  # compute the proportion of simulations resulting in extinction
  extinct_1 <- count_1 / 250

  # similar procedure applied to the results by other initial conditions
  count_2 <- 0
  for (i in 251:500){
    load(paste("demographic_cluster_", i, ".rda", sep = ""))
    if (tail(pop_size, 1) == 0){
      count_2 <- count_2 + 1
    }
  }
  extinct_2 <- count_2 / 250

  count_3 <- 0
  for (i in 501:750){
    load(paste("demographic_cluster_", i, ".rda", sep = ""))
    if (tail(pop_size, 1) == 0){
      count_3 <- count_3 + 1
    }
  }
  extinct_3 <- count_3 / 250

  count_4 <- 0
  for (i in 751:1000){
    load(paste("demographic_cluster_", i, ".rda", sep = ""))
    if (tail(pop_size, 1) == 0){
      count_4 <- count_4 + 1
    }
  }
  extinct_4 <- count_4 / 250

  extinctions <- c(extinct_1, extinct_2, extinct_3, extinct_4)
  names(extinctions) <- c("large_adults", "small_adults",
                          "large_spread", "small_spread")

  png(filename="question_36.png", width = 600, height = 400)
  barplot(extinctions, xlab = "initial conditions", ylab = "extinction rate",
              main = "Extinction proportion with different initial conditions")
  Sys.sleep(0.1)
  dev.off()
  
  return("A population with a small and spread population is the most likely
  to go extinct. This is because a larger population can lead to greater number
  of adults in general, which means that there is a greater chance for 
  recruitment. Smaller initial population means that there is less adults,
  and hence less recruitment. Similarly, when the species have spread 
  population, there is less adults, leading to smaller possibility of recruit-
  ment. By contrast, species with greater number of adults are generally less
  likely to go extinct.")
}

# Question 37
question_37 <- function(){
  # set up the initial conditions
  clutch_distribution <- c(0.06, 0.08, 0.13, 0.15, 0.16, 0.18, 0.15, 0.06, 0.03)
  projection_matrix <- matrix(c(0.1, 0.6, 0.0, 0.0,
                                0.0, 0.4, 0.4, 0.0,
                                0.0, 0.0, 0.7, 0.25,
                                2.6, 0.0, 0.0, 0.4), nrow = 4, ncol = 4)
  simulation_length <- 120
  init_large <- state_initialise_spread(num_stages = 4, initial_size = 100)
  init_small <- state_initialise_spread(num_stages = 4, initial_size = 10)
  population_stochastic_large <- 0
  population_stochastic_small <- 0

  # stochastic model
  # initial condition 3 - large spread population
  for (i in (501:750)){
    load(paste("demographic_cluster_", i, ".rda", sep = ""))
    # identify the population size results and sum them up
    population_stochastic_large <- population_stochastic_large + pop_size
  }
  # derive the mean population size at each step
  mean_stochastic_large <- population_stochastic_large / 250

  # initial condition 4 - small spread population
  for (i in (751:1000)){
    load(paste("demographic_cluster_", i, ".rda", sep = ""))
    population_stochastic_small <- population_stochastic_small + pop_size
  }
  mean_stochastic_small <- population_stochastic_small / 250

  # deterministic model
  # initial condition 3 & 4
  deterministic_large <- deterministic_simulation(init_large,
                                    projection_matrix, simulation_length)
  deterministic_small <- deterministic_simulation(init_small,
                                    projection_matrix, simulation_length)
  
  png(filename="question_37_small.png", width = 600, height = 400)
  # plot your graph for the small initial population size here
  plot(mean_stochastic_small, type = "l", col = "#ff7b00",
      main = "Comparison between models with small initial population size",
      xlab = "Time step", ylab = "Population")
  lines(deterministic_small, col = "#6327ef")
  legend("topright", fill = c("#ff7b00", "#6327ef"), 
      legend = c("stochastic model", "deterministic model"))
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="question_37_large.png", width = 600, height = 400)
  # plot your graph for the large initial population size here
  plot(mean_stochastic_large, type = "l", col = "#ff7b00",
      main = "Comparison between models with large initial population size",
      xlab = "Time step", ylab = "Population")
  lines(deterministic_large, col = "#6327ef")
  legend("topright", fill = c("#ff7b00", "#6327ef"), 
      legend = c("stochastic model", "deterministic model"))
  Sys.sleep(0.1)
  dev.off()
  
  return("When the initial population size is large, it is appropriate to
  use a deterministic model to approximate the behavior of an 'averaged'
  stochastic system. The difference between the stochastic and deterministic
  models are larger if the initial population size is small. When the initial 
  popoulation size is small, the standard error could be greater which means 
  that the average may deviate, leading to an inaccuracy in the deterministic
  model.")
}



# Challenge questions - these are optional, substantially harder, and a maximum
# of 14% is available for doing them. 

# Challenge question A
Challenge_A <- function() {
  # loading required package
  require(ggplot2)

  # initialising
  count <- 0 # count for the number of loops
  richness_count <- 0 # count for the number of records of species richness
  speciation_rate <- 0.1
  mean_richness_max <- 100
  mean_richness_min <- 0

  community_max <- init_community_max(100)
  community_min <- init_community_min(100)
  sum_richness_max <- 0
  sum_richness_min <- 0
  standard_deviation <- 1

  # deriving the mean species richness
  for (i in 1:2200){
    # update the current community for each loop
    community_max <- neutral_generation_speciation(
      community = community_max,
      speciation_rate = 0.1
    )
    community_min <- neutral_generation_speciation(
      community = community_min,
      speciation_rate = 0.1
    )
    
    # record the initial species richness after the burn-in period
    if (count == 200){
      richness_count <- richness_count + 1
      mean_richness_max[richness_count] <- species_richness(community_max)
      mean_richness_min[richness_count] <- species_richness(community_min)
      # Calculating the lower and upper bound of the 97.2% confidence interval
      error <- qnorm(0.972) * standard_deviation / richness_count

      upper_max <- mean_richness_max[richness_count] + error
      upper_min <- mean_richness_min[richness_count] + error
      lower_max <- mean_richness_min[richness_count] - error
      lower_min <- mean_richness_min[richness_count] - error
    } else if (count > 200 && count <= 2200) {
      # record every 20 generations
      if ((count - 200) %% 20 == 0){
        richness_count <- richness_count + 1
        # calculate the current richness
        richness_max <- species_richness(community_max)
        richness_min <- species_richness(community_min)
        # calculate the sum and the mean
        sum_richness_max <- sum(sum_richness_max, richness_max)
        sum_richness_min <- sum(sum_richness_min, richness_min)
        mean_richness_max[richness_count] <- sum_richness_max / richness_count
        mean_richness_min[richness_count] <- sum_richness_min / richness_count
        # Calculating the lower and upper bound of the 97.2% confidence interval
        error <- qnorm(0.972) * standard_deviation / richness_count

        upper_max <- c(upper_max, mean_richness_max[richness_count] + error)
        upper_min <- c(upper_min, mean_richness_min[richness_count] + error)
        lower_max <- c(lower_max, mean_richness_min[richness_count] - error)
        lower_min <- c(lower_min, mean_richness_min[richness_count] - error)
      }
    }
    count <- count + 1
  }
  
  # combining the results to a dataframe
  df_max <- data.frame(mean_richness_max, upper_max, lower_max)
  df_min <- data.frame(mean_richness_min, upper_min, lower_min)

  # plotting the graph
  png(filename="Challenge_A_min.png", width = 600, height = 400)
  # plot your graph here
  print(ggplot(data = df_min, aes(x = seq(1, 100), y = mean_richness_min)) +
          geom_point() +
          geom_errorbar(aes(ymin = lower_min, ymax = upper_min)) +
          theme_bw() +
          labs(x = "Time", y = "Mean species richness", 
              title = "Mean species richness against time"))
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="Challenge_A_max.png", width = 600, height = 400)
  # plot your graph here
  print(ggplot(data = df_max, aes(x = seq(1, 100), y = mean_richness_max)) +
          geom_point() +
          geom_errorbar(aes(ymin = lower_max, ymax = upper_max)) +
          theme_bw() +
          labs(x = "Time", y = "Mean species richness", 
              title = "Mean species richness against time"))
  Sys.sleep(0.1)
  dev.off()

  return("In general, in the graph generated, it needs around 25 turns to reach
  the equilibrium, which is 25 * 20(record interval) + 200(burn-in period) = 700
  generations.")
}

# Challenge question B
Challenge_B <- function() {
  # load required package for plotting
  require(ggplot2)

  # initialising
  community <- vector()
  averaged_df <- data.frame()

  for (i in 1:100){
    # initialise for each loop
    richness <- vector()
    averaged_richness <- vector()

    # computing the initial community with size 100
    community <- sample(x = 1:100, size = i, replace = FALSE)
    richness[1] <- i
    averaged_richness[1] <- i

    # generating the whole range averaged time series for 200 generations
    for (j in 1:200){
      # getting the current community
      if (length(community) > 1){
        community <- neutral_generation_speciation(community,
                                                  speciation_rate = 0.1)
      }
      # calculate the current species richness
      richness[j+1] <- species_richness(community)
      # calculate the averaged species richness for each generation
      averaged_richness[j+1] <- sum(richness) / (j+1)
    }

    # creating a dataframe of averaged time series for each initial community
    averaged_each <- data.frame(rep(i, 201), seq(1, 201, by = 1), 
                                averaged_richness)
    names(averaged_each) <- c("initial_richness", "generations", 
                                "average_richness")
    averaged_df <- rbind(averaged_df, averaged_each)
  }
  
  png(filename="Challenge_B.png", width = 600, height = 400)
  # plot your graph here
  as.factor(averaged_df$initial_richness)
  print(ggplot(averaged_df, aes(x = generations, y = average_richness)) + 
          geom_line(data = averaged_df, aes(x = generations, y = average_richness,
                    group = initial_richness, col = initial_richness)) +
          theme_bw() +
          labs(x = "Time (number of generations)", y = "Averaged richness",
              title = "Averaged time series for a whole range of different intial species richness"))
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question C
Challenge_C <- function() {
  # initialise
  richness_each_1 <- vector()
  richness_each_2 <- vector()
  richness_each_3 <- vector()
  richness_each_4 <- vector()
  richness_mean <- vector()

  # load the results for each size
  for (i in 1:25){
    load(paste("neutral_cluster_", i, ".rda", sep = ""))

    # calculating the mean species richness across each simulation
    richness_each_1[i] <- species_richness(community)
    richness_mean[i] <- sum(richness_each_1) / i
  }
  for (i in 26:50){
    load(paste("neutral_cluster_", i, ".rda", sep = ""))

    # calculating the mean species richness across each simulation
    richness_each_2[i-25] <- species_richness(community)
    richness_mean[i] <- sum(richness_each_2) / (i-25)
  }
  for (i in 51:75){
    load(paste("neutral_cluster_", i, ".rda", sep = ""))

    # calculating the mean species richness across each simulation
    richness_each_3[i-50] <- species_richness(community)
    richness_mean[i] <- sum(richness_each_3) / (i-50)
  }
  for (i in 76:100){
    load(paste("neutral_cluster_", i, ".rda", sep = ""))

    # calculating the mean species richness across each simulation
    richness_each_4[i-75] <- species_richness(community)
    richness_mean[i] <- sum(richness_each_4) / (i-75)
  }

  # creating a column to represent the different size
  size <- c(rep(500, times = 25), rep(1000, times = 25),
            rep(2500, times = 25), rep(5000, times = 25))
  # creating a dataframe
  richness_df <- data.frame(size, richness_mean)

  png(filename="Challenge_C.png", width = 600, height = 400)
  # plot your graph here
  plot(subset(richness_df, size =="500")$richness_mean, 
      col = "#9a3838", ylim = c(0, 2600),
      xlab = "Generation", ylab = "Mean species richness",
      main = "The mean species richness against simulation length")
  points(subset(richness_df, size =="1000")$richness_mean, col = "#2f2f6f")
  points(subset(richness_df, size =="2500")$richness_mean, col = "#b77a09")
  points(subset(richness_df, size =="5000")$richness_mean, col = "#157115")
  legend("topright", fill = c("#9a3838", "#2f2f6f", "#b77a09", "#157115"),
          legend = c("Size 500", "Size 1000", "Size 2500", "Size 5000"))
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question D
Challenge_D <- function() {
  
  # initialising
  lineages <- rep(1, times = J)
  abundances <- vector()
  N <- J
  
  png(filename="Challenge_D", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question E
# defines the function of deriving the mean life stage at certain time
mean_step <- function(state){
  # initialize
  sum_pop <- 0
  # summing up the population times i
  for (i in 1:length(state)){
    sum_pop <- sum_pop + i * state[i]
  }
  # divide by the sum of population in each state to derive 
  # the mean population state
  mean <- sum_pop / sum(state)
  return(mean)
}

# defining the simulation function which outputs a mean stage time series
mean_simulation <- function(initial_state, projection_matrix, clutch_distribution, simulation_length){
  # individual recruitment probability
  indiv_recruit <- stochastic_recruitment(projection_matrix, clutch_distribution)
  # initialising
  state <- initial_state
  mean_stage <- vector()
  # the first entry which is the initial mean lifestage
  mean_stage[1] <- mean_step(state)
  # loop through each time step and record the corresponding mean life stage
  for (i in 1:simulation_length){
    # current state vector
    state <- stochastic_step(state, projection_matrix, clutch_distribution,
                              indiv_recruit)
    mean_stage[i+1] <- mean_step(state)
  }
  return(mean_stage)
}

Challenge_E <- function() {
  # setting the parameters
  clutch_distribution <- c(0.06, 0.08, 0.13, 0.15, 0.16, 0.18, 0.15, 0.06, 0.03)
  projection_matrix <- matrix(c(0.1, 0.6, 0.0, 0.0,
                                0.0, 0.4, 0.4, 0.0,
                                0.0, 0.0, 0.7, 0.25,
                                2.6, 0.0, 0.0, 0.4), nrow = 4, ncol = 4)
  simulation_length <- 24
  init_adults <- state_initialise_adult(num_stages = 4, initial_size = 100)
  init_spread <- state_initialise_spread(num_stages = 4, initial_size = 100)

  # generating the simulations
  mean_simu_adults <- mean_simulation(init_adults, projection_matrix,
                                      clutch_distribution, simulation_length)
  mean_simu_spread <- mean_simulation(init_spread, projection_matrix,
                                      clutch_distribution, simulation_length)
  png(filename="Challenge_E.png", width = 600, height = 400)
  # plot your graph here
  plot(mean_simu_adults, type = "l", col = "red", 
      xlab = "Time step", ylab = "Mean life stage",
      main = "Comparison of mean life stage with different initial population state")
  lines(mean_simu_spread, col = "#46469a")
  legend("topright", fill = c("red", "#46469a"), legend = c("adults", "spread"))
  Sys.sleep(0.1)
  dev.off()
  
  return("The simulation with an initial adult population have a much greater
  variation than the simulation with an initial mixed population at first,
  and the behavior of the simulations converges to a similar pattern at the
  end. This is because that at the beginning, the simulation with initial
  adult populations have greater number of adults. However, the number of
  adults will decrease by death and number of population in other life stages
  will increase by recruitment, and therefore number of new-born population
  will greatly increase, leading to a small mean; at the end, the population
  will be spreaded by maturation, death, recruitment, hence will converge 
  with the simulation with initial mixed population.")
}

# Challenge question F
Challenge_F <- function(){
  # loading the package
  require(ggplot2)

  # initialising
  simulation_size <- 120
  time_step <- vector()
  initial_condition <- vector()
  population_size <- vector()
  population_size_df <- data.frame()

  iter_sets <- list(1:250, 251:500, 501:750, 751:1000)
  conditions <- c("large_adult", "small_adult", "large_mix", "small_mix")

  # loop through the 1000 simulations
  for (i in 1:1000){
    # load the .rda ouput files
    load(paste("demographic_cluster_", i, ".rda", sep = ""))
    # loop through each simulation size
    for (j in 0:simulation_size){
      # check which initial condition it has
      for (m in 1:4){
        if (i >= (min(as.numeric(iter_sets[[m]]))) &&
            i <= (max(as.numeric(iter_sets[[m]])))){
          initial_condition[j+1] <- conditions[[m]]
        }
      }
      # record the simulation number
      simulation_number[j+1] <- i
      time_step[j+1] <- j
      population_size[j+1] <- pop_size[j+1]
    }
    # create a dataframe for each loop!
    df <- data.frame(simulation_number, initial_condition, 
                      time_step, population_size)
    # appending the overall dataframe
    population_size_df <- rbind(population_size_df, df)
  }
  png(filename="Challenge_F.png", width = 600, height = 400)
  # plot your graph here
  print(ggplot(data = population_size_df, aes(x = time_step, y = population_size)) +
            geom_line(data = population_size_df, aes(x = time_step, y = population_size,
                      group = simulation_number, colour = initial_condition),
                      alpha = 0.1) +
            theme_bw() +
            labs(x = "Time step for each simulation", y = "Population size",
                title = "Cluster simulation results of the population size for different initial conditions"))
  Sys.sleep(0.1)
  dev.off()
  
}
