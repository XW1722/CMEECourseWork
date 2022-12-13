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
  return(sample.int(max_value, size = 2, replace = FALSE))
}

# Question 5
neutral_step <- function(community){
  i <- choose_two(length(community))
  community[i[1]] <- community[i[2]]
  return(community)
}

# Question 6
neutral_generation <- function(community){
  for (each in 1:length(community)/2){
    community <- neutral_step(community)
  }
  return(community)
}

# Question 7
neutral_time_series <- function(community,duration){
  richness <- c()
  richness[1] <- length(unique(community))
  for (i in 2:duration){
    community <- neutral_generation(community) # updating the current community for each loop
    richness[[i]] <- length(unique(community))
  }
  return(richness)
}

# Question 8
question_8 <- function() {
  x <- seq(from = 1, to = 200)
  richness <- neutral_time_series(community = init_community_max(100), duration = 200)
  png(filename="question_8.png", width = 600, height = 400)
  # plot your graph here
  plot(x, richness)
  Sys.sleep(0.1)
  dev.off()
  return("The system will always converge to 1. This is because that when the number of a species increases while there is no speciation, it will increase the probability of reproduction of this species. Therefore, the system will end in having one single species.")
}

# Question 9
neutral_step_speciation <- function(community,speciation_rate){
  i <- choose_two(length(community))
  rate <- runif(1, min = 0, max = 1)
  if (rate < speciation_rate){
    community[i[1]] <- max(community) + 1
  } else {
    community[i[1]] <- community[i[2]]
  }
  return(community)
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
  for (each in 1:length(community)/2){
    community <- neutral_step_speciation(community, speciation_rate)
  }
  return(community)
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  richness <- c()
  richness[1] <- length(unique(community))
  for (i in 2:duration){
    community <- neutral_generation_speciation(community, speciation_rate)
    richness[i] <- length(unique(community))
  }
  return(richness)
}

# Question 12
question_12 <- function()  {
  generation <- seq(from = 1, to = 200)
  richness_max <- neutral_time_series_speciation(init_community_max(100), 0.1, 200) 
  richness_min <- neutral_time_series_speciation(community = init_community_min(100), 
    speciation_rate = 0.1, duration = 200)
  png(filename="question_12.png", width = 600, height = 400)
  plot(generation, richness_max, col = "blue", xlab = "Generation", ylab = "Neutral time series")
  lines(generation, richness_min, col = "red")
  legend('topright', legend = c('richness_max', 'richness_min'), fill = c("blue", "red"))
  Sys.sleep(0.1)
  dev.off()
  return("The initial condition of number of species does not have a great influence on the final convergence, given that the number of generation and the speciation rate is held constant. 
    This is because the convergence of the species richness is mainly determined by the rate of extinction and the rate of speciation. Since the initial rate of speciation is the same, it means that when equilibrium is reached, the level of equilibrium would be similar.
    Though the initial number of species is different, this is not a factor of the species richness.")
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
    richness_max <- neutral_time_series_speciation(
      community = community_max,
      speciation_rate = 0.1,
      duration = 1) 
    richness_min <- neutral_time_series_speciation(
      community = community_min, 
      speciation_rate = 0.1,
      duration = 1)
    # updating the community for current generation
    community_max <- neutral_generation_speciation(
      community = community_max,
      speciation_rate = 0.1
    )
    community_min <- neutral_generation_speciation(
      community = community_min,
      speciation_rate = 0.1
    )
    # record the species abundance octave vector
    if (count == 200){
      octave_max <- octaves(species_abundance(community_max))
      octave_min <- octaves(species_abundance(community_min))
      sum_max <- sum_vect(sum_max, octave_max)
      sum_min <- sum_vect(sum_min, octave_min)
    } else if (count > 200 & count <= 2200) {
      if ((count - 200) %% 20 == 0){
        octave_max <- octaves(species_abundance(community_max))
        octave_min <- octaves(species_abundance(community_min))
        sum_max <- sum_vect(sum_max, octave_max)
        sum_min <- sum_vect(sum_min, octave_min)
      }
    }
    count <- count + 1
  }
  mean_max <- sum_max / 101
  mean_min <- sum_min / 101

  png(filename="question_16_min.png", width = 600, height = 400)
  boxplot(mean_min, xlab = "Species abundance", ylab = "Mean abundance",
    main = "Mean species abundance with minimum initial condition")
  Sys.sleep(0.1)
  dev.off()

  png(filename="question_16_max.png", width = 600, height = 400)
  boxplot(mean_max, xlab = "Species abundance", ylab = "Mean abundance",
    main = "Mean species abundance with maximum initial condition")
  Sys.sleep(0.1)
  dev.off()
  
  return("The initial condition does not matter.
  Though the two graphs are not completely the same, the difference is small.
  The initial species richness is not a factor of the species abundance.
  No matter how large the initial species richness is, the species are equally
  likely to replicate or die.
  When the abundance is small, individuals are likely to replicate; when the 
  abundance is great enough, individuals will be likely to die.
  Equilibrium will be reached regardless of the number of initial species.")
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

  while (computing_time < wall_time){
    # current community
    community <- neutral_generation_speciation(community, 0.3) 
    
    # storing the species richness and abundances
    if ((count <= burn_in_generations) && (count %% interval_rich == 0)){
      # species richness
      time_series <- c(time_series, species_richness(community))
    }
    if (count %% interval_oct == 0){
      # species abundances
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
  # initialisation
  count <- 1
  mean_a <- c()
  mean_b <- c()
  mean_c <- c()
  mean_d <- c()
  sum_1 <- 0
  sum_2 <- 0
  sum_3 <- 0
  sum_4 <- 0
  combined_results <- list()
  for (i in 1:25){
    # read the output files
    load(paste("demographic_cluster_", i, ".rda", sep = ""))

    for (inv in 0:length(abundance_list)){
      # find the number of generations done
      num_gen <- inv * interval_oct
      # check if the burn-in time is up
      if (num_gen > burn_in_generations){
        # calculating the sum
        sum_1 <- sum_vect(sum_1, abundance_list[[inv]])
      }
    }
    # averaging across time
    mean_a[i] <- sum_1 / length(abundance_list)
  }
  # averaging across the 25 simulations
  mean_a[26] <- sum(mean_a)/25

  # repeat the procedure for other sizes
  for (i in 26:50){
    load(paste("demographic_cluster_", i, ".rda", sep = ""))
    for (inv in 0:length(abundance_list)){
      num_gen <- inv * interval_oct
      if (num_gen > burn_in_generations){
        sum_2 <- sum_vect(sum_2, abundance_list[[inv]])
      }
    }
    mean_b[i-25] <- sum_2 / length(abundance_list)
  }
  mean_b[26] <- sum(mean_b)/25

  for (i in 51:75){
    load(paste("demographic_cluster_", i, ".rda", sep = ""))
    for (inv in 0:length(abundance_list)){
      num_gen <- inv * interval_oct
      if (num_gen > burn_in_generations){
        sum_3 <- sum_vect(sum_3, abundance_list[[inv]])
      }
    }
    mean_c[i-50] <- sum_3 / length(abundance_list)
  }
  mean_c[26] <- sum(mean_c) / 25

  for (i in 76:100){
    load(paste("demographic_cluster_", i, ".rda", sep = ""))
    for (inv in 0:length(abundance_list)){
      num_gen <- inv * interval_oct
      if (num_gen > burn_in_generations){
        sum_4 <- sum_vect(sum_4, abundance_list[[inv]])
      }
    }
    mean_d[i-75] <- sum_4 / length(abundance_list)
  }
  mean_d[26] <- sum(mean_d)/25

  # save results to an .rda file
  combined_results <- list(mean_a, mean_b, mean_c, mean_d)
  save(combined_results, file = "process_neutral_cluster_results.rda")
}


plot_neutral_cluster_results <- function(){
  plot <- vector("list")
  sizes <- c(500, 1000, 2500, 5000)
  for (i in 1:4){
    p <- barplot(height = combined_results[i], xlab = "Abundance", ylab = "Mean", sub = paste("Data size: ", sizes[i]))
    plot[[i]] <- p
  }
  png(filename="plot_neutral_cluster_results.png", width = 600, height = 400)
  print(plot)
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
  
  png(filename="question_36", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 37
question_37 <- function(){
  
  png(filename="question_37_small", width = 600, height = 400)
  # plot your graph for the small initial population size here
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="question_37_large", width = 600, height = 400)
  # plot your graph for the large initial population size here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}



# Challenge questions - these are optional, substantially harder, and a maximum
# of 14% is available for doing them. 

# Challenge question A
Challenge_A <- function() {
  
  
  
  png(filename="Challenge_A_min", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="Challenge_A_max", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question B
Challenge_B <- function() {
  
  
  
  png(filename="Challenge_B", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question C
Challenge_C <- function() {
  
  
  
  png(filename="Challenge_C", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question D
Challenge_D <- function() {
  
  
  
  png(filename="Challenge_D", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function(){
  
  
  
  png(filename="Challenge_E", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question F
Challenge_F <- function(){
  
  
  
  png(filename="Challenge_F", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
}
