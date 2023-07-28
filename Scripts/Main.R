# Script to:
# A) Simulate interactions with certain females receiving no male aggression
# B) Plot the mean difference in the inferred and real ranks for females at the bottom of the hierarchy that receive no male aggression, for each simulation

### SET UP

# Clear the workspace
rm(list = ls())
# Set seed
set.seed(1)
# Packages
library(aniDom)
library(ggplot2)
# Source code
source("Scripts/Source_2.0.R")

# Can load a previously run output if we want to, but saving is not straightforward as uses parameters specified below which may differ to those used to generate the loaded output
# load("Outputs/n_sims=100,n_males=16,n_females=10,ratio_ints_to_dyad=9,6,3,steepness=1.RData")

### PARAMETERS

# Number of simulations
n_sims <- 1000

# Number of males
n_males <- 16
# Number of females
n_females <- 10
# Number of females not to be aggressed in biased males scenario
n_fem_breed <-  3
# Number of times each MM, MF and FF dyad interacts
ratio_ints_to_dyad <-  c(9,6,3)
# Female categories
fem_cats <- c("Breeding females", "Non-breeding females")
# steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * steepness)))
steepness <-  1



### RUN SIMULATION ####

# Dataframe for rank results
output <- data.frame(run = rep(1:n_sims, each = 4), # simulation number
                     female_category = rep(fem_cats, each = 1, times = n_sims * 2),
                     bias_type = rep(c("Males interact with non-breeding females only", "Males interact with all females"), each = 2, times = n_sims),
                     result = rep(NA, times = n_sims * 4))

# Run simulations
for(i in 1:n_sims) {
  
  # Save the start time
  if(i == 1) {start_time <- Sys.time()}
  
  ### BIASED MALES
  male_aggro_bias <- TRUE
  
  # Generate 'real interactions' for a virtual population
  ints_obs <- get.real.ints(n_males = n_males, # Number of males
                            n_females = n_females, # Number of females
                            n_fem_breed = n_fem_breed, # Number of females not to be aggressed
                            ratio_ints_to_dyad = ratio_ints_to_dyad, # Number of times each  MM, MF and FF dyad interacts
                            male_aggro_bias = male_aggro_bias, # Whether males don't interact aggresively with subordinate females (TRUE) or if interact same as with other females (FALSE)
                            steepness = steepness # steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * steepness)))
  )
  
  # Infer hierarchy from interaction
  dom <- get.Elo(ints_obs$ints_real)
  
  # Add to original dataframe
  ints_obs$inds$dominance_inferred_order <- dom$order[match(ints_obs$inds$ID, dom$ID)]
  ints_obs$inds$dominance_inferred_metric <- dom$metric[match(ints_obs$inds$ID, dom$ID)]
  
  # Compare inferred to real hierarchy
  # New column to fill
  ints_obs$inds$ranks_inferred_above_real <- NA
  for(j in 1:nrow(ints_obs$inds)) {
    # If female
    if(ints_obs$inds$sex[j] == "F") {
      # Ranks
      ints_obs$inds$ranks_inferred_above_real[j] <- j - ints_obs$inds$dominance_inferred_order[j]
    }
  }
  
  # Mean ranks inferred greater than real
  output$result[(4*i)-3] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$breeding_female == 1)]) # breeding females
  output$result[(4*i)-2] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$breeding_female == 0)]) # non-breeding females

  
  
  ### UNBIASED MALES
  male_aggro_bias <- FALSE
  
  # Generate 'real interactions' for a virtual population
  ints_obs <- get.real.ints(n_males = n_males, # Number of males
                            n_females = n_females, # Number of females
                            ratio_ints_to_dyad = ratio_ints_to_dyad, # Number of times each dyad interacts
                            male_aggro_bias = male_aggro_bias, # Whether males don't interact aggresively with subordinate females (TRUE) or if interact same as with other females (FALSE)
                            steepness = steepness # steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * steepness)))
  )
  
  # Infer hierarchy from interaction
  dom <- get.Elo(ints_obs$ints_real)
  
  # Add to original dataframe
  ints_obs$inds$dominance_inferred_order <- dom$order[match(ints_obs$inds$ID, dom$ID)]
  ints_obs$inds$dominance_inferred_metric <- dom$metric[match(ints_obs$inds$ID, dom$ID)]
  
  # Compare inferred to real hierarchy
  # New column to fill
  ints_obs$inds$ranks_inferred_above_real <- NA
  for(j in 1:nrow(ints_obs$inds)) {
    # If female
    if(ints_obs$inds$sex[j] == "F") {
      # Ranks
      ints_obs$inds$ranks_inferred_above_real[j] <- j - ints_obs$inds$dominance_inferred_order[j]
    }
  }
  
  # Mean ranks inferred greater than real
  output$result[(4*i)-1] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$breeding_female == 1)]) # breeding females
  output$result[(4*i)] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$breeding_female == 0)]) # non-breeding females
  
  # Estimate the finish time
  average_duration <- difftime(Sys.time(), start_time, units = "secs") / i # duration of average loop
  print(paste(100 * i / n_sims, "% Complete", "                 Estimated Finish Time: ", Sys.time() + ((n_sims - i) * average_duration), sep = "")) # estimated finish
  if(i == n_sims) {print(paste("Total Duration:", round(difftime(Sys.time(), start_time, units = "mins")), "minutes"))} # report duration on last run
}

# Relevel female_category, so plots subordinates on left and dominants on right
output$female_category <- factor(output$female_category, levels = fem_cats)


# Interaction-level data
output_name <- paste(Sys.time(), ",n_sims=", n_sims, ",n_males=", n_males, ",n_females=", n_females, ",ratio_ints_to_dyad=", 
                         ratio_ints_to_dyad[1], ",", ratio_ints_to_dyad[2], ",", ratio_ints_to_dyad[3], 
                         ",steepness=", steepness, sep = "")



### SAVE OUTPUTS ####

save(output, file = paste("Outputs/", output_name, ".RData", sep = ""))



### PLOT RESULTS ####

# Save results plot to PDF
pdf(paste("Figures/", output_name, ".pdf", sep = ""), height = 5, width = 8) # save
ggplot(output, aes(result, fill = bias_type)) +
  geom_histogram(position = position_dodge2(preserve = "single")) +
  theme_linedraw() +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "Males' Interactions with Females") +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed", linewidth = 1) +
  labs(x = "Mean Inferred Rank Relative to Real Rank", y = "Frequency") +
  facet_grid(~female_category) +
  theme(legend.position = "bottom")
dev.off() # finish


