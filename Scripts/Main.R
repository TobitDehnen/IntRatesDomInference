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




### PARAMETERS

# Number of simulations
n_sims <- 100

# Number of males
n_males <- 16
# Number of females
n_females <- 10
# Number of times each dyad interacts
real_ratio_ints_to_dyad <-  3
# Number of times each MM, MF and FF dyad interacts
real_ratio_ints_to_dyad <-  c(9,6,3)
# real_steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * real_steepness)))
real_steepness <-  1



### RUN SIMULATION ####

# Vectors to fill with outputs
output <- data.frame(run = rep(1:n_sims, each = 4), # simulation number
                     female_category = rep(c("Subordinate females", "Dominant females"), each = 1, times = n_sims * 2),
                     bias_type = rep(c("Males interact with dominant females only", "Males interact with all females"), each = 2, times = n_sims),
                     result = rep(NA, times = n_sims * 4))

# Run simulations
for(i in 1:n_sims) {
  
  ### BIASED MALES
  male_aggro_bias <- TRUE
  
  # Generate 'real interactions' for a virtual population
  ints_obs <- get.real.ints(n_males = n_males, # Number of males
                            n_females = n_females, # Number of females
                            real_ratio_ints_to_dyad = real_ratio_ints_to_dyad, # Number of times each  MM, MF and FF dyad interacts
                            male_aggro_bias = male_aggro_bias, # Whether males don't interact aggresively with subordinate females (TRUE) or if interact same as with other females (FALSE)
                            real_steepness = real_steepness # real_steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * real_steepness)))
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
      ints_obs$inds$ranks_inferred_above_real[j] <- j - ints_obs$inds$dominance_inferred_order[j]
    }
  }
  
  # Mean ranks inferred greater than real
  output$result[(4*i)-3] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$dominant_female == 0)]) # Subordinate females
  output$result[(4*i)-2] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$dominant_female == 1)]) # Dominant females
  
  
  ### UNBIASED MALES
  male_aggro_bias <- FALSE
  
  # Generate 'real interactions' for a virtual population
  ints_obs <- get.real.ints(n_males = n_males, # Number of males
                            n_females = n_females, # Number of females
                            real_ratio_ints_to_dyad = real_ratio_ints_to_dyad, # Number of times each dyad interacts
                            male_aggro_bias = male_aggro_bias, # Whether males don't interact aggresively with subordinate females (TRUE) or if interact same as with other females (FALSE)
                            real_steepness = real_steepness # real_steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * real_steepness)))
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
      ints_obs$inds$ranks_inferred_above_real[j] <- j - ints_obs$inds$dominance_inferred_order[j]
    }
  }
  
  # Mean ranks inferred greater than real
  output$result[(4*i)-1] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$dominant_female == 0)]) # Subordinate females
  output$result[(4*i)] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$dominant_female == 1)]) # Dominant females
}

# Interaction-level data
output_filename <- paste("Outputs/", "n_sims=", n_sims, "_n_males=", n_males, "_n_females=", n_females, "_real_ratio_ints_to_dyad=", 
                         real_ratio_ints_to_dyad[1], ",", real_ratio_ints_to_dyad[2], ",", real_ratio_ints_to_dyad[3], 
                         "_real_steepness=", real_steepness, ".RData", sep = "")



### SAVE OUTPUTS ####

save(output, file = output_filename)


### PLOT RESULTS ####

# Save results plot to PDF
pdf("Figures/Male_aggression_female_dominance_consequences_0.2_steepness.pdf", height = 5, width = 8) # save
ggplot(output, aes(result, fill = bias_type)) +
  geom_histogram(position = "dodge2") +
  theme_linedraw() +
  scale_fill_manual(values = c("#56B4E9", "#E69F00")) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed", linewidth = 1) +
  labs(x = "Mean Inferred Rank Relative to Real Rank", y = "Frequency") +
  facet_grid(~female_category) +
  theme(legend.position = "bottom")
dev.off() # finish
