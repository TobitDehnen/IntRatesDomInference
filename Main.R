# Script to:
# A) Simulate interactions with certain females receiving no male aggression
# B) Plot the mean difference in the inferred and real ranks for females at the bottom of the hierarchy that receive no male aggression, for each simulation

# Clear the workspace
rm(list = ls())
# Set seed
set.seed(1)
# Packages
library(aniDom)
library(ggplot2)
# Source code
source("Source.R")

# Number of simulations
n_sims <- 1000

# Number of males
n_males <- 16
# Number of females
n_females <- 10
# Number of times each dyad interacts
real_ratio_ints_to_dyad = 3
# real_steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * real_steepness)))
real_steepness = 0.5

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


# Plot results
pdf("Figures/Male_aggression_female_dominance_consequences.pdf", height = 7, width = 7) # save
ggplot(output) +
  geom_histogram(aes(x = result), bins = 30, fill = "grey60") +
  theme_linedraw() +
  geom_vline(xintercept = 0, colour = "#007052", linetype = "dashed", linewidth = 1) +
  #scale_x_continuous(limits = c(min(output$result),max(output$result)), n.breaks = 4) +
  labs(x = "Mean Inferred Rank Relative to Real Rank", y = "Frequency") +
  facet_grid(bias_type ~ female_category)
dev.off() # finish


