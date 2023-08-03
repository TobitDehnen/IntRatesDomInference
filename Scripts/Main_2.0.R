# Script to:
# A) Simulate interactions in sex-stratified animal groups with varying interaction rates between MM, MF and FF dyads as well as within MF dyads
# B) Plot the effect of variation in MF dyad interaction rates on females' inferred hierarchy positions

### SET UP

# Clear the workspace
rm(list = ls())
# Set seed
set.seed(1)
# Packages
library(aniDom)
library(ggplot2)
library(here)

# Source code
source(here("Scripts", "Source_2.0.R"))

# Can also load a previously run output if we want to, but don't resave this as the title (using below parameters) may not match the dataset
#previous_run <- "n_sims=1000,n_males=16,n_females=10,ratio_ints_to_dyad=9,6,3,steepness=1.RData"
#load(here("Outputs", previous_run))


### PARAMETERS THAT CAN BE PLAYED WITH

# Number of simulations
n_sims <- 1000
# Hierarchy inference method, either get.Elo (randomised Elo scores) or get.perc (percolation and conductance method)
hierarchy_method <- "get.Elo" # OR "get.perc"

# Number of males
n_males <- 16
# Number of females
n_females <- 10
# Proportion of females not to be aggressed in biased males scenario
prop_fem_breed <- 0.5
# Number of times each MM, MF and FF dyad interacts
ratio_ints_to_dyad <-  c(18,12,6)
# steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * steepness)))
steepness <-  1
# Compare inferred vs real dominance order, among either only females or all group members
dom_comp <- "female" # or "entire"


### PARAMETERS TO KEEP THE SAME

# Female categories
fem_cats <- c("Breeding Females", "Non-Breeding Females")
# Male interaction bias categories
bias_cats <- c("Males interact with all females", "Males interact with non-breeding females only")



### RUN SIMULATION ####

# Dataframe for rank results
output <- data.frame(run = rep(1:n_sims, each = 4), # simulation number
                     female_category = rep(fem_cats, each = 1, times = n_sims * 2),
                     bias_type = rep(bias_cats, each = 2, times = n_sims),
                     result = rep(NA, times = n_sims * 4))

# Run simulations
for(i in 1:n_sims) {
  
  # Save the start time
  if(i == 1) {start_time <- Sys.time()}
  
  # For both unbiased and biased male aggression scenarios:
  for(male_aggro_bias in c(FALSE, TRUE)) {
    
    # Generate 'real interactions' for a virtual population
    ints_obs <- get.real.ints(n_males = n_males, # Number of males
                              n_females = n_females, # Number of females
                              prop_fem_breed = prop_fem_breed, # Proportion of females not to be aggressed
                              ratio_ints_to_dyad = ratio_ints_to_dyad, # Number of times each  MM, MF and FF dyad interacts
                              male_aggro_bias = male_aggro_bias, # Whether males don't interact aggresively with subordinate females (TRUE) or if interact same as with other females (FALSE)
                              steepness = steepness # steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * steepness)))
    )
    
    # Infer hierarchy from interaction
    if(hierarchy_method == "get.Elo") {
      dom <- get.Elo(ints_obs$ints_real) # randomised Elo ratings
    } else if (hierarchy_method == "get.perc") {
      dom <- get.perc(ints_obs$ints_real) # Percolation and conductance
    }
    
    # Add rank order to original dataframe
    ints_obs$inds$dominance_inferred_order <- dom$order[match(ints_obs$inds$ID, dom$ID)]
    
    # Intra-female order
    ints_obs$inds$dominance_inferred_order_femalesexspecific <- NA
    ints_obs$inds$dominance_real_order_femalesexspecific <- NA
    ints_obs$inds[which(ints_obs$inds$sex == "F"),]$dominance_inferred_order_femalesexspecific <- order(ints_obs$inds[which(ints_obs$inds$sex == "F"),]$dominance_inferred_order)
    ints_obs$inds[which(ints_obs$inds$sex == "F"),]$dominance_real_order_femalesexspecific <- 1:nrow(ints_obs$inds[which(ints_obs$inds$sex == "F"),]) 
    
    # Compare inferred to real hierarchy
    # New column to fill
    ints_obs$inds$ranks_inferred_above_real <- NA
    if(dom_comp == "female") {
      for(j in 1:nrow(ints_obs$inds)) {
        # If female
        if(ints_obs$inds$sex[j] == "F") {
          # Ranks
          ints_obs$inds$ranks_inferred_above_real[j] <- ints_obs$inds$dominance_real_order_femalesexspecific[j] - ints_obs$inds$dominance_inferred_order_femalesexspecific[j]
        }
      } 
    } else if (dom_comp == "entire") {
      for(j in 1:nrow(ints_obs$inds)) {
        # If female
        if(ints_obs$inds$sex[j] == "F") {
          # Ranks
          ints_obs$inds$ranks_inferred_above_real[j] <- ints_obs$inds$dominance_real_order[j] - ints_obs$inds$dominance_inferred_order[j]
        }
      } 
    }
    
    # Assign results
    if(male_aggro_bias == FALSE) { # Males interact with all females
      output$result[(4*i)-3] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$breeding_female == 1)]) # breeding females
      output$result[(4*i)-2] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$breeding_female == 0)]) # non-breeding females 
    } else if (male_aggro_bias == TRUE) { # Males interact with non-breeding females only
      output$result[(4*i)-1] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$breeding_female == 1)]) # breeding females
      output$result[(4*i)] <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$breeding_female == 0)]) # non-breeding females
    }
  }
  
  # Estimate finish time
  average_duration <- difftime(Sys.time(), start_time, units = "secs") / i # duration of average loop
  print(paste(100 * i / n_sims, "% Complete", "                 Estimated Finish Time: ", Sys.time() + ((n_sims - i) * average_duration), sep = "")) # estimated finish
  if(i == n_sims) {print(paste("Total Duration:", round(difftime(Sys.time(), start_time, units = "mins")), "minutes"))} # report duration on last run
}

# Relevel for plotting
output$female_category <- factor(output$female_category, levels = fem_cats)
output$bias_type <- factor(output$bias_type, levels = bias_cats)

# Add parameters to output as extra columns, so they're hardcoded into the output
output$n_sims <- n_sims
output$n_males <- n_males
output$n_females <- n_females
output$prop_fem_breed <- prop_fem_breed
output$ratio_ints_to_dyad <- paste(as.character(ratio_ints_to_dyad), collapse = ",")
output$steepness <- steepness
output$dom_comp <- dom_comp
output$hierarchy_method <- hierarchy_method

# Interaction-level data
output_name <- paste("n_sims=", n_sims, ",n_males=", n_males, ",n_females=", n_females, ",prop_fem_breed=", prop_fem_breed, ",ratio_ints_to_dyad=", 
                     ratio_ints_to_dyad[1], ",", ratio_ints_to_dyad[2], ",", ratio_ints_to_dyad[3], 
                     ",steepness=", steepness, ",dom_comp=", dom_comp, ",hierarchy_method=", hierarchy_method, sep = "")



### SAVE OUTPUTS ####

save(output, file = here("Outputs", paste(output_name, ".RData", sep = "")))


### PLOT RESULTS ####

# Save results plot to PDF
#pdf(here("Figures", paste(output_name, "_hist", ".pdf", sep = "")), height = 5, width = 8) # save
#ggplot(output, aes(result, fill = bias_type)) +
#  geom_histogram(position = position_dodge2(padding = 0.2, preserve = "single"), bins = 40) +
#  theme_linedraw(base_size = 15) +
#  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
#  geom_vline(xintercept = 0, colour = "black", linetype = "dashed", linewidth = 1) +
#  labs(x = "Mean Inferred minus Real Rank", y = "Frequency") +
#  facet_grid(~female_category) +
#  theme(legend.position = "bottom")
#dev.off() # finish

# Save results plot to PDF
pdf(here("Figures", paste(output_name, "_dens", ".pdf", sep = "")), height = 5, width = 8) # save
ggplot(output, aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_linedraw(base_size = 15) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "Mean Inferred Minus Real Hierarchy Position", y = "Frequency") +
  facet_grid(~female_category) +
  theme(legend.position = "bottom", panel.spacing = unit(3, "lines"))
dev.off() # finish
