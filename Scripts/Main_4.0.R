# Script to:
# A) Simulate interactions in sex-stratified animal groups with various parameters (group size, sex ratio, proportion of females that breed,
# hierarchy steepness & number of interactions among MM/MF/FF dyads) and compare breeding and non-breeding females' inferred vs real hierarchy
# positions, either comparing across the entire hierarchy or simply within females, where the hierarchy is inferred via one of three possible methods.
# Two scenarios are modelled: Males either equally tolerant of all females, or more tolerant of breeding females (redirecting intersexual aggression 
# towards non-breeding females, which also interact less with breeding females as a consequence of not being tolerated (as could happen in real 
# animal groups when tolerated females spatially associate with males (e.g. at food patches in real groups).
# B) Plot the combined effect of male tolerance scenario and inference method on females' inferred vs real hierarchy positions.

### SET UP

# Clear the workspace
rm(list = ls())
# Set seed
set.seed(1)

# Packages not in functions
library(ggplot2) # plotting
library(here) # path
library(parallel) # multiple core lapply function
library(pbmcapply) # progress bar wrapper of mclapply
library(reshape2) # collapsing results (short to long format)

# Source code
source(here("Scripts", "Source_4.0.R"))

# Can also load a previously run output if we want to plot that without running it, but don't resave this as the title (using below parameters) may not match the dataset
#previous_run <- "n_sims=500,n_males=16,n_females=10,prop_fem_breeding=0.5,ratio_ints_to_dyad=12,8,4,steepness=1,dom_comp=female,hierarchy_method=get.matrix.RData"
#load(here("Outputs", previous_run))



### PARAMETERS THAT CAN BE PLAYED WITH

# Number of cores to use for running simulations:
n_cores <- 8 # positive integer
# Number of simulations to run (recommended at least 100):
n_sims <- 1000 # positive integer
# Hierarchy inference method to test:
hierarchy_method <- "get.perc" # either: randomised Elo scores ("get.Elo"), Percolation and Conductance ("get.perc") or I&SI ("get.matrix")

# Number of males
n_males <- 16 # positive integer
# Number of females
n_females <- 10 # positive integer
# Proportion of females assigned as breeding
prop_fem_breeding <- 0.5 # 0-1, when multiplied with n_females must produce integer
# Number of times each MM, MF and FF dyad interacts in unbiased scenario; in the biased scenario only MF dyads involving non-breeding females interact, and FF dyads
ratio_ints_to_dyad <- c(12,8,4) # three integers, last one should be an even number (as it's divided by 2 in the biased scenario)
# steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * steepness))): positive number
steepness <- 1 # positive value 
# Compare inferred vs real dominance order, among either only females or all group members (as females could be inferred as dominant to some males)
dom_comp <- "female" # "female"  OR "entire"



### OTHER PARAMETERS FOR SIMULATION (DO NOT CHANGE)

# Female categories
fem_cats <- c("Breeding females", "Non-breeding females") # do not change
# Male interaction bias categories
bias_cats <- c("Males tolerate all females equally", "Males tolerate breeding females only") # do not change



### MAKE DATAFRAME TO RUN ####
col_names <- c("n_cores",
               "sim",
               "run",
               "bias_type",
               fem_cats)
output <- as.data.frame(matrix(nrow = n_sims * 2, ncol = length(col_names)))
colnames(output) <- col_names

# Add sim, run, bias category and other parameters as columns (for use in mclapply function)
output$n_cores <- n_cores # number of cores to run across
output$sim <- 1:(n_sims * 2) # each simulation has a number
output$run <- rep(1:n_sims, each = 2) # each repeat of each scenario
output$bias_type <- rep(bias_cats, times = n_sims) # scenario types
output$hierarchy_method <- hierarchy_method
output$n_males <- n_males
output$n_females <- n_females
output$prop_fem_breeding <- prop_fem_breeding
output$ratio_ints_to_dyad <- paste(ratio_ints_to_dyad, collapse = "_") # collapse values into single string
output$steepness <- steepness
output$dom_comp <- dom_comp


### SIMULATIONS ####

# Convert output dataframe into output list (for mclapply function)
output_list <- split(output, seq(nrow(output)))

# Perform simulations
output_list <- pbmclapply(output_list, function(output_list) { # pbmclapply allows progress bar showing for multiple core lapply function
  
  # Generate 'real interactions' for virtual population
  ints_obs <- get.real.ints(n_males = output_list$n_males, # Number of males
                            n_females = output_list$n_females, # Number of females
                            prop_fem_breeding = output_list$prop_fem_breeding, # Proportion of females not to be aggressed
                            ratio_ints_to_dyad = as.numeric((strsplit(output_list$ratio_ints_to_dyad, split = "_"))[[1]]), # Number of times each  MM, MF and FF dyad interacts
                            bias_type = output_list$bias_type, # males either aggress all females equally (FALSE) or redirect aggression from breeding to non-breeding females and no. interactions between breeding & non-breeding females is simultaneously halved 
                            steepness = output_list$steepness, # steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * steepness)))
                            n_cores = output_list$n_cores, # number of cores to use
                            for_network_illustration = F # if it's to simply illustrate the network, always allocate the same females as breeders/non-breeders for reproducability
  )
  
  ## Real hierarchy positions (intrasexual and group-level)
  # Entire hierarchy order
  ints_obs$inds$dominance_real_order <- 1:nrow(ints_obs$inds)
  # Intra-female order
  ints_obs$inds$dominance_real_order_femalesexspecific <- NA
  ints_obs$inds[which(ints_obs$inds$sex == "F"),]$dominance_real_order_femalesexspecific <- 1:nrow(ints_obs$inds[which(ints_obs$inds$sex == "F"),]) 
  
  # Infer hierarchy from interactions
  if(output_list$hierarchy_method == "get.Elo") {
    dom <- get.Elo(ints_obs$ints_real) # randomised Elo ratings
  } else if (output_list$hierarchy_method == "get.perc") {
    dom <- get.perc(ints_obs$ints_real) # Percolation and Conductance
  } else if (output_list$hierarchy_method == "get.matrix") {
    dom <- get.matrix(ints_obs$ints_real) # I&SI
  }
  
  ## Add inferred positions from hierarchy
  # Entire hierarchy order
  ints_obs$inds$dominance_inferred_order <- dom$order[match(ints_obs$inds$ID, dom$ID)]
  # Intra-female order
  ints_obs$inds$dominance_inferred_order_femalesexspecific <- NA
  ints_obs$inds[which(ints_obs$inds$sex == "F"),]$dominance_inferred_order_femalesexspecific <- match(ints_obs$inds[which(ints_obs$inds$sex == "F"),]$dominance_inferred_order, # observed order
                                                                                                      sort(ints_obs$inds[which(ints_obs$inds$sex == "F"),]$dominance_inferred_order)) # sorted order
  
  
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
        ints_obs$inds$ranks_inferred_above_real[j] <- ints_obs$inds$dominance_real_order[j] - ints_obs$inds$dominance_inferred_order[j] # need to change the ints_obs$dominance_real_order here as that variable doesn't exist so gets zeros instead
      }
    } 
  }
  
  # Assign results
  output_list$`Breeding females` <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$breeding_female == 1)]) # breeding females
  output_list$`Non-breeding females` <- mean(ints_obs$inds$ranks_inferred_above_real[which(ints_obs$inds$breeding_female == 0)]) # non-breeding females
  
  # Return output
  return(output_list)} # return output list
  , mc.cores = n_cores # number of cores
)



### POST-RUN PROCESSING ####

# Convert output to long-format and return to dataframe format
output_list <- mclapply(output_list, function(output_list) {
  output_list <- melt(output_list, # the output_list
                      id.vars = colnames(output_list)[-which(colnames(output_list) %in% fem_cats)], # all the variables to keep but not split apart on
                      measure.vars = fem_cats, # The source columns
                      variable.name = "female_category", # the column name for the measure.vars
                      value.name = "result" # the column the values will go into
  )
  return(output_list)}
  ,mc.cores = n_cores # number of cores
)
output <- do.call(rbind, output_list)

# Relevel for plotting
output$female_category <- factor(output$female_category, levels = fem_cats)
output$bias_type <- factor(output$bias_type, levels = bias_cats)

# Name for saving
output_name <- paste("n_sims=", n_sims, ",n_males=", n_males, ",n_females=", n_females, ",prop_fem_breeding=", prop_fem_breeding, ",ratio_ints_to_dyad=", 
                     ratio_ints_to_dyad[1], ",", ratio_ints_to_dyad[2], ",", ratio_ints_to_dyad[3], 
                     ",steepness=", steepness, ",dom_comp=", dom_comp, ",hierarchy_method=", hierarchy_method, sep = "")



### SAVE OUTPUTS ####

save(output, file = here("Outputs", paste(output_name, ".RData", sep = "")))



### PLOT RESULTS ####

# Save base results plot to PDF (use Publication_plotting_quantiles.R script for more in-depth plots showing distribution quantiles)
pdf(here("Figures", paste(output_name, ".pdf", sep = "")), height = 5, width = 8) # save
ggplot(output, aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_linedraw(base_size = 15) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "Mean Inferred Minus Real Hierarchy Position", y = "Density") +
  facet_grid(~female_category) +
  theme(legend.position = "bottom", panel.spacing = unit(3, "lines"))
dev.off() # finish

