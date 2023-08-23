# Source code

# Function to generate 'real interactions' for a virtual group
get.real.ints <- function(n_males = 16, # Number of males
                          n_females = 10, # Number of females
                          prop_fem_breeding = 0.5, # Proportion of females not to be aggressed
                          ratio_ints_to_dyad = c(12,8,4), # Number of times each MM, MF, FF dyad interacts, last number must be even
                          bias_type = "Males tolerate all females equally", # intersexual interactions scenario
                          steepness = 1, # steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * steepness)))
                          n_cores = 8, # number of cores to use when generating interaction outcomes
                          for_network_illustration = F # if it's to simply illustrate the network, always allocate the same females as breeders/non-breeders
) {
  
  # Generate individuals and their dominance
  inds <- data.frame(ID = LETTERS[1:(n_males+n_females)], # IDs
                     dominance = sort(c(rnorm((n_males), mean = 0, sd = 1), # distribution higher for biomdal (males and females)
                                        rnorm((n_females), mean = -4, sd = 1)), # distribution lower for bimodal (males and females)
                                      decreasing = TRUE), # Dominance
                     sex = c(rep("M", times = n_males), rep("F", times = n_females))) # sex
  # Standardise dominance between 0 and no. individuals in group -1
  inds$dominance <- (inds$dominance - min(inds$dominance)) / (max(inds$dominance) - min(inds$dominance)) * ((n_males + n_females) - 1)
  
  
  ### Assign which females are preferred
  inds$breeding_female <- NA
  inds$breeding_female[which(inds$sex == "F")] <- sample(c(rep(1, times = n_females * prop_fem_breeding), rep(0, times = n_females - (n_females * prop_fem_breeding))), size = n_females)
  
  # if for illustration, hard code group size, sex composition, dominance values and breeder allocations
  if(for_network_illustration) {
    inds <- data.frame(ID = LETTERS[1:(16+10)], # IDs
                                      dominance = c(2.60, 1.43, 1.17, 1.07, 0.55, 0.48, 0.34, 0.22, 0.11, 0.05, -0.06, -0.10, -0.19, -1.21, -1.36, -1.39,
                                                    -2.88, -2.89, -3.04, -3.43, -3.71, -3.77, -4.57, -4.65, -5.01, -5.55), # Dominance
                                      sex = c(rep("M", times = 16), rep("F", times = 10))) # sex
  # Standardise dominance between 0 and no. individuals in group -1
  inds$dominance <- (inds$dominance - min(inds$dominance)) / (max(inds$dominance) - min(inds$dominance)) * ((n_males + n_females) - 1)
  inds$breeding_female[which(inds$sex == "F")] <- c(0, 1, 0, 1, 0, 1, 1, 0, 0, 1)}
  
  ### Generate interactions 
  ## MM - same irrespective of male-female aggression scenario
  ints_real_MM <- expand.grid(Ind_A = inds$ID[which(inds$sex == "M")], Ind_B = inds$ID[which(inds$sex == "M")]) # get all combinations
  ints_real_MM <- ints_real_MM[-which(ints_real_MM$Ind_A == ints_real_MM$Ind_B),] # remove self-interactions
  ints_real_MM <- ints_real_MM[rep(seq_len(nrow(ints_real_MM)), times = ratio_ints_to_dyad[1]),] # repeat certain number of times
  
  if(bias_type == "Males tolerate breeding females only") {
    ## MF - with non-breeding only, where aggression is redirected from breeding to non-breeding
    ints_real_MF <- expand.grid(Ind_A = inds$ID[which(inds$sex == "M")], Ind_B = inds$ID[which(inds$sex == "F" & inds$breeding_female == 0)]) # get all combinations
    ints_real_MF <- ints_real_MF[rep(seq_len(nrow(ints_real_MF)), times = round((ratio_ints_to_dyad[2]) * (1 / prop_fem_breeding)), digits = 0),] # redirected aggression
    
    ## FF
    # within-categories dyads interact at the same rate as in the unbiased scenario, between-category dyads interact half as much
    ints_real_FF <- expand.grid(Ind_A = inds$ID[which(inds$sex == "F")], Ind_B = inds$ID[which(inds$sex == "F")]) # get all combinations
    ints_real_FF <- ints_real_FF[-which(ints_real_FF$Ind_A == ints_real_FF$Ind_B),] # remove self-interactions
    ints_real_FF$Ind_A_status <- inds$breeding_female[match(ints_real_FF$Ind_A, inds$ID)] # add A's status
    ints_real_FF$Ind_B_status <- inds$breeding_female[match(ints_real_FF$Ind_B, inds$ID)] # add B's status
    
    # Separate out within- and between-category interactions: within category dyads interact as much as all FF dyads as in unbiased scenario, between category dyads interact half as much
    ints_real_FF_same_category <- ints_real_FF[which(ints_real_FF$Ind_A_status == ints_real_FF$Ind_B_status),] # same category dyads
    ints_real_FF_different_category <- ints_real_FF[which(ints_real_FF$Ind_A_status != ints_real_FF$Ind_B_status),] # opposite category dyads 
    ints_real_FF_same_category <- ints_real_FF_same_category[rep(seq_len(nrow(ints_real_FF_same_category)), times = ratio_ints_to_dyad[3]),] # same as in unbiased
    ints_real_FF_different_category <- ints_real_FF_different_category[rep(seq_len(nrow(ints_real_FF_different_category)), times = ratio_ints_to_dyad[3] / 2),] # half as many interactions
    
    # Recombine within- and between-category interactions & remove superfluous columns
    ints_real_FF <- rbind(ints_real_FF_same_category,ints_real_FF_different_category )
    ints_real_FF <- ints_real_FF[,which(colnames(ints_real_FF) %in% c("Ind_A", "Ind_B"))]
    
  } else if (bias_type == "Males tolerate all females equally") {
    ## MF
    ints_real_MF <- expand.grid(Ind_A = inds$ID[which(inds$sex == "M")], Ind_B = inds$ID[which(inds$sex == "F")]) # get all combinations
    ints_real_MF <- ints_real_MF[rep(seq_len(nrow(ints_real_MF)), times = ratio_ints_to_dyad[2]),] # repeat certain number of times
    ## FF
    ints_real_FF <- expand.grid(Ind_A = inds$ID[which(inds$sex == "F")], Ind_B = inds$ID[which(inds$sex == "F")]) # get all combinations
    ints_real_FF <- ints_real_FF[-which(ints_real_FF$Ind_A == ints_real_FF$Ind_B),] # remove self-interactions
    ints_real_FF <- ints_real_FF[rep(seq_len(nrow(ints_real_FF)), times = ratio_ints_to_dyad[3]),] # repeat certain number of times
  }
  
  ## Combine, shuffle and reset rownames
  ints_real <- rbind(ints_real_MM, ints_real_MF, ints_real_FF)
  ints_real <- ints_real[sample(nrow(ints_real)),]
  rownames(ints_real) <- 1:nrow(ints_real)
  
  ## Simulate outcomes
  ints_real$winner <- NA
  ints_real$loser <- NA
  # dataframe -> list
  ints_real_list <- split(ints_real, seq(nrow(ints_real)))
  
  # Run mclapply over dataframe (instead of loop)
  ints_real_list <- mclapply(ints_real_list, function(ints_real_list) {
    
    # Simulate an interaction
    Ind_A_tmp <- ints_real_list$Ind_A
    Ind_B_tmp <- ints_real_list$Ind_B
    
    # Get rank difference
    rank_diff <- inds$dominance[which(inds$ID == Ind_A_tmp)] - inds$dominance[which(inds$ID == Ind_B_tmp)]
    
    # Get probability that A wins
    p_Ind_A_tmp_wins_this <-   1 / (1 + exp(-(rank_diff * steepness)))
    
    # Does A win?
    Ind_A_tmp_won_this <- sample(c(T, F), 1, prob = c(p_Ind_A_tmp_wins_this,1 - p_Ind_A_tmp_wins_this))
    
    # Allocate winner and loser
    if (Ind_A_tmp_won_this) {
      ints_real_list$winner <- as.character(Ind_A_tmp)
      ints_real_list$loser <- as.character(Ind_B_tmp)
    } else {
      ints_real_list$winner <- as.character(Ind_B_tmp)
      ints_real_list$loser <- as.character(Ind_A_tmp)
    }
    
    return(ints_real_list)} # return output list
    , mc.cores = n_cores # number of cores
  )
  # recombine output list into df
  ints_real <- do.call(rbind, ints_real_list)
  
  
  # Remove any unnecessary columns
  ints_real <- ints_real[,which(colnames(ints_real) %in% c("winner", "loser"))]
  
  # Return simulated interactions
  return(list(inds = inds, ints_real = ints_real))
}

# Function to infer hierarchy via randomised Elo scores
get.Elo <- function(ints_obs # dataframe holding interactions, containing: ID, winner and loser columns
) {
  
  # If don't have packages, install
  if(!("aniDom" %in% installed.packages()[,"Package"])) {install.packages("aniDom")}
  
  # Attach package
  library(aniDom)
  
  # Calculate Elo scores
  dom_scores <- aniDom::elo_scores(winners = ints_obs$winner, losers = ints_obs$loser, randomise = TRUE, n.rands = 1000)
  # Calculate score means and order
  mean_scores <- rowMeans(dom_scores)
  mean_scores <- mean_scores[order(mean_scores)]
  # standardise between 0 and no. individuals in dataset - 1
  mean_scores <- (mean_scores - min(mean_scores)) / (max(mean_scores) - min(mean_scores)) * (length(unique(c(unique(ints_obs$winner), unique(ints_obs$loser)))) - 1)
  # Create output file
  dominance_df <- data.frame(ID = names(mean_scores), metric = unname(mean_scores))[order(mean_scores, decreasing = T),]
  dominance_df$order <- 1:nrow(dominance_df)
  dominance_df$method <- "Elo_rand"
  
  return(dominance_df)
}

# Function to infer hierarchy via percolation & conductance
get.perc <- function(ints_obs # dataframe holding interactions, containing: ID, winner and loser columns 
) {
  
  # If don't have packages, install
  packages_required <- c("Perc", "DynaRankR")
  for(i in 1:length(packages_required)){
    if(!(packages_required[i] %in% installed.packages()[,"Package"])) {
      install.packages(paste(packages_required[i]))
    }
  }
  
  # Attach packages
  library(Perc)
  library(DynaRankR)
  
  conductance_matrix <- conductance(edgelist_to_matrix(ints_obs[,1:2],unique(c(ints_obs[,1],ints_obs[,2]))), maxLength = 4) # maxLength 4 is the median of the available indirect path lengths (2-6)
  simOrders <- simRankOrder(conductance_matrix$p.hat)
  dominance_df <- data.frame(ID = simOrders$BestSimulatedRankOrder[,'ID'], metric = NA, order = simOrders$BestSimulatedRankOrder[,'ranking'], method = "perc")[order(simOrders$BestSimulatedRankOrder[,2]),]
  return(dominance_df)
}

# Function to infer hierarchy via I&SI
get.matrix <- function(ints_obs # dataframe holding interactions, containing: ID, winner and loser columns 
) {
  # if don't have CRAN packages, install
  packages_required <- c("EloRating", "DynaRankR")
  for(i in 1:length(packages_required)){
    if(!(packages_required[i] %in% installed.packages()[,"Package"])) {
      install.packages(paste(packages_required[i]))
    }
  }
  
  library(EloRating)
  library(DynaRankR)
  
  ISI_ranks <- quiet(ISIranks(ISI(edgelist_to_matrix(ints_obs[,1:2],unique(c(ints_obs[,1],ints_obs[,2]))))))  ## Generate ranking matrices and then get best ranking 
  
  dominance_df <- data.frame(ID = ISI_ranks[,"ID"], metric = NA, order = ISI_ranks[,"avg"], method = "mat_reorder")  # can add to have T/F for whether there was a single best order: resolved = ifelse(ncol(ISI_ranks > 3), F,T)
  dominance_df <- dominance_df[order(dominance_df$order),] # Order by order
  return(dominance_df)
}

# Function to prevent output from being printed (I&SI values from get.matrix function)
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 