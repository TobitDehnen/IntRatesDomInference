# Script to:
# A) Generate a social group, in which individuals linearly differ in dominance, and 
# simulate 'real' interactions among group members, with winning probability varying 
# with difference in dominance through a sigmoidal function.


# Clear the workspace
rm(list = ls())
# Set seed
set.seed(1)

#### FUNCTIONS ####

# Function to generate 'real interactions' for a virtual population
get.real.ints <- function(n_males = 16, # Number of males
                          n_females = 10, # Number of females
                          real_ratio_ints_to_dyad = 3, # Number of times each dyad interacts
                          male_aggro_bias = TRUE, # Whether males don't interact aggresively with subordinate females (TRUE) or if interact same as with other females (FALSE)
                          real_steepness = 1 # real_steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * real_steepness)))
) {
  
  # The number of females (half) at the bottom of the hierarchy that are not aggressed by males
  n_females_no_m_aggro <- round(n_females/2, digits = 0)
  
  # Dataframe of individuals
  inds <- data.frame(ID = LETTERS[1:(n_males+n_females)], # IDs
                     dominance = sort(c(rnorm((n_males), mean = 0, sd = 1), # distribution higher for biomdal (males and females)
                                        rnorm((n_females), mean = -4, sd = 1)), # distribution lower for bimodal (males and females)
                                      decreasing = TRUE), # Dominance
                     sex = c(rep("M", times = n_males), rep("F", times = n_females))) # sex
  
  # Standardise dominance between 0 and no. individuals in group -1, to facilitate comparison of social systems, between metrics and between real and inferred hierarchies
  inds$dominance <- (inds$dominance - min(inds$dominance)) / (max(inds$dominance) - min(inds$dominance)) * ((n_males+n_females) - 1)
  
  ## Generate real interactions
  ints_real <- expand.grid(inds$ID, inds$ID) # get all combinations
  colnames(ints_real) <- c("Ind_A", "Ind_B") # rename columns
  ints_real <- ints_real[-which(ints_real$Ind_A == ints_real$Ind_B),] # remove self-interactions
  ints_real <- ints_real[rep(seq_len(nrow(ints_real)), times = real_ratio_ints_to_dyad),] # repeat certain number of times
  rownames(ints_real) <- 1:nrow(ints_real) # reset rownames
  
  # Simulate outcomes
  ints_real$winner <- NA
  ints_real$loser <- NA
  for (i in 1:nrow(ints_real)) {
    
    # Simulate an interaction
    Ind_A_tmp = ints_real$Ind_A[i]
    Ind_B_tmp = ints_real$Ind_B[i]
    
    # Get rank difference
    rank_diff <- inds$dominance[which(inds$ID == Ind_A_tmp)] - inds$dominance[which(inds$ID == Ind_B_tmp)]
    
    # Get probability that A wins
    p_Ind_A_tmp_wins_this <-   1 / (1 + exp(-(rank_diff * real_steepness)))
    
    # Does A win?
    Ind_A_tmp_won_this <- sample(c(T, F), 1, prob = c(p_Ind_A_tmp_wins_this,1 - p_Ind_A_tmp_wins_this))
    
    # Allocate winner and loser
    if (Ind_A_tmp_won_this) {
      ints_real$winner[i] <- as.character(Ind_A_tmp)
      ints_real$loser[i] <- as.character(Ind_B_tmp)
    } else {
      ints_real$winner[i] <- as.character(Ind_B_tmp)
      ints_real$loser[i] <- as.character(Ind_A_tmp)
    }
  }
  
  # Remove unnecessary columns
  ints_real <- ints_real[,which(colnames(ints_real) %in% c("winner", "loser"))]
  # Assign which females not to be aggressed by males
  inds$dominant_female <- c(rep(NA, times = n_males), rep(1, times = n_females - n_females_no_m_aggro), rep(0, times = n_females_no_m_aggro))
  
  # If the males do not aggress the subordinate half of the females
  if(male_aggro_bias) {
    # IDs of females not to be aggressed by males
    fem_no_m_aggro <- inds$ID[which(inds$dominant_female == 0)]
    # Males
    males <- inds$ID[which(inds$sex == "M")]
    # Remove those interactions
    ints_real <- ints_real[-which(
      ((ints_real$winner %in% males) & # winner is male
         ints_real$loser %in% fem_no_m_aggro) # loser is a female is not to be aggressed by male
      | # OR
        ((ints_real$winner %in% fem_no_m_aggro) & # winner is female
           ints_real$loser %in% males) # loser is a female is not to be aggressed by male
    ),] 
  }
  
  # Return simulated interactions
  return(list(inds = inds, ints_real = ints_real))
}

get.Elo <- function(ints_obs # dataframe holding interactions, containing: ID, winner and loser columns
) {
  # Need aniDom
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
