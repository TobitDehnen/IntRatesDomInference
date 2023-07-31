# Script to:
# A) Generate a social group, in which individuals linearly differ in dominance, and 
# simulate 'real' interactions among group members, with winning probability varying 
# with difference in dominance through a sigmoidal function.

# Set seed
set.seed(1)

#### FUNCTIONS ####

# Function to generate 'real interactions' for a virtual population
get.real.ints <- function(n_males = 16, # Number of males
                          n_females = 10, # Number of females
                          n_fem_breed = 3, # Number of females not to be aggressed
                          ratio_ints_to_dyad = c(9,6,3), # Number of times each MM, MF, FF dyad interacts
                          male_aggro_bias = TRUE, # Whether males don't interact aggresively with subordinate females (TRUE) or if interact same as with other females (FALSE)
                          steepness = 1 # steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * steepness)))
) {
  
  # Individuals and their dominance
  inds <- data.frame(ID = LETTERS[1:(n_males+n_females)], # IDs
                     dominance = sort(c(rnorm((n_males), mean = 0, sd = 1), # distribution higher for biomdal (males and females)
                                        rnorm((n_females), mean = -4, sd = 1)), # distribution lower for bimodal (males and females)
                                      decreasing = TRUE), # Dominance
                     sex = c(rep("M", times = n_males), rep("F", times = n_females))) # sex
  
  # Standardise dominance between 0 and no. individuals in group -1
  inds$dominance <- (inds$dominance - min(inds$dominance)) / (max(inds$dominance) - min(inds$dominance)) * ((n_males+n_females) - 1)
  
  ## Generate real interactions
  # MM
  ints_real_MM <- expand.grid(Ind_A = inds$ID[which(inds$sex == "M")], Ind_B = inds$ID[which(inds$sex == "M")]) # get all combinations
  ints_real_MM <- ints_real_MM[-which(ints_real_MM$Ind_A == ints_real_MM$Ind_B),] # remove self-interactions
  ints_real_MM <- ints_real_MM[rep(seq_len(nrow(ints_real_MM)), times = ratio_ints_to_dyad[1]),] # repeat certain number of times
  # MF
  ints_real_MF <- expand.grid(Ind_A = inds$ID[which(inds$sex == "M")], Ind_B = inds$ID[which(inds$sex == "F")]) # get all combinations
  ints_real_MF <- ints_real_MF[rep(seq_len(nrow(ints_real_MF)), times = ratio_ints_to_dyad[2]),] # repeat certain number of times
  # FF
  ints_real_FF <- expand.grid(Ind_A = inds$ID[which(inds$sex == "F")], Ind_B = inds$ID[which(inds$sex == "F")]) # get all combinations
  ints_real_FF <- ints_real_FF[-which(ints_real_FF$Ind_A == ints_real_FF$Ind_B),] # remove self-interactions
  ints_real_FF <- ints_real_FF[rep(seq_len(nrow(ints_real_FF)), times = ratio_ints_to_dyad[3]),] # repeat certain number of times
  
  # Combine, shuffle and reset rownames
  ints_real <- rbind(ints_real_MM, ints_real_MF, ints_real_FF)
  ints_real <- ints_real[sample(nrow(ints_real)),]
  rownames(ints_real) <- 1:nrow(ints_real)
  
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
    p_Ind_A_tmp_wins_this <-   1 / (1 + exp(-(rank_diff * steepness)))
    
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

  # Assign which females breed
  inds$breeding_female <- NA
  inds$breeding_female[which(inds$sex == "F")] <- sample(c(rep(1, times = n_fem_breed), rep(0, times = n_females - n_fem_breed)), size = n_females)
  
  
  # If the males do not aggress the subordinate half of the females
  if(male_aggro_bias) {
    # IDs of females not to be aggressed by males
    fem_no_m_aggro <- inds$ID[which(inds$breeding_female == 1)]
    # Males
    males <- inds$ID[which(inds$sex == "M")]
    # Remove those interactions
    ints_real <- ints_real[-which(
      ((ints_real$winner %in% males) & # winner is male
         ints_real$loser %in% fem_no_m_aggro) # loser is a female is not to interact with males
      | # OR
        ((ints_real$winner %in% fem_no_m_aggro) & # winner is female not to interact with males
           ints_real$loser %in% males) # loser is a male
    ),] 
  }
  
  # Return simulated interactions
  return(list(inds = inds, ints_real = ints_real))
}

get.Elo <- function(ints_obs # dataframe holding interactions, containing: ID, winner and loser columns
) {
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
