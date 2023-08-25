### SET UP

# Clear the workspace
rm(list = ls())
# Set seed
set.seed(1)

# Packages not in functions
library(here) # path
library(igraph) # network
library(parallel) # multiple core lapply function
library(cowplot) # combine networks

# Source code
source(here("Scripts", "Source_4.0.R"))

# Colours for males and breeding + non-breeding females
male_col <- "gray90"
female_breeder_col <- "#009e73"
female_nonbreeder_col <- "#e69f00"

# Colours for F<->F, F<->M and M<->M edges
edges_FF <- adjustcolor("#0072b2", 0.8)
edges_FM <- adjustcolor("#56b4e9", 0.4)
edges_MM <- adjustcolor("gray70", 0.25)

# bias_cats to plot
bias_cats <- c("Males tolerate breeding females only", "Males tolerate all females equally")

# Save networks into this list
networks <- vector(mode = "list", length = length(bias_cats))



### MAKE NETWORKS ####
for(i in 1:length(networks)) {
  # Generate 'real interactions' for virtual population
  ints_obs <- get.real.ints(n_males = 16, # Number of males
                            n_females = 10, # Number of females
                            prop_fem_breeding = 0.5, # Proportion of females not to be aggressed
                            ratio_ints_to_dyad = c(12,8,4), # Number of times each MM, MF, FF dyad interacts, last number must be even
                            bias_type = bias_cats[i], # "Males tolerate all females equally" or "Males tolerate breeding females only"
                            steepness = 1, # steepness of sigmoidal function: larger numbers create steeper hierarchies via: probability A wins = 1 / (1 + exp(-(rank_diff * steepness)))
                            n_cores = 8, # number of cores to use when generating interaction outcomes
                            for_network_illustration = T # if it's to simply illustrate the network, always allocate the same females as breeders/non-breeders
  )
  
  
  # Individual attributes
  attributes <- ints_obs$inds
  # edges
  edgelist <- ints_obs$ints_real
  
  # Make directed winner-loser network
  edges <- graph.data.frame(edgelist)
  matrix <- get.adjacency(edges, sparse = FALSE)
  matrix <- matrix[sort(rownames(matrix)), sort(colnames(matrix))] # order so it's the same for both plots (A->Z)
  graph <- graph.adjacency(matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  
  # Generate and save coordinates for plotting both graphs the same
  if(i == 1) {
    layout_graph <- layout_with_graphopt(graph, niter = 500, charge = 0.1)
    base_coords <- data.frame(ID = colnames(matrix), dim_1 = layout_graph[,1], dim_2 = layout_graph[,2])
  }
  
  ## Node colours as grey for males and red/blue for breeding/non-breeding females
  attributes$colour <- NA
  for(j in 1:nrow(attributes)) {
    if(attributes$sex[j] == "M") {
      attributes$colour[j] <- male_col
    } else{attributes$colour[j] <- ifelse(attributes$breeding_female[j] == 1, female_breeder_col, female_nonbreeder_col)}
  }
  V(graph)$colour = attributes$colour[match(V(graph)$name, attributes$ID)]
  
  ## Assigning sex to node shape, males square and females circular
  V(graph)$shape = attributes$sex[match(V(graph)$name, attributes$ID)]
  V(graph)$shape = gsub("M","square",V(graph)$shape)
  V(graph)$shape = gsub("F","circle",V(graph)$shape)
  
  ## Assign intrinsic dominance to node size
  V(graph)$size = attributes$dominance[match(V(graph)$name, attributes$ID)] + 5
  
  ## Assign edge colours depending on the sex of the nodes the edge connects
  graph <- set_edge_attr(graph,
                         name = "color",
                         value = c(edges_FF, edges_FM, edges_MM)
                         [1 + (as.integer(as.data.frame(ends(graph, E(graph)))[,1] %in% attributes$ID[which(attributes$sex =="M")]) +
                                 as.integer(as.data.frame(ends(graph, E(graph)))[,2] %in% attributes$ID[which(attributes$sex =="M")]))]
  )
  
  # Node label size
  V(graph)$label.cex = 1.1
  
  # Set margins
  if(i == 1) {
    par(mar = c(0,0,3,0))  # leave space on top
  } else {
    par(mar = c(0,0,3,0))  # leave space on top
  }
  
  ## Plot the network
  plot(graph, # graph object
       vertex.label = NA, # ID
       edge.width = E(graph)$weight/5, # edge weights
       edge.curved = 0.3, # curved edges
       vertex.size = sqrt(V(graph)$size * 10), # upscale so size of smallest is not 0
       vertex.color = V(graph)$colour,
       vertex.label.color = "black",
       vertex.frame.color = "black",
       vertex.frame.width = 1, # width of node frames
       edge.arrow.size = 0.5,
       layout = as.matrix(base_coords[,2:3])) # keep layout
  
  # Annotate the graph
  mtext(bias_cats[i], # text
        side = 3, # top
        line = 0, # first line
        cex = 1.7, # size
        font = 3 # bold
  ) # label
  
  # Add legend
  if(i == 2) {
    legend(x = 0.23, y = -0.67,
           title = "Nodes Types",
           legend = c("Males","Breeding females", "Non-breeding females"),
           col = c(male_col, female_breeder_col, female_nonbreeder_col),
           pch = c(15, 16, 16),
           cex = 1.1,
           pt.cex = 2,
           bty = "o",
           box.lty = 1,
           box.lwd = 3,
           box.col = adjustcolor("black", 0.8),
           bg = adjustcolor("white", 0.6),
           y.intersp = 1,
           ncol = 1)
  }
  # Save network
  networks[[i]] <- recordPlot()
}



### COMBINE NETWORKS AND SAVE THEM ####

## First combine each plot with a narrow NULL plot (to prevent overlapping when combining)
# The first network (biased)
net1 <- plot_grid(networks[[1]], 
                  NULL,
                  labels = c("", ""), ncol = 2,
                  rel_widths = c(0.8,0))
# The first network (unbiased)
net2 <- plot_grid(networks[[2]], 
                  NULL,
                  labels = c("", ""), ncol = 2,
                  rel_widths = c(0.8,0))

# Combine the two networks
combined_plots <- plot_grid(net2,
                            NULL, # to prevent overlap
                            net1,
                            hjust = -0.4,
                            labels = c("A", "", "B"), 
                            label_size = 18,
                            ncol = 3,
                            rel_widths = c(1,0.3,1),
                            rel_heights = c(1,1,1))


# Save networks to PDF
pdf(here("Figures", paste("Aggression_networks", ".pdf", sep = "")), height = 6, width = 13) # save
combined_plots
dev.off() # finish
