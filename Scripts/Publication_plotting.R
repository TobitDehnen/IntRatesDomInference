# ELO output
previous_run <- "n_sims=500,n_males=16,n_females=10,prop_fem_breeding=0.5,ratio_ints_to_dyad=12,8,4,steepness=1,dom_comp=female,hierarchy_method=get.Elo.RData"
load(here("Outputs", previous_run))
output_ELO <- output
# PERC output
previous_run <- "n_sims=500,n_males=16,n_females=10,prop_fem_breeding=0.5,ratio_ints_to_dyad=12,8,4,steepness=1,dom_comp=female,hierarchy_method=get.perc.RData"
load(here("Outputs", previous_run))
output_PERC <- output

library(cowplot)

### ELO
# breeders
ELO_breed <- ggplot(output_ELO[which(output_ELO$female_category == "Breeding Females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_linedraw(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "Frequency") +
  theme(legend.position = "none")
# non-breeders
ELO_non_breed <- ggplot(output_ELO[which(output_ELO$female_category == "Non-breeding Females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_linedraw(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "") +
  theme(legend.position = "none")

### PERC
# breeders
PERC_breed <- ggplot(output_PERC[which(output_PERC$female_category == "Breeding Females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_linedraw(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "Mean Inferred Minus Real Hierarchy Position", y = "Frequency") +
  theme(legend.position = "none")
# non-breeders
PERC_non_breed <- ggplot(output_PERC[which(output_PERC$female_category == "Non-breeding Females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_linedraw(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "Mean Inferred Minus Real Hierarchy Position", y = "") +
  theme(legend.position = "none")


# Save combined results plot to PDF
pdf(here("Figures", "Combined_ELO_and_PERC_results.pdf"), height = 5, width = 8) # save
# Combine plots
plot_grid(ELO_breed, ELO_non_breed, PERC_breed, PERC_non_breed, labels = "AUTO", ncol = 2, nrow = 2)
dev.off() # finish

## To Do:
# single colour code legend (centred at bottom)—see complex examples in plot_grid (maybe I can add a central plot at the bottom of just the legend)
# save wide so that entire x axis labels are printed