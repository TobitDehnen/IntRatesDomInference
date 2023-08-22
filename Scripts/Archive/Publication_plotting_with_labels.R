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
A <- ggplot(output_ELO[which(output_ELO$female_category == "Breeding Females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_linedraw(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_y_continuous(limits = c(0,1.7)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00", "black"), name = "") +
  scale_colour_manual(values = c("black", "#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "Frequency") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  geom_text(size = 4, aes(x = 0.8, y = 1.6, label = "Rand. Elo:\n Breeding females", colour = "black"))
# non-breeders
B <- ggplot(output_ELO[which(output_ELO$female_category == "Non-breeding Females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_linedraw(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_y_continuous(limits = c(0,1.7)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00", "black"), name = "") +
  scale_colour_manual(values = c("black", "#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) + 
  geom_text(size = 4, aes(x = 0.8, y = 1.6, label = "Rand. Elo:\n Non-breeding females", colour = "black"))

### PERC
# breeders
C <- ggplot(output_PERC[which(output_PERC$female_category == "Breeding Females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_linedraw(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_y_continuous(limits = c(0,1.7)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00", "black"), name = "") +
  scale_colour_manual(values = c("black", "#56B4E9", "#E69F00"), name = "") +
  labs(x = "Mean Inferred Minus Real Hierarchy Position", y = "Frequency") + 
  geom_text(size = 4, aes(x = 0.8, y = 1.6, label = "Perc:\n Breeding females", colour = "black"))
# non-breeders
D <- ggplot(output_PERC[which(output_PERC$female_category == "Non-breeding Females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_linedraw(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_y_continuous(limits = c(0,1.7)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00", "black"), name = "") +
  scale_colour_manual(values = c("black", "#56B4E9", "#E69F00"), name = "") +
  labs(x = "Mean Inferred Minus Real Hierarchy Position", y = "") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) + 
  geom_text(size = 4, aes(x = 0.8, y = 1.6, label = "Perc:\n Non-breeding females", colour = "black"))

# Combine plots
combined_plots <- plot_grid(A + theme(legend.position = "none"),
                            NULL,
                            B + theme(legend.position = "none"),
                            C + theme(legend.position = "none"),
                            NULL,
                            D + theme(legend.position = "none"),
                            hjust = -0.4,
                            labels = c("A","B", "","C", "D",""), 
                            rel_widths = c(1,0.05,1,1,0.05,1))

# Get Legend
legend_b <- get_legend(ggplot(output_ELO, aes(result, colour = bias_type, fill = bias_type)) +
                         geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
                         scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") + 
                         scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") + 
                         guides(color = guide_legend(nrow = 1)) +
                         theme_linedraw(base_size = 15) +
                         theme(legend.position = "bottom"))


# Save combined results plot to PDF
pdf(here("Figures", "Combined_ELO_and_PERC_results_labeled.pdf"), height = 6, width = 8) # save
# Combine the 4 plots and the legend
plot_grid(combined_plots, NULL, legend_b, ncol = 1, rel_heights = c(1, 0.03, .07))
dev.off() # finish
