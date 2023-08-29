library(ggplot2)
library(cowplot)
library(here)


# ELO output
previous_run <- "n_sims=500,n_males=16,n_females=10,prop_fem_breeding=0.5,ratio_ints_to_dyad=12,8,4,steepness=1,dom_comp=female,hierarchy_method=get.Elo.RData"
load(here("Outputs", previous_run))
output_ELO <- output
# PERC output
previous_run <- "n_sims=500,n_males=16,n_females=10,prop_fem_breeding=0.5,ratio_ints_to_dyad=12,8,4,steepness=1,dom_comp=female,hierarchy_method=get.perc.RData"
load(here("Outputs", previous_run))
output_PERC <- output
# I&SI output
previous_run <- "n_sims=500,n_males=16,n_females=10,prop_fem_breeding=0.5,ratio_ints_to_dyad=12,8,4,steepness=1,dom_comp=female,hierarchy_method=get.matrix.RData"
load(here("Outputs", previous_run))
output_ISI <- output

### ELO
# breeders
plot_A <- ggplot(output_ELO[which(output_ELO$female_category == "Breeding females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_y_continuous(limits = c(0,1.8)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "Density") +
  annotate("text", x = 0.9, y = 1.6, angle = 0,
           label = 'atop(bold("Elo rand."),italic("Breeding females"))', size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())
# non-breeders
plot_B <- ggplot(output_ELO[which(output_ELO$female_category == "Non-breeding females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_y_continuous(limits = c(0,1.8)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "") +
  annotate("text", x = 0.9, y = 1.6, angle = 0,
           label = 'atop(bold("Elo rand."),italic("Non-breeding females"))', size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())

### I&SI
# breeders
plot_C <- ggplot(output_ISI[which(output_ISI$female_category == "Breeding females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_y_continuous(limits = c(0,1.8)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "Density") +
  annotate("text", x = 0.9, y = 1.6, angle = 0,
           label = 'atop(bold("I&SI"),italic("Breeding females"))', size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())
# non-breeders
plot_D <- ggplot(output_ISI[which(output_ISI$female_category == "Non-breeding females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_y_continuous(limits = c(0,1.8)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "") +
  annotate("text", x = 0.9, y = 1.6, angle = 0,
           label = 'atop(bold("I&SI"),italic("Non-breeding females"))', size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())

### PERC
# breeders
plot_E <- ggplot(output_PERC[which(output_PERC$female_category == "Breeding females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_y_continuous(limits = c(0,1.8)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "Mean Inferred Minus Real Hierarchy Position", y = "Density") +
  annotate("text", x = 0.9, y = 1.6, angle = 0,
           label = 'atop(bold("Perc. & Cond."),italic("Breeding females"))', size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())
# non-breeders
plot_F <- ggplot(output_PERC[which(output_PERC$female_category == "Non-breeding females"),], aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_y_continuous(limits = c(0,1.8)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "Mean Inferred Minus Real Hierarchy Position", y = "") +
  annotate("text", x = 0.9, y = 1.6, angle = 0,
           label = 'atop(bold("Perc. & Cond."),italic("Non-breeding females"))', size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())

# Combine plots
combined_plots <- plot_grid(plot_A + theme(legend.position = "none"),
                            NULL,
                            plot_B + theme(legend.position = "none"),
                            plot_C + theme(legend.position = "none"),
                            NULL,
                            plot_D + theme(legend.position = "none"),
                            plot_E + theme(legend.position = "none"),
                            NULL,
                            plot_F + theme(legend.position = "none"),
                            hjust = -0.4,
                            labels = c("A", "B", "",
                                       "C", "D", "",
                                       "E", "F", ""), 
                            rel_widths = rep(c(1,0.05,1), times = 3))

# Get Legend
legend_b <- get_legend(plot_A + 
                         guides(color = guide_legend(nrow = 1)) +
                         theme_classic(base_size = 15) +
                         theme(legend.position = "bottom"))


# Save combined results plot to PDF
pdf(here("Figures", "Combined_ELO_PERC_ISI_results.pdf"), height = 8, width = 8) # save
# Combine the multi-plot and the legend
plot_grid(combined_plots, NULL, legend_b, ncol = 1, rel_heights = c(1, 0.01, .05))
dev.off() # finish
