library(ggplot2)
library(cowplot)
library(here)

# Source code
source(here("Scripts", "Source_4.0.R"))

# ELO output
previous_run <- "n_sims=500,n_males=16,n_females=10,prop_fem_breeding=0.5,ratio_ints_to_dyad=12,8,4,steepness=1,dom_comp=entire,hierarchy_method=get.Elo.RData"
load(here("Outputs", previous_run))
output_ELO <- output
# PERC output
previous_run <- "n_sims=500,n_males=16,n_females=10,prop_fem_breeding=0.5,ratio_ints_to_dyad=12,8,4,steepness=1,dom_comp=entire,hierarchy_method=get.perc.RData"
load(here("Outputs", previous_run))
output_PERC <- output
# I&SI output
previous_run <- "n_sims=500,n_males=16,n_females=10,prop_fem_breeding=0.5,ratio_ints_to_dyad=12,8,4,steepness=1,dom_comp=entire,hierarchy_method=get.matrix.RData"
load(here("Outputs", previous_run))
output_ISI <- output

## Set up axes and positions depending on dominance comparison

# Positions of the mean & sd points & lines
y_mean_sd_positions <- c(-0.075, -0.18)
# Y limits
y_lims <- c(-0.18,1.8)
# X limits
x_lims <- c(-1.4,12)
# Annotation position
ann_pos <- c(0.65 * x_lims[2], 0.87 * y_lims[2])


### ELO
# # Breeders

# Data, means & SDs
output_plot_A <- output_ELO[which(output_ELO$female_category == "Breeding females"),]
A_unbiased_mean <- mean(output_plot_A$result[which(output_plot_A$bias_type == "Males tolerate all females equally")])
A_unbiased_sd <- sd(output_plot_A$result[which(output_plot_A$bias_type == "Males tolerate all females equally")])
A_biased_mean <- mean(output_plot_A$result[which(output_plot_A$bias_type == "Males tolerate breeding females only")])
A_biased_sd <- sd(output_plot_A$result[which(output_plot_A$bias_type == "Males tolerate breeding females only")])
# Plot
plot_A <- ggplot(output_plot_A, aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = x_lims) +
  scale_y_continuous(limits = y_lims) +
  geom_segment(aes(x = A_unbiased_mean - A_unbiased_sd, xend = A_unbiased_mean + A_unbiased_sd, 
                   y = y_mean_sd_positions[1], yend = y_mean_sd_positions[1]), linewidth = 1, colour = "#56B4E9", lineend = "round", alpha = 0.4) +
  geom_segment(aes(x = A_biased_mean - A_biased_sd, xend = A_biased_mean + A_biased_sd, 
                   y = y_mean_sd_positions[2], yend = y_mean_sd_positions[2]), linewidth = 1, colour = "#E69F00", lineend = "round", alpha = 0.4) +
  geom_point(aes(x = A_unbiased_mean, y = y_mean_sd_positions[1]), size = 2, colour = "#56B4E9", alpha = 0.4) +
  geom_point(aes(x = A_biased_mean, y = y_mean_sd_positions[2]), size = 2, colour = "#E69F00", alpha = 0.4) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "Density") +
  annotate("text", x = ann_pos[1], y = ann_pos[2], angle = 0,
           label = expression(atop(atop(textstyle(bold("Randomised Elo")), textstyle(bold("Ratings"))), italic("Breeding females"))), size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())

# # Non-breeders
# Data, means & SDs
output_plot_B <- output_ELO[which(output_ELO$female_category == "Non-breeding females"),]
B_unbiased_mean <- mean(output_plot_B$result[which(output_plot_B$bias_type == "Males tolerate all females equally")])
B_unbiased_sd <- sd(output_plot_B$result[which(output_plot_B$bias_type == "Males tolerate all females equally")])
B_biased_mean <- mean(output_plot_B$result[which(output_plot_B$bias_type == "Males tolerate breeding females only")])
B_biased_sd <- sd(output_plot_B$result[which(output_plot_B$bias_type == "Males tolerate breeding females only")])
# Plot
plot_B <- ggplot(output_plot_B, aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = x_lims) +
  scale_y_continuous(limits = y_lims) +
  geom_segment(aes(x = B_unbiased_mean - B_unbiased_sd, xend = B_unbiased_mean + B_unbiased_sd, 
                   y = y_mean_sd_positions[1], yend = y_mean_sd_positions[1]), linewidth = 1, colour = "#56B4E9", lineend = "round", alpha = 0.4) +
  geom_segment(aes(x = B_biased_mean - B_biased_sd, xend = B_biased_mean + B_biased_sd, 
                   y = y_mean_sd_positions[2], yend = y_mean_sd_positions[2]), linewidth = 1, colour = "#E69F00", lineend = "round", alpha = 0.4) +
  geom_point(aes(x = B_unbiased_mean, y = y_mean_sd_positions[1]), size = 2, colour = "#56B4E9", alpha = 0.4) +
  geom_point(aes(x = B_biased_mean, y = y_mean_sd_positions[2]), size = 2, colour = "#E69F00", alpha = 0.4) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "") +
  annotate("text", x = ann_pos[1], y = ann_pos[2], angle = 0,
           label = expression(atop(atop(textstyle(bold("Randomised Elo")), textstyle(bold("Ratings"))), italic("Non-breeding females"))), size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())

### I&SI
# # Breeders

# Data, means & SDs
output_plot_C <- output_ISI[which(output_ISI$female_category == "Breeding females"),]
C_unbiased_mean <- mean(output_plot_C$result[which(output_plot_C$bias_type == "Males tolerate all females equally")])
C_unbiased_sd <- sd(output_plot_C$result[which(output_plot_C$bias_type == "Males tolerate all females equally")])
C_biased_mean <- mean(output_plot_C$result[which(output_plot_C$bias_type == "Males tolerate breeding females only")])
C_biased_sd <- sd(output_plot_C$result[which(output_plot_C$bias_type == "Males tolerate breeding females only")])
# Plot
plot_C <- ggplot(output_plot_C, aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = x_lims) +
  scale_y_continuous(limits = y_lims) +
  geom_segment(aes(x = C_unbiased_mean - C_unbiased_sd, xend = C_unbiased_mean + C_unbiased_sd, 
                   y = y_mean_sd_positions[1], yend = y_mean_sd_positions[1]), linewidth = 1, colour = "#56B4E9", lineend = "round", alpha = 0.4) +
  geom_segment(aes(x = C_biased_mean - C_biased_sd, xend = C_biased_mean + C_biased_sd, 
                   y = y_mean_sd_positions[2], yend = y_mean_sd_positions[2]), linewidth = 1, colour = "#E69F00", lineend = "round", alpha = 0.4) +
  geom_point(aes(x = C_unbiased_mean, y = y_mean_sd_positions[1]), size = 2, colour = "#56B4E9", alpha = 0.4) +
  geom_point(aes(x = C_biased_mean, y = y_mean_sd_positions[2]), size = 2, colour = "#E69F00", alpha = 0.4) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "Density") +
  annotate("text", x = ann_pos[1], y = ann_pos[2], angle = 0,
           label = expression(atop(atop(textstyle(bold("I&SI")), textstyle(bold("(matrix ordering)"))), italic("Breeding females"))), size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())

# # Non-breeders
# Data, means & SDs
output_plot_D <- output_ISI[which(output_ISI$female_category == "Non-breeding females"),]
D_unbiased_mean <- mean(output_plot_D$result[which(output_plot_D$bias_type == "Males tolerate all females equally")])
D_unbiased_sd <- sd(output_plot_D$result[which(output_plot_D$bias_type == "Males tolerate all females equally")])
D_biased_mean <- mean(output_plot_D$result[which(output_plot_D$bias_type == "Males tolerate breeding females only")])
D_biased_sd <- sd(output_plot_D$result[which(output_plot_D$bias_type == "Males tolerate breeding females only")])
# Plot
plot_D <- ggplot(output_plot_D, aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = x_lims) +
  scale_y_continuous(limits = y_lims) +
  geom_segment(aes(x = D_unbiased_mean - D_unbiased_sd, xend = D_unbiased_mean + D_unbiased_sd, 
                   y = y_mean_sd_positions[1], yend = y_mean_sd_positions[1]), linewidth = 1, colour = "#56B4E9", lineend = "round", alpha = 0.4) +
  geom_segment(aes(x = D_biased_mean - D_biased_sd, xend = D_biased_mean + D_biased_sd, 
                   y = y_mean_sd_positions[2], yend = y_mean_sd_positions[2]), linewidth = 1, colour = "#E69F00", lineend = "round", alpha = 0.4) +
  geom_point(aes(x = D_unbiased_mean, y = y_mean_sd_positions[1]), size = 2, colour = "#56B4E9", alpha = 0.4) +
  geom_point(aes(x = D_biased_mean, y = y_mean_sd_positions[2]), size = 2, colour = "#E69F00", alpha = 0.4) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "", y = "") +
  annotate("text", x = ann_pos[1], y = ann_pos[2], angle = 0,
           label = expression(atop(atop(textstyle(bold("I&SI")), textstyle(bold("(matrix ordering)"))), italic("Non-breeding females"))), size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())

### PERC
# # Breeders
# Data, means & SDs
output_plot_E <- output_PERC[which(output_PERC$female_category == "Breeding females"),]
E_unbiased_mean <- mean(output_plot_E$result[which(output_plot_E$bias_type == "Males tolerate all females equally")])
E_unbiased_sd <- sd(output_plot_E$result[which(output_plot_E$bias_type == "Males tolerate all females equally")])
E_biased_mean <- mean(output_plot_E$result[which(output_plot_E$bias_type == "Males tolerate breeding females only")])
E_biased_sd <- sd(output_plot_E$result[which(output_plot_E$bias_type == "Males tolerate breeding females only")])
# Plot
plot_E <- ggplot(output_plot_E, aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = x_lims) +
  scale_y_continuous(limits = y_lims) +
  geom_segment(aes(x = E_unbiased_mean - E_unbiased_sd, xend = E_unbiased_mean + E_unbiased_sd, 
                   y = y_mean_sd_positions[1], yend = y_mean_sd_positions[1]), linewidth = 1, colour = "#56B4E9", lineend = "round", alpha = 0.4) +
  geom_segment(aes(x = E_biased_mean - E_biased_sd, xend = E_biased_mean + E_biased_sd, 
                   y = y_mean_sd_positions[2], yend = y_mean_sd_positions[2]), linewidth = 1, colour = "#E69F00", lineend = "round", alpha = 0.4) +
  geom_point(aes(x = E_unbiased_mean, y = y_mean_sd_positions[1]), size = 2, colour = "#56B4E9", alpha = 0.4) +
  geom_point(aes(x = E_biased_mean, y = y_mean_sd_positions[2]), size = 2, colour = "#E69F00", alpha = 0.4) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "Mean Inferred Minus Real Hierarchy Position", y = "Density") +
  annotate("text", x = ann_pos[1], y = ann_pos[2], angle = 0,
           label = expression(atop(atop(textstyle(bold("Percolation and")), textstyle(bold("Conductance"))), italic("Breeding females"))), size = 3, parse = TRUE) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank())

# # Non-breeders
# Data, means & SDs
output_plot_F <- output_PERC[which(output_PERC$female_category == "Non-breeding females"),]
F_unbiased_mean <- mean(output_plot_F$result[which(output_plot_F$bias_type == "Males tolerate all females equally")])
F_unbiased_sd <- sd(output_plot_F$result[which(output_plot_F$bias_type == "Males tolerate all females equally")])
F_biased_mean <- mean(output_plot_F$result[which(output_plot_F$bias_type == "Males tolerate breeding females only")])
F_biased_sd <- sd(output_plot_F$result[which(output_plot_F$bias_type == "Males tolerate breeding females only")])
# Plot
plot_F <- ggplot(output_plot_F, aes(result, colour = bias_type, fill = bias_type)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = x_lims) +
  scale_y_continuous(limits = y_lims) +
  geom_segment(aes(x = F_unbiased_mean - F_unbiased_sd, xend = F_unbiased_mean + F_unbiased_sd, 
                   y = y_mean_sd_positions[1], yend = y_mean_sd_positions[1]), linewidth = 1, colour = "#56B4E9", lineend = "round", alpha = 0.4) +
  geom_segment(aes(x = F_biased_mean - F_biased_sd, xend = F_biased_mean + F_biased_sd, 
                   y = y_mean_sd_positions[2], yend = y_mean_sd_positions[2]), linewidth = 1, colour = "#E69F00", lineend = "round", alpha = 0.4) +
  geom_point(aes(x = F_unbiased_mean, y = y_mean_sd_positions[1]), size = 2, colour = "#56B4E9", alpha = 0.4) +
  geom_point(aes(x = F_biased_mean, y = y_mean_sd_positions[2]), size = 2, colour = "#E69F00", alpha = 0.4) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  labs(x = "Mean Inferred Minus Real Hierarchy Position", y = "") +
  annotate("text", x = ann_pos[1], y = ann_pos[2], angle = 0,
           label = expression(atop(atop(textstyle(bold("Percolation and")), textstyle(bold("Conductance"))), italic("Non-breeding females"))), size = 3, parse = TRUE) +
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
                            rel_widths = c(1,0.05,1),
                            rel_heights = c(1,1,1.1),
                            nrow = 3)

# Get Legend
legend_plot <- ggplot(output_plot_A, aes(result, colour = bias_type, fill = bias_type)) +
  geom_density(bw = 0.17, alpha = 0.4, linewidth = 1.2) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") +
  guides(color = guide_legend(nrow = 1)) +
  theme_classic(base_size = 15) +
  theme(legend.position = "bottom")
legend <- get_legend(legend_plot)


# Save combined results plot to PDF
pdf(here("Figures", "Combined_ELO_PERC_ISI_results_mean_sd_dom_comp=entire.pdf"), height = 8, width = 9) # save
# Combine the multi-plot and the legend
plot_grid(combined_plots, NULL, legend, ncol = 1, rel_heights = c(1, 0.01, .05))
dev.off() # finish
