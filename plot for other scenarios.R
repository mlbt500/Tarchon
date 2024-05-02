# Install and load the required packages if not already installed
# install.packages(c("ggplot2", "scales", "grid"))

library(ggplot2)
library(scales)
library(grid)

# Filter the merged_arup data frame for the LW and FS scenarios
lw_fs_data <- merged_arup[merged_arup$Scenario %in% c("LW", "FS"), ]

# Replace "Foreign Best Case*" with "Foreign*" in the Country column
lw_fs_data$Country <- ifelse(lw_fs_data$Country == "Foreign Best Case*", "Foreign*", lw_fs_data$Country)

# Reorder the levels of the Adjustment factor and change labels
lw_fs_data$Adjustment <- factor(lw_fs_data$Adjustment, levels = c("Unadjusted", "Adjusted"), labels = c("Original", "Corrected"))

# Create the plot
plot <- ggplot(lw_fs_data, aes(x = Interest.group, y = SEW.bn., fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f", SEW.bn.), group = Scenario, y = SEW.bn. / 2),  # Adjust y position
            position = position_dodge(width = 0.9), size = 3, color = "black") +
  facet_grid(Country ~ Adjustment, scales = "free_y", space = "free_y",
             labeller = labeller(Country = label_wrap_gen(width = 12))) +
  labs(x = NULL,
       y = "SEW (£bn)",
       title = "This is true for Arup's two other scenarios,\nFalling Short and Leading the Way") +
  scale_fill_manual(values = c("LW" = "#F1A2B1", "FS" = "#0079C2")) +  # Pink and blue colors
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(30, 20, 10, 20),
        text = element_text(family = "sans"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", lineheight = 1.2),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90", size = 0.2),
        panel.grid.minor = element_blank())

# Create footnote grobs
footnote1 <- textGrob("*Based on Germany values in the Arup report, but in adjusted version includes foreign ownership marked wrongly as GB.",
                      x = 0.01, y = 0.03, hjust = 0, vjust = 0, gp = gpar(fontsize = 8, fontfamily = "sans"))
footnote2 <- textGrob("**Data taken from the Consumer Transformation Scenario of the Arup Market Modelling Anaysis p. 87.",
                      x = 0.01, y = 0.01, hjust = 0, vjust = 0, gp = gpar(fontsize = 8, fontfamily = "sans"))

# Get the plot layout
plot_layout <- grid.layout(nrow = 4, heights = unit(c(0.475, 0.475, 0.025, 0.025), "npc"))

# Arrange the plot and footnotes in the layout
grid.newpage()
pushViewport(viewport(layout = plot_layout))
print(plot, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1))
upViewport()
pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
grid.draw(footnote1)
popViewport()
pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1))
grid.draw(footnote2)
popViewport()