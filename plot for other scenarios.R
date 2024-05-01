# Install and load the required packages if not already installed
# install.packages(c("ggplot2", "scales", "grid"))
library(ggplot2)
library(scales)
library(grid)

# Filter the merged_arup data frame for the LW and FS scenarios
lw_fs_data <- merged_arup[merged_arup$Scenario %in% c("LW", "FS"), ]

# Replace "Germany" with "Foreign Best Case*" in the Country column
lw_fs_data$Country <- ifelse(lw_fs_data$Country == "Germany", "Foreign Best Case*", lw_fs_data$Country)

# Reorder the levels of the Adjustment factor
lw_fs_data$Adjustment <- factor(lw_fs_data$Adjustment, levels = c("Unadjusted", "Adjusted"))

# Create the plot
plot <- ggplot(lw_fs_data, aes(x = Interest.group, y = SEW.bn., fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f", SEW.bn.), group = Scenario),
            position = position_dodge(width = 0.9), vjust = 0, size = 3, color = "black") +
  facet_grid(Country ~ Adjustment, scales = "free_y", space = "free_y",
             labeller = labeller(Country = label_wrap_gen(width = 12))) +
  labs(x = NULL,
       y = "SEW (£bn)") +
  scale_fill_manual(values = c("LW" = "#F8766D", "FS" = "#00BFC4")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(10, 20, 10, 20),
        text = element_text(family = "Arial"))

# Create a footnote grob
footnote <- textGrob("*Based on Germany values in the Arup report, but in adjusted version includes foreign ownership marked wrongly as GB.",
                     x = 0.01, y = 0.01, hjust = 0, vjust = 0, gp = gpar(fontsize = 8))

# Get the plot layout
plot_layout <- grid.layout(nrow = 2, heights = unit(c(0.95, 0.05), "npc"))

# Arrange the plot and footnote in the layout
grid.newpage()
pushViewport(viewport(layout = plot_layout))
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
upViewport()
grid.draw(footnote)