library(ggplot2)
library(scales)
library(grid)

# Filter the merged_arup data frame for the CT scenario
ct_data <- merged_arup[merged_arup$Scenario == "CT", ]

# Replace "Germany" with "Foreign Best Case*" in the Country column
ct_data$Country <- ifelse(ct_data$Country == "Germany", "Foreign Best Case*", ct_data$Country)

# Reorder the levels of the Adjustment factor
ct_data$Adjustment <- factor(ct_data$Adjustment, levels = c("Unadjusted", "Adjusted"))

# Create the plot
plot <- ggplot(ct_data, aes(x = Interest.group, y = SEW.bn., fill = Interest.group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f", SEW.bn.), y = SEW.bn. / 2), size = 3, color = "black") +
  facet_grid(Country ~ Adjustment, scales = "free_y") +
  labs(x = NULL,
       y = "SEW (£bn)") +
  scale_fill_manual(values = c("Consumer" = "#1E88E5",
                               "Producer" = "#FFC107",
                               "Interconnector" = "lightgreen",
                               "Total" = "#D81B60")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(10, 20, 10, 20),
        text = element_text(family = "Arial"))

# Create a footnote grob
footnote <- textGrob("*Based on Germany values in the Arup report, but in adjusted version includes foreign ownership marked wrongly as GB.",
                     x = 0.01, y = 0.01, hjust = 0, vjust = 0, gp = gpar(fontsize = 8))

# Create arrow grobs
arrow_gb <- segmentsGrob(x0 = 0.75, y0 = 0.5, x1 = 0.25, y1 = 0.5, arrow = arrow(angle = 30, length = unit(0.2, "inches"), type = "closed"), gp = gpar(col = "red", lwd = 3))
arrow_foreign <- segmentsGrob(x0 = 0.75, y0 = 0.5, x1 = 0.25, y1 = 0.5, arrow = arrow(angle = 30, length = unit(0.2, "inches"), type = "closed"), gp = gpar(col = "red", lwd = 3))

# Get the plot layout
plot_layout <- grid.layout(nrow = 3, heights = unit(c(0.475, 0.475, 0.05), "npc"))

# Arrange the plot, arrows, and footnote in the layout
grid.newpage()
pushViewport(viewport(layout = plot_layout))
print(plot, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1))
upViewport()
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.draw(arrow_gb)
popViewport()
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.draw(arrow_foreign)
popViewport()
pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
grid.draw(footnote)
popViewport()