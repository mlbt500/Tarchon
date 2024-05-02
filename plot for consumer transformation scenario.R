library(ggplot2)
library(scales)
library(grid)

# Filter the merged_arup data frame for the CT scenario
ct_data <- merged_arup[merged_arup$Scenario == "CT", ]

# Replace "Foreign Best Case*" with "Foreign*" in the Country column
ct_data$Country <- ifelse(ct_data$Country == "Foreign Best Case*", "Foreign*", ct_data$Country)

# Reorder the levels of the Adjustment factor and change labels
ct_data$Adjustment <- factor(ct_data$Adjustment, levels = c("Unadjusted", "Adjusted"), labels = c("Original", "Corrected"))

# Create the plot
plot <- ggplot(ct_data, aes(x = Interest.group, y = SEW.bn., fill = Interest.group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f", SEW.bn.), y = SEW.bn. + max(ct_data$SEW.bn.) * 0.05), size = 3, color = "black") +
  facet_grid(Country ~ Adjustment, scales = "free_y") +
  labs(x = NULL,
       y = "SEW (£bn)",
       title = "Accounting for foreign ownership of assets\nTarchon is not in the National Interest") +
  scale_fill_manual(values = c("Consumer" = "#A8B5C8",
                               "Producer" = "#0079C2",
                               "Interconnector" = "#CFDEE4",
                               "Total" = "#F1A2B1")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(30, 20, 10, 20),
        text = element_text(family = "Arial"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", lineheight = 1.2),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90", size = 0.2),
        panel.grid.minor = element_blank())

# Create footnote grobs
footnote1 <- textGrob("*Based on Germany values in the Arup report, but in adjusted version includes foreign ownership marked wrongly as GB.",
                      x = 0.01, y = 0.03, hjust = 0, vjust = 0, gp = gpar(fontsize = 8))
footnote2 <- textGrob("**Data taken from the Consumer Transformation Scenario of the Arup Market Modelling Anaysis p. 87.",
                      x = 0.01, y = 0.01, hjust = 0, vjust = 0, gp = gpar(fontsize = 8))

# Create a circle grob around the GB corrected total bar
gb_total_bar <- ct_data[ct_data$Country == "GB" & ct_data$Adjustment == "Corrected" & ct_data$Interest.group == "Total", ]
circle <- circleGrob(x = unit(0.75, "npc"), y = unit(gb_total_bar$SEW.bn., "native"),
                     r = unit(0.5, "snpc"), gp = gpar(col = "black", lwd = 2),
                     vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# Get the plot layout
plot_layout <- grid.layout(nrow = 4, heights = unit(c(0.475, 0.475, 0.025, 0.025), "npc"))

# Arrange the plot, circle, and footnotes in the layout
grid.newpage()
pushViewport(viewport(layout = plot_layout))
print(plot, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1))
upViewport()
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
grid.draw(circle)
popViewport()
pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
grid.draw(footnote1)
popViewport()
pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1))
grid.draw(footnote2)
popViewport()