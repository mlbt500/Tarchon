library(ggplot2)
library(ggtext)
library(scales)
library(gridExtra)

arup <- read.csv("arup.csv")
# Step 1: Create a new data frame called adjusted_arup by making a copy of the arup data frame
adjusted_arup <- arup

# Step 2: For rows where Country is "GB" and Interest.group is "Producer", multiply the SEW.bn. value by 0.25
adjusted_arup$SEW.bn.[adjusted_arup$Country == "GB" & adjusted_arup$Interest.group == "Producer"] <- 
  adjusted_arup$SEW.bn.[adjusted_arup$Country == "GB" & adjusted_arup$Interest.group == "Producer"] * 0.25

# Step 3: For rows where Country is "Germany" and Interest.group is "Producer", add 0.75 times the original SEW.bn. value from the corresponding "Producer" row where Country is "GB"
gb_producer_sew <- arup$SEW.bn.[arup$Country == "GB" & arup$Interest.group == "Producer"]
adjusted_arup$SEW.bn.[adjusted_arup$Country == "Germany" & adjusted_arup$Interest.group == "Producer"] <- 
  adjusted_arup$SEW.bn.[adjusted_arup$Country == "Germany" & adjusted_arup$Interest.group == "Producer"] + 
  0.75 * gb_producer_sew

# Step 4: For rows where Country is "Germany" and Interest.group is "Interconnector", add the SEW.bn. value from the corresponding "Interconnector" row where Country is "GB"
gb_interconnector_sew <- arup$SEW.bn.[arup$Country == "GB" & arup$Interest.group == "Interconnector"]
adjusted_arup$SEW.bn.[adjusted_arup$Country == "Germany" & adjusted_arup$Interest.group == "Interconnector"] <- 
  adjusted_arup$SEW.bn.[adjusted_arup$Country == "Germany" & adjusted_arup$Interest.group == "Interconnector"] + 
  gb_interconnector_sew

# Step 5: Remove the rows where Country is "GB" and Interest.group is "Interconnector"
adjusted_arup <- adjusted_arup[!(adjusted_arup$Country == "GB" & adjusted_arup$Interest.group == "Interconnector"), ]

# Step 6: For each Scenario and Country combination, recalculate the "Total" SEW.bn. value by summing the adjusted SEW.bn. values for "Consumer", "Producer", and "Interconnector"
for (scenario in unique(adjusted_arup$Scenario)) {
  for (country in unique(adjusted_arup$Country)) {
    total_sew <- sum(adjusted_arup$SEW.bn.[adjusted_arup$Scenario == scenario & 
                                             adjusted_arup$Country == country & 
                                             adjusted_arup$Interest.group != "Total"])
    adjusted_arup$SEW.bn.[adjusted_arup$Scenario == scenario & 
                            adjusted_arup$Country == country & 
                            adjusted_arup$Interest.group == "Total"] <- total_sew
  }
}

# Step 7: Add an "Adjustment" column to both data frames
arup$Adjustment <- "Unadjusted"
adjusted_arup$Adjustment <- "Adjusted"

# Step 8: Merge the two data frames using rbind()
merged_arup <- rbind(arup, adjusted_arup)