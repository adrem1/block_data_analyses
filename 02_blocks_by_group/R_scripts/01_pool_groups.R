####################################################
# date: 20250228
# author: RABIT
# title: block analysis by pool groups
###################################################

# set the work dir
setwd("path-to-your-working-directory") #edit with Working Directory Path

# Load necessary libraries
library(dplyr)
library(jsonlite)
library(ggplot2)
library(tidyr)
library(gridExtra)

#initialise workspace
rm(list = ls())

#########################################################
# group data provided by BALANCE
# group data available here:
# https://www.balanceanalytics.io/api/groupdata.json
# load all data
#########################################################

# Load JSON file
json_file <- "data/groupdata.json"  # Update with your file path
json_data <- fromJSON(json_file)

# Convert JSON to dataframe
poolGroups <- as.data.frame(json_data$pool_group_json)

# Rename first column to pool_id_bech32
colnames(poolGroups)[1] <- "pool_id_bech32"

# import epoch block data
epoch_data_list <- list.files(path ="./data", pattern = "^epoch_data_.*\\.csv$", full.names = TRUE )

epochData <- do.call(rbind, lapply(epoch_data_list, read.csv))

#########################################################
# remove missing epoch data
# and clean workspace for clarity 
#########################################################

# Check non numeric values in epoch_no
non_numeric_epochs <- epochData[!is.numeric(as.numeric(epochData$epoch_no)), ]

# Ensure epoch_no is treated as a character type, removing any extra spaces
epochData$epoch_no <- trimws(as.character(epochData$epoch_no))

# Now convert to numeric
epochData$epoch_no <- as.numeric(epochData$epoch_no)

# Check for NAs after conversion
sum(is.na(epochData$epoch_no))

# Find the rows with NA in epoch_no
na_values <- epochData[is.na(epochData$epoch_no), ]
print(na_values) 
#note: there are 6 missing values (out of 6M, so 0.0001%) we'll remove them
# all at the epoch 342-343 boundary

# Remove rows with NA values in epoch_no
epochData <- epochData %>%
  filter(!is.na(epoch_no))

# remove unnecessary objects for clarity
rm(epoch_data_list,json_data,json_file,na_values,non_numeric_epochs)

#########################################################
# classify groups with one pool as single pools
# count pools per group
# merge block and group data 
#########################################################

# Classify pool groups with only one pool as 'SINGLEPOOL'
poolGroups <- poolGroups %>%
  group_by(pool_group) %>%
  mutate(pool_group = ifelse(n_distinct(pool_id_bech32) == 1, "SINGLEPOOL", pool_group)) %>%
  ungroup()

# Count the number of distinct pools in each pool group
pool_counts <- poolGroups %>%
  group_by(pool_group) %>%
  summarise(num_pools = n_distinct(pool_id_bech32), .groups = "drop")

# merge the data frames to have block data with groups
epochDataGroups <- left_join(epochData, poolGroups, by = "pool_id_bech32")

#########################################################
# classify groups (M, L, XL) based on pool counts
#########################################################

# classify M groups 
M_groups <- pool_counts %>%
  filter(num_pools >= 2 & num_pools < 4)

# determine percentage of M groups 
nrow(M_groups)/nrow(pool_counts)

# count total pools in M groups
sum(M_groups$num_pools)

###########################

# classify L groups 
L_groups <- pool_counts %>%
  filter(num_pools >= 4 & num_pools < 8)

# determine percentage of L groups 
nrow(L_groups)/nrow(pool_counts)

# count total pools in L groups
sum(L_groups$num_pools)

###########################

# classify XL groups (exclude SINGLEPOOL group)
XL_groups <- pool_counts %>%
  filter(pool_group != "SINGLEPOOL") %>%
  filter(num_pools >= 8)

# determine percentage of XL groups 
nrow(XL_groups)/nrow(pool_counts)

# count total pools in XL groups
sum(XL_groups$num_pools)

#####################################################
# track block production of XL groups
#####################################################

# Create an empty dataframe to store the results
all_group_pools <- data.frame()

# Loop through each group with 8 or more pools
for (group in XL_groups$pool_group) {
  group_data <- epochDataGroups %>%
    filter(pool_group == group) %>%
    group_by(epoch_no) %>%
    summarise(pool_id_bech32 = n_distinct(pool_id_bech32), .groups = "drop") %>%
    mutate(pool_group = group) %>%
    select(epoch_no, pool_id_bech32, pool_group)
  
  # Combine the data for all groups
  all_group_pools <- bind_rows(all_group_pools, group_data)
}

# Plot the number of pools over epochs for each group in a separate panel
ggplot(all_group_pools, aes(x = epoch_no, y = pool_id_bech32)) +
  geom_line(aes(color = pool_group), size = 1) +  # Line plot for number of pools
  facet_wrap(~ pool_group, scales = "free_y") +  # Create a separate panel for each group, with free y-axis scaling
  labs(title = "Number of Pools (in XL groups) Contributing Blocks",
       x = "Epoch",
       y = "Number of Pools contributing blocks") +  # Remove the legend from the plot
  theme_minimal(base_size = 14) +  # Minimize gridlines and increase base size
  theme(
    panel.background = element_rect(fill = "black"),  # Set black background
    plot.background = element_rect(fill = "black"),   # Set black background
    strip.background = element_rect(fill = "black"),  # Set black background for panel labels
    strip.text = element_text(color = "white"),        # White panel label text
    axis.text = element_text(color = "white"),         # White axis labels
    axis.title = element_text(color = "white"),        # White axis titles
    plot.title = element_text(color = "white"),        # White plot title
    panel.grid = element_blank(),                      # Remove gridlines
    axis.ticks = element_line(color = "white"),        # White axis ticks
    axis.text.y = element_text(color = "white"),       # White y-axis labels
    axis.text.x = element_text(color = "white"),       # White x-axis labels
    axis.ticks.length = unit(0.2, "cm"),               # Adjust axis tick length
    legend.position = "none"                           # Remove legend
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1),  # Ensure integer y-axis labels
                     limits = c(0, NA))  # Set the y-axis to start from 0 and let the upper limit auto-adjust

#####################################################
# track block production of L groups
#####################################################

# Create an empty dataframe to store the results
all_group_pools <- data.frame()

# Loop through each group with 8 or more pools
for (group in L_groups$pool_group) {
  group_data <- epochDataGroups %>%
    filter(pool_group == group) %>%
    group_by(epoch_no) %>%
    summarise(pool_id_bech32 = n_distinct(pool_id_bech32), .groups = "drop") %>%
    mutate(pool_group = group) %>%
    select(epoch_no, pool_id_bech32, pool_group)
  
  # Combine the data for all groups
  all_group_pools <- bind_rows(all_group_pools, group_data)
}

# Plot the number of pools over epochs for each group in a separate panel
ggplot(all_group_pools, aes(x = epoch_no, y = pool_id_bech32)) +
  geom_line(aes(color = pool_group), size = 1) +  # Line plot for number of pools
  facet_wrap(~ pool_group, scales = "free_y") +  # Create a separate panel for each group, with free y-axis scaling
  labs(title = "Number of Pools (in L groups) Contributing Blocks",
       x = "Epoch",
       y = "Number of Pools contributing blocks") +  # Remove the legend from the plot
  theme_minimal(base_size = 14) +  # Minimize gridlines and increase base size
  theme(
    panel.background = element_rect(fill = "black"),  # Set black background
    plot.background = element_rect(fill = "black"),   # Set black background
    strip.background = element_rect(fill = "black"),  # Set black background for panel labels
    strip.text = element_text(color = "white"),        # White panel label text
    axis.text = element_text(color = "white"),         # White axis labels
    axis.title = element_text(color = "white"),        # White axis titles
    plot.title = element_text(color = "white"),        # White plot title
    panel.grid = element_blank(),                      # Remove gridlines
    axis.ticks = element_line(color = "white"),        # White axis ticks
    axis.text.y = element_text(color = "white"),       # White y-axis labels
    axis.text.x = element_text(color = "white"),       # White x-axis labels
    axis.ticks.length = unit(0.2, "cm"),               # Adjust axis tick length
    legend.position = "none"                           # Remove legend
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1),  # Ensure integer y-axis labels
                     limits = c(0, NA))  # Set the y-axis to start from 0 and let the upper limit auto-adjust

#########################################################
# proportion of pools belonging to groups
# including single pools
#########################################################

# Step 1: Compute total pools per epoch and categorize groups into
# XL, L, M, and single pools
epochProportions <- epochDataGroups %>%
  group_by(epoch_no, pool_group) %>%  # Don't filter SINGLEPOOL, include it here
  summarise(distinct_pools = n_distinct(pool_id_bech32), .groups = "drop") %>%
  mutate(group_type = case_when(
    pool_group == "SINGLEPOOL" ~ "Single Pools",  # Explicitly categorize SINGLEPOOL
    pool_group %in% XL_groups$pool_group ~ "XLarge Groups (8+ Pools)",  # xLarge groups 
    pool_group %in% L_groups$pool_group ~ "Large Groups (4-7 Pools)",  # Large groups 
    TRUE ~ "Medium Groups (<4 Pools)"  # Everything else falls into medium groups
  )) %>%
  group_by(epoch_no, group_type) %>%
  summarise(total_pools = sum(distinct_pools), .groups = "drop") %>%
  group_by(epoch_no) %>%
  mutate(proportion = total_pools / sum(total_pools))

# Step 2: Plot proportion over time (including M, L, XL groups and single pools)
ggplot(epochProportions, 
       aes(x = epoch_no, y = proportion, fill = factor
           (group_type, levels = c("XLarge Groups (8+ Pools)", 
              "Large Groups (4-7 Pools)", "Medium Groups (<4 Pools)", "Single Pools")))) +
  geom_area(position = "stack", alpha = 0.7) +
  scale_fill_manual(values = c("XLarge Groups (8+ Pools)" = "#4B0082",  # Dark purple
                               "Large Groups (4-7 Pools)" = "#6A0DAD",  # Medium purple
                               "Medium Groups (<4 Pools)" = "#9370DB",  # Light purple
                               "Single Pools" = "magenta")) +  # Magenta for single pools
  labs(title = "Pools in Groups (M, L, XL) and Single Pools Over Time",
       x = "Epoch",
       y = "Proportion of Pools",
       fill = "Group Type") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "black", color = NA),  # Black background
    panel.background = element_rect(fill = "black", color = NA), # Black panel background
    panel.grid = element_blank(),  # Remove grid lines
    axis.text = element_text(color = "white"),  # White axis text
    axis.title = element_text(color = "white", face = "bold"),  # White axis labels
    plot.title = element_text(color = "white", face = "bold", size = 16, hjust = 0.5),  # White title
    legend.background = element_rect(fill = "black"),  # Black legend background
    legend.key = element_rect(fill = "black"),  # Black legend key background
    legend.text = element_text(color = "white"),  # White legend text
    legend.title = element_text(color = "white", face = "bold")  # White legend title
  )

#########################################################
# blocks produced by pool groups
# including single pools
#########################################################

# Step 1: Categorize groups based on size

group_categories <- poolGroups %>%
  mutate(group_category = case_when(
    pool_group == "SINGLEPOOL" ~ "Single Pools",  # Ensure SINGLEPOOL gets its own category first
    pool_group %in% XL_groups$pool_group ~ "XL",  # xLarge groups 
    pool_group %in% L_groups$pool_group ~ "L",  # Large groups 
    TRUE ~ "M"  # Everything else falls into medium groups
  ))

blockProportions <- left_join(epochDataGroups, group_categories, 
                              by = c("pool_id_bech32", "pool_group")) %>%
  select(-pool_ticker.y) %>%  # Remove the duplicate column
  rename(pool_ticker = pool_ticker.x)  # Rename back to pool_ticker


# Setp 2: Compute block proportions by group_category per epoch
blockProportionsSummary <- blockProportions %>%
  group_by(epoch_no, group_category) %>%
  summarise(blocks_minted = n(), .groups = "drop") %>%  # Count blocks per group_category per epoch
  group_by(epoch_no) %>%
  mutate(proportion = blocks_minted / sum(blocks_minted)) %>%  # Compute proportions
  ungroup()

# Remove rows where group_category is NA
blockProportionsSummary <- blockProportionsSummary %>%
  filter(!is.na(group_category))

# Step 3. Plot proportions over epochs for each group_category

# Define custom colors for each group
group_colors <- c(
  "XL" = "#4B0082",   # Dark Purple
  "L" = "#800080",    # Medium Purple
  "M" = "#D8BFD8",   # Light Purple
  "Single Pools" = "#FF00FF"     # Magenta
)

# Convert group_category to factor with custom order for legend
blockProportionsSummary$group_category <- factor(
  blockProportionsSummary$group_category, 
  levels = c("XL", "L", "M", "Single Pools")  # Set order
)

# Create the plot
ggplot(blockProportionsSummary, aes(x = epoch_no, y = proportion, color = group_category)) +
  geom_line(size = 1) +  # Line plot
  scale_color_manual(values = group_colors) +  # Apply custom colors
  labs(title = "Block Proportion by Group Category Over Epochs",
       x = "Epoch",
       y = "Proportion of Blocks Minted",
       color = "Group Category") +
  theme_minimal(base_size = 14) +  # Clean theme
  theme(
    plot.background = element_rect(fill = "black", color = NA),  # Black background
    panel.background = element_rect(fill = "black", color = NA),  # Black panel
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.text = element_text(color = "white"),  # White axis labels
    axis.title = element_text(color = "white"),  # White axis titles
    plot.title = element_text(color = "white", face = "bold"),  # White plot title
    legend.background = element_rect(fill = "black"),  # Black legend background
    legend.text = element_text(color = "white"),  # White legend text
    legend.title = element_text(color = "white")  # White legend title
  )

#########################################################
# Area plot of block proportions
# Single Pools vs. All Other Groups
#########################################################

# Step 1: Reclassify groups into two categories
blockProportionsSummary <- blockProportions %>%
  filter(!is.na(group_category)) %>%  # Remove NA values
  mutate(group_category = ifelse(group_category == "Single Pools", "Single Pools", "Pool Groups")) %>%
  group_by(epoch_no, group_category) %>%
  summarise(blocks_minted = n(), .groups = "drop") %>%
  group_by(epoch_no) %>%
  mutate(proportion = blocks_minted / sum(blocks_minted)) %>%
  ungroup()

# Define custom colors
group_colors <- c(
  "Single Pools" = "#FF00FF",  # Magenta
  "Pool Groups" = "#4B0082"     # Dark Purple
)

# Convert group_category to factor to maintain order
blockProportionsSummary$group_category <- factor(
  blockProportionsSummary$group_category, 
  levels = c("Pool Groups", "Single Pools")  # Ensure stacked order
)

# Create the area plot
ggplot(blockProportionsSummary, aes(x = epoch_no, y = proportion, fill = group_category)) +
  geom_area(position = "stack", alpha = 0.8) +  # Stacked area plot
  scale_fill_manual(values = group_colors) +  # Apply custom colors
  labs(title = "Single Pools vs. Pool Groups Over Epochs",
       x = "Epoch",
       y = "Proportion of Blocks Minted",
       fill = "Pool Category") +
  theme_minimal(base_size = 14) +  # Clean theme
  theme(
    plot.background = element_rect(fill = "black", color = NA),  # Black background
    panel.background = element_rect(fill = "black", color = NA),  # Black panel
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank(),  # Remove vertical gridlines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical gridlines
    axis.text = element_text(color = "white"),  # White axis labels
    axis.title = element_text(color = "white"),  # White axis titles
    plot.title = element_text(color = "white", face = "bold"),  # White plot title
    legend.background = element_rect(fill = "black"),  # Black legend background
    legend.text = element_text(color = "white"),  # White legend text
    legend.title = element_text(color = "white"),  # White legend title
    panel.border = element_blank(),  # Remove any extra borders
    axis.ticks = element_blank()  # Remove axis ticks
  )

# Compute min, max, and avg proportions per group category
group_stats <- blockProportionsSummary %>%
  group_by(group_category) %>%
  summarise(
    min_proportion = min(proportion),
    max_proportion = max(proportion),
    avg_proportion = mean(proportion),
    .groups = "drop"
  )

# Print the stats
group_stats


#########################################################
# WEIGHTED (by number of pools) blocks produced 
# by pool groups including single pools 
#########################################################

# Count distinct pools per group_category per epoch
pool_counts2 <- blockProportions %>%
  group_by(epoch_no, group_category) %>%
  summarise(num_pools = n_distinct(pool_id_bech32), .groups = "drop")

# Remove rows where group_category is NA
pool_counts2 <- pool_counts2 %>%
  filter(!is.na(group_category))

# Compute block counts per group_category per epoch
block_counts <- blockProportions %>%
  group_by(epoch_no, group_category) %>%
  summarise(blocks_minted = n(), .groups = "drop")

# Remove rows where group_category is NA
block_counts <- block_counts %>%
  filter(!is.na(group_category))

# Merge the two datasets
weightedData <- left_join(block_counts, pool_counts2, by = c("epoch_no", "group_category")) %>%
  mutate(weighted_blocks = blocks_minted / num_pools)  # Weight by pool count

# Compute the total weighted blocks per epoch
total_weighted_blocks <- weightedData %>%
  group_by(epoch_no) %>%
  summarise(total_weighted = sum(weighted_blocks, na.rm = TRUE), .groups = "drop")

# Compute weighted proportions
weightedProportionsSummary <- left_join(weightedData, total_weighted_blocks, by = "epoch_no") %>%
  mutate(weighted_proportion = weighted_blocks / total_weighted) %>%
  select(epoch_no, group_category, weighted_proportion)

# Define custom colors for each group
group_colors <- c(
  "XL" = "#4B0082",   # Dark Purple
  "L" = "#800080",    # Medium Purple
  "M" = "#D8BFD8",   # Light Purple
  "Single Pools" = "#FF00FF"     # Magenta
)

# Convert group_category to factor with custom order for legend
weightedProportionsSummary$group_category <- factor(
  weightedProportionsSummary$group_category, 
  levels = c("XL", "L", "M", "Single Pools")  # Set order
)

# Create the plot
ggplot(weightedProportionsSummary, aes(x = epoch_no, y = weighted_proportion, color = group_category)) +
  geom_line(size = 1) +  # Line plot
  scale_color_manual(values = group_colors) +  # Apply custom colors
  labs(title = "Weighted Block Proportion by Group Category Over Epochs",
       x = "Epoch",
       y = "Weighted Proportion of Blocks Minted",
       color = "Group Category") +
  theme_minimal(base_size = 14) +  # Clean theme
  theme(
    plot.background = element_rect(fill = "black", color = NA),  # Black background
    panel.background = element_rect(fill = "black", color = NA),  # Black panel
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.text = element_text(color = "white"),  # White axis labels
    axis.title = element_text(color = "white"),  # White axis titles
    plot.title = element_text(color = "white", face = "bold"),  # White plot title
    legend.background = element_rect(fill = "black"),  # Black legend background
    legend.text = element_text(color = "white"),  # White legend text
    legend.title = element_text(color = "white")  # White legend title
  )

#########################################################
# MINIMUM number of pools per epoch 
# to mint 50% +1 of epoch blocks 
#########################################################

# Step 1: Count total blocks per epoch
epoch_block_counts <- epochDataGroups %>%
  group_by(epoch_no) %>%
  summarise(total_blocks = n(), .groups = "drop")

# Step 2: Count blocks per pool per epoch
pool_block_counts <- epochDataGroups %>%
  group_by(epoch_no, pool_id_bech32) %>%
  summarise(blocks_minted = n(), .groups = "drop")

# Step 3: Compute minimum number of pools needed for 50% +1 blocks
min_pools_50 <- pool_block_counts %>%
  arrange(epoch_no, desc(blocks_minted)) %>%  # Sort by epoch, then by blocks minted (descending)
  group_by(epoch_no) %>%
  mutate(cumulative_blocks = cumsum(blocks_minted)) %>%  # Compute cumulative sum of blocks
  left_join(epoch_block_counts, by = "epoch_no") %>%  # Add total blocks per epoch
  mutate(threshold = (total_blocks / 2) + 1) %>%  # Calculate 50% + 1 threshold
  filter(cumulative_blocks >= threshold) %>%  # Find first row where cumulative blocks exceed threshold
  summarise(min_pools_needed = n(), .groups = "drop")  # Count pools needed

#########################################################
# MINIMUM number of groups per epoch 
# to mint 50% +1 of epoch blocks 
#########################################################

# Step 1: Count total blocks per epoch
epoch_block_counts <- epochDataGroups %>%
  group_by(epoch_no) %>%
  summarise(total_blocks = n(), .groups = "drop")

# Step 2: Count blocks per group per epoch
group_block_counts <- epochDataGroups %>%
  filter(pool_group != "SINGLEPOOL") %>%  # Exclude SINGLEPOOL
  group_by(epoch_no, pool_group) %>%
  summarise(blocks_minted = n(), .groups = "drop")

# Step 3: Compute minimum number of groups needed for 50% +1 blocks
min_groups_50 <- group_block_counts %>%
  arrange(epoch_no, desc(blocks_minted)) %>%  # Sort by descending block count
  group_by(epoch_no) %>%
  mutate(cumulative_blocks = cumsum(blocks_minted)) %>%  # Compute cumulative blocks
  left_join(epoch_block_counts, by = "epoch_no") %>%  # Add total blocks per epoch
  mutate(threshold = (total_blocks / 2) + 1) %>%  # Compute 50% +1 threshold
  filter(cumulative_blocks >= threshold) %>%  # Keep only rows where cumulative blocks exceed threshold
  summarise(min_groups_needed = n(), .groups = "drop")  # Count the number of groups needed

#########################################################
# combine the 2 datasets and plot them
# to show trends in minimum entities needed for 50% +1 
#########################################################

# merge 50% +1 pools and groups
min_pools_vs_groups <- left_join(min_pools_50, min_groups_50, by = "epoch_no")

# Prepare data for faceting: reshape into long format
min_pools_vs_groups_long <- min_pools_vs_groups %>%
  pivot_longer(cols = c(min_pools_needed, min_groups_needed),
               names_to = "Metric",
               values_to = "Value") %>%
  mutate(Metric = recode(Metric,
                         "min_pools_needed" = "Minimum Pools",
                         "min_groups_needed" = "Minimum Groups"))

# Create the faceted plot
ggplot(min_pools_vs_groups_long, aes(x = epoch_no, y = Value, color = Metric)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +  # Separate panels for each metric
  scale_color_manual(values = c("Minimum Pools" = "magenta", "Minimum Groups" = "purple")) +
  labs(title = "Minimum Entities Needed for 50%+1 Blocks",
       x = "Epoch",
       y = "Minimum Required") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_line(color = "gray30"),
    panel.grid.minor = element_line(color = "gray20"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", face = "bold"),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white", face = "bold"),
    legend.position = "none"
  )

ggplot(min_pools_vs_groups_long, aes(x = epoch_no, y = Value, color = Metric)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +  # Separate panels for each metric
  scale_color_manual(values = c("Minimum Pools" = "magenta", "Minimum Groups" = "purple")) +
  labs(title = "Minimum Entities Needed for 50%+1 Blocks",
       x = NULL,  # Remove x-axis label
       y = NULL) + # Remove y-axis label
  geom_vline(xintercept = 257, linetype = "dashed", color = "white", size = 1) +  # Add dashed line at epoch 257
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_text(color = "white"),  # White axis text
    axis.title = element_text(color = "white"),  # White axis title (hidden above)
    axis.ticks = element_line(color = "white"),  # White axis ticks
    plot.title = element_text(color = "white", face = "bold"),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white", face = "bold"),
    legend.position = "none"
  )
#########################################################
# longevity/stability of pools/groups
#########################################################

# Longevity of pools: First and last epoch when a pool appears
longevity_pools <- epochDataGroups %>%
  group_by(pool_id_bech32) %>%
  summarise(
    first_epoch = min(epoch_no),
    last_epoch = max(epoch_no),
    longevity = last_epoch - first_epoch + 1  # Difference between first and last epoch
  )

# Longevity of groups: First and last epoch when a group appears
longevity_groups <- epochDataGroups %>%
  group_by(pool_group) %>%
  summarise(
    first_epoch = min(epoch_no),
    last_epoch = max(epoch_no),
    longevity = last_epoch - first_epoch + 1  # Difference between first and last epoch
  )

# Define the total number of epochs (fixed value for the analysis period)
total_epochs <- length(unique(epochDataGroups$epoch_no))  # Fixed total epochs over the analysis period

# Stability for pools: Percentage of epochs pool has been active (relative to total epochs)
stability_pools <- epochDataGroups %>%
  group_by(pool_id_bech32) %>%
  summarise(
    active_epochs = n_distinct(epoch_no[!is.na(pool_id_bech32)]),  # Epochs where the pool minted blocks
    stability = active_epochs / total_epochs * 100  # Stability as percentage of total epochs
  )

# Stability for groups: Percentage of epochs group has been active (relative to total epochs)
stability_groups <- epochDataGroups %>%
  group_by(pool_group) %>%
  summarise(
    active_epochs = n_distinct(epoch_no[!is.na(pool_group)]),  # Epochs where the group minted blocks
    stability = active_epochs / total_epochs * 100  # Stability as percentage of total epochs
  )

# Longevity Plot for Pools
longevity_pools_plot <- ggplot() +
  geom_histogram(data = longevity_pools, aes(x = longevity), bins = 30, fill = "magenta", alpha = 0.7) +
  labs(title = "Longevity of Pools", x = "Longevity (Epochs)", y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_line(color = "gray30"),
    panel.grid.minor = element_line(color = "gray20"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", face = "bold"),
    legend.position = "none"
  )

# Stability Plot for Pools
stability_pools_plot <- ggplot() +
  geom_histogram(data = stability_pools, aes(x = stability), bins = 30, fill = "magenta", alpha = 0.7) +
  labs(title = "Stability of Pools", x = "Stability (%)", y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_line(color = "gray30"),
    panel.grid.minor = element_line(color = "gray20"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", face = "bold"),
    legend.position = "none"
  )

# Longevity Plot for Groups
longevity_groups_plot <- ggplot() +
  geom_histogram(data = longevity_groups, aes(x = longevity), bins = 30, fill = "purple", alpha = 0.7) +
  labs(title = "Longevity of Groups", x = "Longevity (Epochs)", y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_line(color = "gray30"),
    panel.grid.minor = element_line(color = "gray20"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", face = "bold"),
    legend.position = "none"
  )

# Stability Plot for Groups
stability_groups_plot <- ggplot() +
  geom_histogram(data = stability_groups, aes(x = stability), bins = 30, fill = "purple", alpha = 0.7) +
  labs(title = "Stability of Groups", x = "Stability (%)", y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_line(color = "gray30"),
    panel.grid.minor = element_line(color = "gray20"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", face = "bold"),
    legend.position = "none"
  )

# Arrange the plots in a 2x2 grid
grid.arrange(longevity_pools_plot, stability_pools_plot, longevity_groups_plot, stability_groups_plot, ncol = 2)


