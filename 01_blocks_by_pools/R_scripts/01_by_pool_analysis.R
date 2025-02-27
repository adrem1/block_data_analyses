####################
# date: 20250225
# author: RABIT
###################

# set the working directory
setwd("path-to-your-working-directory")

# Load necessary libraries
  library(ggplot2)
  library(dplyr)
  library(tidyr)

#initialise workspace
rm(list = ls())

#########################################
# load and clean all data
#########################################

# import epoch data and bind it
df1 <- read.csv("data/epoch_data_209-218.csv")
df2 <- read.csv("data/epoch_data_219-299.csv")
df3 <- read.csv("data/epoch_data_300-390.csv")
df4 <- read.csv("data/epoch_data_391-539.csv")
allEpochs <- rbind(df1,df2,df3,df4)

# Check non numeric values in epoch_no
non_numeric_epochs <- allEpochs[!is.numeric(as.numeric(allEpochs$epoch_no)), ]

# Ensure epoch_no is treated as a character type, removing any extra spaces
allEpochs$epoch_no <- trimws(as.character(allEpochs$epoch_no))

# Now convert to numeric
allEpochs$epoch_no <- as.numeric(allEpochs$epoch_no)

# Check for NAs after conversion
sum(is.na(allEpochs$epoch_no))

# Find the rows with NA in epoch_no
na_values <- allEpochs[is.na(allEpochs$epoch_no), ]
print(na_values) 
#note: there are 6 missing values (out of 5M, so 0.00012%) we'll remove them
# all at the epoch 342-343 boundary

# Remove rows with NA values in epoch_no
cleanedEpochs <- allEpochs %>%
  filter(!is.na(epoch_no))

# Check the result to confirm the rows are removed
sum(is.na(cleanedEpochs$epoch_no))  # Should return 0

#remove unnecessary objects
rm(df1,df2,df3,df4,na_values,non_numeric_epochs)

#########################################
# summarise data and identify epoch with
# d=0 (eg no more federated blocks)
#########################################

# Count total blocks per epoch
block_counts <- cleanedEpochs %>%
  group_by(epoch_no) %>%
  summarise(total_blocks = n(), .groups = "drop")

# Count blocks with missing pool_id_bech32 per epoch
missing_counts <- cleanedEpochs %>%
  filter(is.na(pool_id_bech32) | pool_id_bech32 == "") %>%  # Handle both NA and empty strings
  group_by(epoch_no) %>%
  summarise(missing_blocks = n(), .groups = "drop")

# Merge results and compute percentage
final_counts <- block_counts %>%
  left_join(missing_counts, by = "epoch_no") %>%
  mutate(missing_blocks = replace_na(missing_blocks, 0),  # Fill NA with 0
         missing_pct = (missing_blocks / total_blocks) * 100)

# Create the plot
ggplot(final_counts, aes(x = epoch_no, y = missing_pct)) +
  geom_line(color = "magenta", size = 1) +  # Magenta line for missing_pct
  theme_minimal(base_size = 14) +  # Clean minimal theme
  theme(
    panel.background = element_rect(fill = "black", color = NA),  # Black background
    plot.background = element_rect(fill = "black", color = NA),
    axis.line = element_line(color = "white"),
    axis.text = element_text(color = "white"),  # White axis text
    axis.title = element_text(color = "white"), # White axis titles
    plot.title = element_text(color = "white", size = 16, face = "bold"), # White title
    axis.text.x = element_text(angle = 90, hjust = 1), # Rotate x-axis labels
    panel.grid = element_blank()  # Remove grid lines
  ) +
  labs(
    title = "Shelley Coming into Effect",
    x = "Epoch Number",
    y = "Percentage of Federated Blocks"
  ) +
  scale_x_continuous(breaks = seq(min(final_counts$epoch_no), max(final_counts$epoch_no), by = 10)) + # X-axis every 10 epochs
  geom_segment(aes(x = 257, xend = 257, y = 0, yend = max(final_counts$missing_pct)), color = "white", linetype = "dashed") +  # White dashed line at epoch 257
  annotate("text", x = 257, y = max(final_counts$missing_pct) * 0.8, label = "Epoch 257", color = "white", angle = 90, vjust = -0.5)  # Annotation for Epoch 257


#########################################
# analyse all data and fit linear
# regressions (# of pools w/block ea epoch)
#########################################

# find number of pools minting each epoch by pool_id
  poolsPerEpoch <- cleanedEpochs %>%
    group_by(epoch_no) %>%
    summarise(num_pools = n_distinct(pool_id_bech32), .groups = "drop")

  # Split data into two subsets
  pools_before_302 <- poolsPerEpoch %>% filter(epoch_no < 302)
  pools_after_302 <- poolsPerEpoch %>% filter(epoch_no >= 302)
  
  # Fit separate linear regression models
  lm_before <- lm(num_pools ~ epoch_no, data = pools_before_302)
  lm_after <- lm(num_pools ~ epoch_no, data = pools_after_302)
  
  # Print summaries
  summary(lm_before)
  summary(lm_after)
  
  ggplot(poolsPerEpoch, aes(x = epoch_no, y = num_pools)) +
    geom_point(color = "white") +  # Scatter points
    
    # Regression line BEFORE epoch 302
    geom_smooth(data = pools_before_302, method = "lm", color = "cyan", fill = "gray", alpha = 0.3, se = TRUE) +
    
    # Regression line AFTER epoch 302
    geom_smooth(data = pools_after_302, method = "lm", color = "magenta", fill = "gray", alpha = 0.3, se = TRUE) +
    
    # Mark epoch 257
    geom_vline(xintercept = 257, color = "white", linetype = "dashed") +
    annotate("text", x = 257, y = max(poolsPerEpoch$num_pools) * 0.8, 
             label = "Epoch 257", color = "white", angle = 90, vjust = -0.5) +
    
    # Labels and theme
    labs(
      title = "Number of Pools Minting Blocks per Epoch (Pre/Post d=0)",
      x = "Epoch Number",
      y = "Number of Pools"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "black", color = NA),
      plot.background = element_rect(fill = "black", color = NA),
      axis.line = element_line(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      plot.title = element_text(color = "white", face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid = element_blank()
    ) +
    scale_x_continuous(breaks = seq(min(poolsPerEpoch$epoch_no), max(poolsPerEpoch$epoch_no), by = 10))

  
  #########################################
  # summarise key metrics
  # 
  #########################################
  
  
  ######################
  # pools with >=1 block
  ######################
  num_unique_pools <- cleanedEpochs %>%
    summarise(unique_pools = n_distinct(pool_id_bech32, na.rm = TRUE))
  
  print(num_unique_pools)
  
  ##################################
  # pools minting in >=10% of epochs
  ##################################
  
  # Count the number of epochs each pool has minted in
  pool_epoch_counts <- cleanedEpochs %>%
    group_by(pool_id_bech32) %>%
    summarise(minted_epochs = n_distinct(epoch_no), .groups = "drop")
  
  # Total number of epochs in the dataset
  total_epochs <- n_distinct(cleanedEpochs$epoch_no)
  
  # Calculate the threshold for 10% of epochs
  threshold_epochs <- total_epochs * 0.10
  
  # Filter pools that minted in at least 10% of epochs
  regularly_minting_pools <- pool_epoch_counts %>%
    filter(minted_epochs >= threshold_epochs)
  
  # Calculate the percentage of pools that mint regularly (at least 10% of epochs)
  percentage_regular_pools <- nrow(regularly_minting_pools) / nrow(pool_epoch_counts) * 100
  
  # Print the result
  print(paste("Percentage of Pools Minting Regularly (at least 10% of epochs):", round(percentage_regular_pools, 2), "%"))
 
  ##################################
  # average pools minting 257-539
  ##################################
  
  # Filter poolsPerEpoch for epochs 257 to 539
  pools_in_range <- poolsPerEpoch %>%
    filter(epoch_no >= 257 & epoch_no <= 539)
  
  # Compute the average number of pools minting blocks in this range
  average_pools_in_range <- mean(pools_in_range$num_pools)
  
  # Print the result
  print(paste("Average number of pools minting blocks between epochs 257 and 539:", round(average_pools_in_range, 2)))

  ##################################
  # unique pools 257-539
  ##################################  
    
  # Filter cleanedEpochs for epochs 257 to 539
  distinct_pools_in_range <- cleanedEpochs %>%
    filter(epoch_no >= 257 & epoch_no <= 539) %>%
    summarise(unique_pools = n_distinct(pool_id_bech32, na.rm = TRUE))
  
  # Print the result
  print(distinct_pools_in_range)
  
  ##################################
  # minimum pools minting in 257-539
  ##################################
  
  min(pools_in_range$num_pools)
 
