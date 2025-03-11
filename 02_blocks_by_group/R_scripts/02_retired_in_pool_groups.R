#######################################
# date: 20250311
# author: RABIT
# title: retired pools by known groups
#######################################

# set the working directory
setwd("path-to-your-working-directory") # edit with Working Directory

# load packages
library(dplyr)
library(jsonlite)
library(ggplot2)
library(tidyr)

#initialise workspace
rm(list = ls())

#########################################
# load pool info, clean data and
# select retired pools only
#########################################

# Load pool info data (for pools without tickers)
no_ticker_pool_info <- read.csv("data/no_ticker_pool_info.csv", stringsAsFactors = FALSE, header = FALSE, fill = TRUE)
colnames(no_ticker_pool_info) <- c("pool_id_bech32", "meta_json", "pool_status", "retiring_epoch", "block_count", "live_pledge", "live_stake", "live_delegators")
no_ticker_pool_info <- no_ticker_pool_info[-1, -9]

# filter out retired pools
retired_pools <- no_ticker_pool_info[no_ticker_pool_info$pool_status == "retired", ]

#########################################################
# group data provided by BALANCE
# load group data and
# classify groups w/ 1 pool as single
#########################################################

# Load JSON file
json_file <- "data/groupdata.json"  # Update with your file path
json_data <- fromJSON(json_file)

# Convert JSON to dataframe
poolGroups <- as.data.frame(json_data$pool_group_json)

# Rename first column to pool_id_bech32
colnames(poolGroups)[1] <- "pool_id_bech32"

# Classify pool groups with only one pool as 'SINGLEPOOL'
poolGroups <- poolGroups %>%
  group_by(pool_group) %>%
  mutate(pool_group = ifelse(n_distinct(pool_id_bech32) == 1, "SINGLEPOOL", pool_group)) %>%
  ungroup()

# add group size category

poolGroups <- poolGroups %>%
  group_by(pool_group) %>%
  mutate(group_size = case_when(
    pool_group == "SINGLEPOOL" ~ "Single Pools",
    n_distinct(pool_id_bech32) >= 8 ~ "XL group",
    n_distinct(pool_id_bech32) >= 4 ~ "L group",
    n_distinct(pool_id_bech32) >= 2 ~ "M group",
    TRUE ~ "Single Pools"  # Default case, should not be needed unless there are unexpected values
  )) %>%
  ungroup()

#########################################################
# load epoch block data
# and clean out na epochs 
#########################################################

# import epoch block data
epoch_data_list <- list.files(path ="./data", pattern = "^epoch_data_.*\\.csv$", full.names = TRUE )

epochData <- do.call(rbind, lapply(epoch_data_list, read.csv))

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
rm(no_ticker_pool_info,epoch_data_list,json_data,json_file,na_values,non_numeric_epochs)

#########################################################
# merge group data with epoch block data
#########################################################

# merge the data frames to have block data with groups
epochDataGroups <- left_join(epochData, poolGroups, by = "pool_id_bech32")



#########################################################
# add retirement status to group data
#########################################################

epochDataGroups <- epochDataGroups %>%
  left_join(retired_pools %>% select(pool_id_bech32, pool_status, retiring_epoch), by = "pool_id_bech32")


# count retired pools by group
retired_pools_by_group <- epochDataGroups %>%
  filter(pool_status == "retired") %>%
  group_by(pool_group) %>%
  summarise(retired_pools = n_distinct(pool_id_bech32)) %>%
  arrange(desc(retired_pools))  # Optional: Sort by count
