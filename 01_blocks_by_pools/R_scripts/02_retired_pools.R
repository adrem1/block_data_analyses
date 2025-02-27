####################
# date: 20250225
# author: RABIT
###################

# set the working directory
setwd("path-to-your-working-directory")

# load packages
library(dplyr)
library(jsonlite)
library(ggplot2)
library(tidyr)

#initialise workspace
rm(list = ls())

#########################################
# load and clean all data
#########################################

# Load pool info data (for pools without tickers)
no_ticker_pool_info <- read.csv("data/no_ticker_pool_info.csv", stringsAsFactors = FALSE, header = FALSE, fill = TRUE)
colnames(no_ticker_pool_info) <- c("pool_id_bech32", "meta_json", "pool_status", "retiring_epoch", "block_count", "live_pledge", "live_stake", "live_delegators")
no_ticker_pool_info <- no_ticker_pool_info[-1, -9]

# transorm lovelaces into ada
no_ticker_pool_info$live_pledge <- as.numeric(no_ticker_pool_info$live_pledge) / 1e6
no_ticker_pool_info$live_stake <- as.numeric(no_ticker_pool_info$live_stake) / 1e6

# Create a new column for the extracted ticker
no_ticker_pool_info$ticker <- NA  # Placeholder column

# Extract ticker where JSON is present
for (i in seq_along(no_ticker_pool_info$meta_json)) {
  if (no_ticker_pool_info$meta_json[i] != "null") {
    json_data <- fromJSON(no_ticker_pool_info$meta_json[i], simplifyVector = TRUE)
    no_ticker_pool_info$ticker[i] <- json_data$ticker
  }
}

# Reorder columns to insert ticker right after meta_json (column 3)
no_ticker_pool_info <- no_ticker_pool_info[, c(1,2, ncol(no_ticker_pool_info), 3:(ncol(no_ticker_pool_info)-1))]

# View the modified dataframe
head(no_ticker_pool_info)


# Count total number of pools
total_pools <- nrow(no_ticker_pool_info)
cat("Total pools:", total_pools, "\n")

# Count pools grouped by pool_status
pool_status_counts <- table(no_ticker_pool_info$pool_status)
print(pool_status_counts)

###################################################
# closer look at the retired pools 
###################################################

# Filter out retired pools
retired_pools <- no_ticker_pool_info[no_ticker_pool_info$pool_status == "retired", ]

# Filter retired pools with blocks
retired_pools_with_blocks <- retired_pools[retired_pools$block_count >= 1, ]

# block to numerical
retired_pools_with_blocks$block_count <- as.numeric(retired_pools_with_blocks$block_count)

#sum blocks minted by retired pools

sum(retired_pools_with_blocks$block_count)

###################################################
# plot retiring pools over time 
###################################################

# Convert retiring_epoch to numeric (if not already)
no_ticker_pool_info$retiring_epoch <- as.numeric(no_ticker_pool_info$retiring_epoch)

# Filter out pools with missing retiring_epoch values
retired_pools <- subset(no_ticker_pool_info, !is.na(retiring_epoch))

# Count the number of retired pools per epoch
retired_pools_count <- as.data.frame(table(retired_pools$retiring_epoch))
colnames(retired_pools_count) <- c("epoch", "count")

# convert epoch values to numeric (from factor)
retired_pools_count <- retired_pools_count %>%
  mutate(epoch = as.numeric(as.character(epoch)))

# sum total retired pools
sum(retired_pools_count$count)

# calculate cumulative count in retired_pools_count
retired_pools_count <- retired_pools_count %>%
  arrange(epoch) %>%  # Ensure the data is ordered by epoch
  mutate(cumulative_count = cumsum(count))  # Calculate cumulative sum of 'count'

#plot number of retired pools over epochs
ggplot(retired_pools_count, aes(x = epoch, y = cumulative_count)) +
  geom_line(color="magenta") +
  geom_point(color = "magenta", size = 1) +
  scale_x_continuous(
    breaks = seq(200, 550, by = 10),  # Force labels every 10 epochs
    limits = c(200, 550)  # Ensure x-axis starts & ends at these values
  ) +
  theme_minimal() +  
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(color="white"),
    axis.line = element_line(color="white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, color = "white"),  
    panel.grid = element_blank()
  ) +
  labs(title = "Number of Retired Pools Over Epochs",
       x = "Epoch",
       y = "Number of Retired Pools (in epoch)")

################################################################################
################################################################################
################################################################################

# Make subset for 2020 count
retired_pools_count_2020 <- retired_pools_count %>%
  filter(epoch %in% 209:239)

sum(retired_pools_count_2020$count) #returns 287

# Make subset for 2021 count
retired_pools_count_2021 <- retired_pools_count %>%
  filter(epoch %in% 240:312)

sum(retired_pools_count_2021$count) #returns 737

# Make subset for 2022 count
retired_pools_count_2022 <- retired_pools_count %>%
  filter(epoch %in% 313:385)

sum(retired_pools_count_2022$count) #returns 347

# Make subset for 2023 count
retired_pools_count_2023 <- retired_pools_count %>%
  filter(epoch %in% 386:458)

sum(retired_pools_count_2023$count) #returns 226

# Make subset for 2024 count
retired_pools_count_2024 <- retired_pools_count %>%
  filter(epoch %in% 459:531)

sum(retired_pools_count_2024$count) #returns 123

# Make subset for 2025 count
retired_pools_count_2025 <- retired_pools_count %>%
  filter(epoch %in% 532:539)

sum(retired_pools_count_2025$count) #returns 13
