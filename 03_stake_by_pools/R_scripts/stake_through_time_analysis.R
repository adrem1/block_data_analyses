###############################
# date: 2025-04-24
# author: RABIT
# title: stake through time
###############################

# initialise workspace
rm(list = ls())

# load required packages
library(dplyr)
library(ggplot2)
library(scales)  # For comma axes formatting
library(patchwork) #for multi-panel plots
library(DescTools) # Gini coefficient
library(tidyr)
library(jsonlite)
library(pROC) # for glm fit analysis
library(ggridges) # for xs pools retirement ridge plot

# set working dir
setwd("<PATH-TO-YOUR-WORKING-DIRECTORY") # modify this accordingly

##############################################################
# load stake epoch data
##############################################################

# load stake data
stake_data_list <- list.files(path ="./data", pattern = "^pool_stake_history_.*\\.csv$", full.names = TRUE )

stakeData <- do.call(rbind, lapply(stake_data_list, read.csv))

# convert lovelaces to numeric data
stakeData$active_stake <- as.numeric(as.character(stakeData$active_stake))

# convert lovelaces to ADA
stakeData$active_stake <- stakeData$active_stake / 10^6

# remove NAs from dataset
stakeData <- stakeData[complete.cases(stakeData), ]

# clean workspace
rm(stake_data_list)

##############################################################
# create reusable theme for plotting 
# this is the one used for ALL plots in this script
##############################################################

# plot theme creation
dark_theme <- theme(
  plot.background = element_rect(fill = "black", color = NA),
  panel.background = element_rect(fill = "black", color = NA),
  panel.grid = element_blank(),
  axis.text = element_text(color = "white"),
  axis.title = element_text(color = "white"),
  plot.title = element_text(color = "white", hjust = 0.5),
  legend.background = element_rect(fill = "black"),
  legend.text = element_text(color = "white"),
  legend.title = element_text(color = "white"),
  axis.line = element_line(color = "white"),
  axis.ticks = element_line(color = "white"),
  axis.ticks.length = unit(0.25, "cm")  # optional
)

##############################################################
# create reusable function to arrange
# plots in multi-panel figures
##############################################################

# plot arranging function
arrange_plots <- function(plots, nrow = NULL, ncol = NULL) {
  # Combine all plots using reduce and `+` operator
  layout <- Reduce(`+`, plots)
  layout + plot_layout(nrow = nrow, ncol = ncol)
}

##############################################################
# calculate share of staked ADA by pools over epochs
##############################################################

stakeData <- stakeData %>%
  group_by(epoch_no) %>%
  mutate(
    total_stake = sum(active_stake),
    stake_share = active_stake / total_stake
  )
##############################################################
# % of total staked ADA held in the top 10, 50 or 100 pools
# note that data can be filtered after epoch 257 when d=0
##############################################################

top_pool_share <- stakeData %>%
  filter(epoch_no >= 257) %>%
  group_by(epoch_no) %>%
  arrange(desc(active_stake), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  summarise(
    top10_share = sum(stake_share[rank <= 10], na.rm = TRUE),
    top50_share = sum(stake_share[rank <= 50], na.rm = TRUE),
    top100_share = sum(stake_share[rank <= 100], na.rm = TRUE),
    .groups = "drop"
  )

##############################################################
# calculate the Gini coefficient: the Gini
# coefficient is a statistical measure of wealth inequality
# in a population. It ranges from 0 to 1, where 0 represents
# perfect equality (everyone has the same wealth) and 1 is the 
# opposite, where one individual has all the wealth and
# everyone else has none.
# note that data can be filtered after epoch 257 when d=0
##############################################################

gini_by_epoch <- stakeData %>%
  #filter(epoch_no >= 257) %>%
  group_by(epoch_no) %>%
  summarise(
    gini = Gini(stake_share, na.rm = TRUE),
    .groups = "drop"
  )
##############################################################
# plots of top pools and Gini coefficient
##############################################################

# Top pools share plot
top_plot <- ggplot(top_pool_share, aes(x = epoch_no)) +
  geom_line(aes(y = top100_share, color = "Top 100"), linewidth = 1) +
  geom_line(aes(y = top50_share, color = "Top 50"), linewidth = 1) +
  geom_line(aes(y = top10_share, color = "Top 10"), linewidth = 1) +
  #geom_vline(xintercept = 257, linetype = "dashed", color = "white", linewidth = 0.2) +
  scale_color_manual(
    values = c("Top 100" = "magenta", "Top 50" = "purple", "Top 10" = "purple4"), 
    breaks = c("Top 100", "Top 50", "Top 10")  # Ensures legend order
  ) +
  labs(title = "Stake Share of Top Pools Over Time",
       x = "Epoch", y = "Stake Share", color = "Pool Group") +
  dark_theme

# Gini plot
gini_plot <- ggplot(gini_by_epoch, aes(x = epoch_no, y = gini)) +
  geom_line(color = "magenta", linewidth = 1) +
  geom_vline(xintercept = 257, linetype = "dashed", color = "white", linewidth = 0.2) +
  labs(title = "Gini Coefficient of Stake Distribution Over Time",
       x = "Epoch", y = "Gini Coefficient") +
  dark_theme

# clean workspace
rm(top_pool_share, gini_by_epoch)

##############################################################
# closer look at top 100 pools' composition
# can be filtered for epochs after d=0
# this will produce both a stacked plot of number of pools  
# by stake size and a heatmap of top pools over time 
##############################################################

# Define alpha levels for bins
bin_alpha_levels <- c(
  "0–40M"  = 0.2,
  "40–50M" = 0.3,
  "50–60M" = 0.45,
  "60–70M" = 0.6,
  "70–80M" = 0.75,
  "80M+"   = 1
)

# manually define legend entries
bin_levels <- names(bin_alpha_levels)

# group top 100 pools after d=0 into 10M ADA bins
stake_bins <- stakeData %>%
  filter(epoch_no >= 257) %>%
  group_by(epoch_no) %>%
  arrange(desc(active_stake), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 100) %>%
  ungroup() %>%
  mutate(
    stake_m = active_stake / 1e6,
    stake_bin = cut(
      stake_m,
      breaks = c(0, 40, 50, 60, 70, 80, Inf),
      labels = bin_levels,
      right = FALSE
    )
  )

# count pools in each bin
bin_counts_by_epoch <- stake_bins %>%
  group_by(epoch_no, stake_bin) %>%
  summarise(count = n(), .groups = "drop")

# Use factor to preserve order
bin_counts_by_epoch$stake_bin <- factor(bin_counts_by_epoch$stake_bin, levels = bin_levels)

# stacked barplot
top100_plot <- ggplot(bin_counts_by_epoch, aes(x = epoch_no, y = count, fill = stake_bin, alpha = stake_bin)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  scale_fill_manual(
    name = "Stake Bin",
    values = rep("#FF00FF", length(bin_levels))  # All magenta
  ) +
  scale_alpha_manual(
    name = "Stake Bin",
    values = bin_alpha_levels
  ) +
  labs(
    title = "Distribution of Size in Top 100 Pools",
    x = "Epoch",
    y = "Number of Pools"
  ) +
  dark_theme +
  theme(legend.position = "right")

# clean up workspace
rm(bin_counts_by_epoch, stake_bins, bin_alpha_levels, bin_levels)

# generate data in heatmap-ready format (top 100 pools)
# convert ADA to ADA (M) for ease of interpretation
# add cap of 100M ADA to max stake to exclude outliers

heatmap_data <- stakeData %>%
  filter(epoch_no >= 257) %>%
  group_by(epoch_no) %>%
  arrange(desc(active_stake), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 100) %>%
  ungroup() %>%
  mutate(
    active_stake_m = active_stake / 1e6,
    capped_stake_m = pmin(active_stake_m, 100)  # Cap at 100M ADA
  )

# plot the heatmap
# the constraints for size are based on the findings above 
heatmap_plot <- ggplot(heatmap_data, aes(x = factor(epoch_no), y = rank, fill = capped_stake_m)) +
  geom_tile() +
  scale_fill_gradient(
    low = "black", high = "#FF00FF", name = "Stake (M ADA)",
    limits = c(60, 80),
    oob = scales::squish  # Clamp values to the range
  ) +
  scale_y_reverse(breaks = c(1, 25, 50, 75, 100)) +
  scale_x_discrete(breaks = function(x) x[as.integer(x) %% 50 == 0]) +
  labs(
    title = "Top 100 Pools by Stake (60M–80M+ ADA)",
    x = "Epoch",
    y = "Pool Rank"
  ) +
  dark_theme +
  theme(
    axis.text.x = element_text(vjust = 0.5, size = 10),
    panel.grid = element_blank()
  )

# clean up
rm(heatmap_data)

##############################################################
# pools for 50% +1 of staked ADA control
# can be filtered for d=0 onwards
##############################################################

pools_needed <- stakeData %>%
  #filter(epoch_no >= 257) %>%
  arrange(epoch_no, desc(active_stake)) %>%
  group_by(epoch_no) %>%
  mutate(
    total_stake = sum(active_stake, na.rm = TRUE),
    threshold = total_stake * 0.5 + 1,
    cumulative_stake = cumsum(active_stake)
  ) %>%
  filter(cumulative_stake < threshold | row_number() == min(which(cumulative_stake >= threshold))) %>%
  summarise(
    num_pools = n(),
    total_stake = first(total_stake),  # retain one value per epoch
    .groups = "drop"
  )

##############################################################
# plot total stake and pools for 50%+1 over time
##############################################################

# Plot 1: Total Staked ADA Over Time
totAda_plot <- ggplot(pools_needed, aes(x = epoch_no, y = total_stake)) +
  geom_line(color = "white", size = 1) +
  geom_vline(xintercept = 257, linetype = "dashed", color = "white", linewidth = 0.2) +
  scale_y_continuous(
    name = "Total Staked ADA",
    labels = scales::comma
  ) +
  dark_theme +
  labs(
    title = "Total Staked ADA Over Time",
    x = "Epoch"
  )

# Plot 2: Number of Pools Needed to Control 50% + 1 of Stake
fifthPercent_plot <- ggplot(pools_needed, aes(x = epoch_no, y = num_pools)) +
  geom_line(color = "magenta", size = 1) +
  #geom_vline(xintercept = 257, linetype = "dashed", color = "white", linewidth = 0.2) +
  dark_theme +
  labs(
    title = "Number of Pools Needed to Control 50%+1 of Stake",
    x = "Epoch",
    y = "Number of Pools"
  )

# clean workspace 
rm(pools_needed)

##############################################################
# load retired pool data and transform it
# then merge it with existing stake data 
# using pool_id_bech32 to merge
##############################################################

# Load pool info data
pool_info <- read.csv("data/no_ticker_pool_info.csv", stringsAsFactors = FALSE, header = FALSE, fill = TRUE)
colnames(pool_info) <- c("pool_id_bech32", "meta_json", "pool_status", "retiring_epoch", "block_count", "live_pledge", "live_stake", "live_delegators")
pool_info <- pool_info[-1, -9]

# Filter out retired pools
retired_pools <- pool_info[pool_info$pool_status == "retired", ]

# merge
stakeData <- stakeData %>%
  left_join(
    retired_pools %>%
      select(pool_id_bech32, pool_status, retiring_epoch),
    by = "pool_id_bech32"
    )

##############################################################
# find and plot the size of the pools at retirement
##############################################################

# pool size in ADA at retirement
retired_stakes <- stakeData %>%
  filter(epoch_no >= 257) %>%
  filter(!is.na(pool_status), pool_status == "retired",
         !is.na(retiring_epoch), epoch_no <= retiring_epoch) %>%
  group_by(pool_id_bech32) %>%
  filter(epoch_no == max(epoch_no)) %>%
  ungroup() %>%
  select(pool_id_bech32, active_stake, epoch_no)

# add pool size "bins" in 10M ADA increments

# Step 1: Calculate max stake rounded up to nearest 10M
max_stake <- ceiling(max(retired_stakes$active_stake, na.rm = TRUE) / 1e7) * 1e7

# Step 2: Create breaks at every 10M interval
breaks <- seq(0, max_stake, by = 1e7)

# Step 3: Create matching labels for each bin
labels <- paste0(head(breaks, -1) / 1e6, "-", tail(breaks, -1) / 1e6, "M")

# Step 4: Bin the data
retired_stakes <- retired_stakes %>%
  mutate(
    stake_bin = cut(
      active_stake,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE
    )
  )

# plot
retired_stakes$stake_bin <- factor(retired_stakes$stake_bin, levels = labels, ordered = TRUE)


sizeRet_plot <- ggplot(retired_stakes, aes(x = stake_bin)) +
  geom_bar(fill = "magenta") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3, color = "white") +
  labs(
    title = "Number of Retired Pools by Stake (since d=0)",
    x = "Stake Bin (ADA)",
    y = "Number of Retired Pools"
  ) +
  dark_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# clean workspace
rm(breaks, labels, max_stake, pool_info, retired_pools, retired_stakes)

##############################################################
# it appears that pools are retiring when small
# there could be an artifact, due to movement of stake
# when retirement is announced. to address this, we calculate
# min_stake, max_stake and delta_stake (per epoch) for each
# pool. we'll use these to further analyse the lifetime of 
# retired pools
##############################################################

# delta_stake (changes in stake across epochs, per pool) 
stakeData <- stakeData %>%
  arrange(pool_id_bech32, epoch_no) %>%
  group_by(pool_id_bech32) %>%
  mutate(delta_stake = active_stake - lag(active_stake)) %>%
  ungroup()

# min_stake (minimum lifetime stake, per pool)
stakeData <- stakeData %>%
  group_by(pool_id_bech32) %>%
  mutate(min_stake = min(active_stake, na.rm = TRUE)) %>%
  ungroup()

# max_stake (maximum lifetime stake, per pool)
stakeData <- stakeData %>%
  group_by(pool_id_bech32) %>%
  mutate(max_stake = max(active_stake, na.rm = TRUE)) %>%
  ungroup()

##############################################################
# closer look at small retired pools
# how small are they?
##############################################################

# Create new df of only retired pools with max_stake in 0-10M ADA
small_retired_pools <- stakeData %>%
  filter(epoch_no >= 257) %>%
  filter(pool_status == "retired") %>%
  group_by(pool_id_bech32) %>%
  filter(max_stake <= 1e7) %>%
  filter(epoch_no == max(epoch_no)) %>%
  ungroup()

# Set up 1M step breaks (0 to 10M)
breaks_1M <- seq(0, 1e7, by = 1e6)
labels_1M <- paste0(head(breaks_1M, -1)/1e6, "-", tail(breaks_1M, -1)/1e6, "M")

# Bin the pools by their max stake
small_retired_pools <- small_retired_pools %>%
  mutate(
    stake_bin = cut(
      max_stake,
      breaks = breaks_1M,
      labels = labels_1M,
      include.lowest = TRUE,
      right = FALSE
    )
  )

# Fix bin order
small_retired_pools$stake_bin <- factor(small_retired_pools$stake_bin, levels = labels_1M)

# plot a histo of retired mall pools by stake bin (in 1M ADA increments)
smallRet_plot <- ggplot(small_retired_pools, aes(x = stake_bin)) +
  geom_bar(fill = "magenta") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3, color = "white") +
  labs(
    title = "Retired Pools with ≤10M Max Stake (post d=0)",
    x = "Max Stake Bin (ADA)",
    y = "Number of Retired Pools"
  ) +
  dark_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

##############################################################
# closer look at x-small retired pools
# how small are they?
# when did they retire?
##############################################################

# Prepare ridge plot data with 100-epoch bins
ridge_data <- small_retired_pools %>%
  mutate(
    stake_m = active_stake / 1e6,
    epoch_group = cut(
      epoch_no,
      breaks = c(seq(250, 500, by = 100), 540),  # Ensure last bin includes 539
      include.lowest = TRUE,
      right = FALSE
    )
  )

# Define custom magenta gradient for bins
magenta_gradient <- colorRampPalette(c("#4B004B", "#FF00FF"))(length(unique(ridge_data$epoch_group)))

# Ridge plot with magenta gradient
retPoolDensity_plot <- ggplot(ridge_data, aes(x = stake_m, y = epoch_group, height = ..density.., group = epoch_group, fill = epoch_group)) +
  geom_density_ridges(scale = 2, rel_min_height = 0.01, alpha = 0.9, color = "white") +
  scale_fill_manual(values = magenta_gradient, name = "Epoch Group") +
  coord_cartesian(xlim = c(0, 0.4)) +
  labs(
    title = "Stake of Small Retired Pools (grouped by Retirement Epoch)",
    x = "Active Stake (Millions of ADA)",
    y = "Retired Pools Density"
  ) +
  dark_theme +
  theme(
    axis.text.y = element_blank()
  )

# clean workspace
rm(ridge_data, small_retired_pools, breaks_1M, labels_1M, magenta_gradient)

##############################################################
# so, did stake drop before retirement?
# if it did, by how much?
# are pools just dropping out, or shrinking to death?
##############################################################

# Calculate drop in stake at retirement
retired_drops <- stakeData %>%
  filter(
    epoch_no >= 257,
    pool_status == "retired",
    epoch_no == retiring_epoch
  ) %>%
  mutate(
    drop_from_max = max_stake - active_stake
  )

# Bin the absolute drop values
max_drop <- ceiling(max(retired_drops$drop_from_max, na.rm = TRUE) / 1e7) * 1e7
drop_breaks <- seq(0, max_drop, by = 1e7)
drop_labels <- paste0(head(drop_breaks, -1) / 1e6, "-", tail(drop_breaks, -1) / 1e6, "M")

retired_drops <- retired_drops %>%
  mutate(
    drop_bin = cut(
      drop_from_max,
      breaks = drop_breaks,
      labels = drop_labels,
      include.lowest = TRUE,
      right = FALSE
    )
  )

# order the bins 
retired_drops$drop_bin <- factor(retired_drops$drop_bin, levels = drop_labels)

# plot of stake drop before retirement
dropRet_plot <- ggplot(retired_drops, aes(x = drop_bin)) +
  geom_bar(fill = "purple") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3, color = "white") +
  labs(
    title = "Absolute Stake Drop (ADA) Before Retirement (Post Epoch 257)",
    x = "Drop in ADA (Millions)",
    y = "Number of Retired Pools"
  ) +
  dark_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# clean up
rm(drop_breaks, drop_labels, max_drop, retired_drops)

##############################################################
# retirement timing
# is announcing retirement effective? is stake moving out of
# pools prior to retirement? if so, how many epochs prior?
##############################################################

# Ensure the data is sorted properly
stakeData <- stakeData %>%
  arrange(pool_id_bech32, epoch_no)

# For each retired pool, find the last epoch before consistent decline started
decline_start <- stakeData %>%
  filter(epoch_no >= 257) %>%
  filter(pool_status == "retired", epoch_no <= retiring_epoch) %>%
  group_by(pool_id_bech32) %>%
  arrange(epoch_no) %>%
  mutate(
    stake_lead = lead(active_stake),
    is_decline = active_stake > stake_lead,
    decline_started_epoch = ifelse(is_decline, epoch_no, NA)
  ) %>%
  # Filter out groups where all decline_started_epoch are NA
  filter(any(!is.na(decline_started_epoch))) %>%
  summarise(
    retiring_epoch = as.numeric(max(retiring_epoch, na.rm = TRUE)),
    decline_start_epoch = as.numeric(min(decline_started_epoch, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    epochs_declining = as.numeric(retiring_epoch) - as.numeric(decline_start_epoch)
  ) %>%
  filter(!is.na(epochs_declining) & epochs_declining >= 0)


# plot the time of decline before retirement
# pools (many, possibly most) are retiring shortly after their size declines
timeRet_plot <- ggplot(decline_start, aes(x = epochs_declining)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "white") +
  #geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3, color = "white") +
  labs(
    title = "Epochs of Stake Decline Before Retirement",
    x = "Number of Epochs Declining",
    y = "Number of Pools"
  ) +
  dark_theme

##############################################################
# is stake declining fast before retirement?
##############################################################

# merge epoch decline
stakeData_with_decline <- stakeData %>%
  left_join(
    decline_start %>% select(pool_id_bech32, decline_start_epoch),
    by = "pool_id_bech32"
  )

# filter data to analyse only the period of decline
# prior to retirement
decline_period_data <- stakeData_with_decline %>%
  filter(
    pool_status == "retired",
    !is.na(decline_start_epoch),
    epoch_no >= decline_start_epoch,
    epoch_no <= retiring_epoch
  )

# calculate decline slope for each retired pool
stake_decline_trend <- decline_period_data %>%
  group_by(pool_id_bech32) %>%
  summarise(
    stake_change = max(active_stake, na.rm = TRUE) - min(active_stake, na.rm = TRUE),
    epochs_declining = max(epoch_no) - min(epoch_no),
    rate_of_decline = stake_change / epochs_declining,
    .groups = "drop"
  )

# plot
rateRet_plot <- ggplot(stake_decline_trend, aes(x = rate_of_decline)) +
  geom_histogram(fill = "purple", bins = 10) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Distribution of Stake Decline Rate Before Retirement",
    x = "ADA Lost per Epoch During Decline",
    y = "Number of Pools"
  ) +
  dark_theme

# clean up
rm(decline_period_data, decline_start, stake_decline_trend, stakeData_with_decline)

##############################################################
# maximum stake vs retirement pattern
# are large pools more or less likely to retire compared to
# small pools?
##############################################################

# summarise dataset to fit glm 
pool_summary <- stakeData %>%
  group_by(pool_id_bech32) %>%
  summarise(
    max_stake = max(max_stake, na.rm = TRUE),
    pool_status = first(pool_status),
    .groups = "drop"
  ) %>%
  mutate(
    retired = as.integer(!is.na(pool_status) & pool_status == "retired")
  )

# fit GLM to examine relationship between pool size and probability 
# of retirement:
# as max stake increases, likelihood of retirement decreases
# coeff: -2.363e-08 (log-odds)
# log-odds * 10M ADA = -0.2363
# exp(-0.2363) ~ 0.79: every 10M ADA growth prob of retirement -21%
# strong significance p<0.001, not great fit (lots of unexplained var)
model <- glm(retired ~ max_stake, data = pool_summary, family = binomial)
summary(model)

# ROC plot shows poor-to-fair fit 
# max stake alone cannot explain retirement in full
roc_result <- roc(pool_summary$retired, predict(model, type = "response"))
plot(roc_result, col = "purple", main = "ROC Curve: Max Stake Predicting Retirement")
auc(roc_result)


# clean up
rm(model, pool_summary, roc_result)

##############################################################
# stake volatility vs stake size
##############################################################

volatility_df <- stakeData %>%
  filter(epoch_no > 257) %>%
  group_by(pool_id_bech32) %>%
  summarise(
    max_stake = max(max_stake, na.rm = TRUE),
    total_delta = sum(delta_stake, na.rm = TRUE),
    retired = any(pool_status == "retired", na.rm = TRUE)
  ) %>%
  filter(max_stake <= 8e7) %>%
  mutate(retired = factor(retired, labels = c("Active", "Retired")))

# Convert to factor for nicer labels in the legend
volatility_df$retired <- factor(volatility_df$retired, labels = c("Active", "Retired"))

# Plot
vol_plot <- ggplot(volatility_df, aes(x = max_stake, y = total_delta, color = retired)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "white") +
  scale_color_manual(values = c("Active" = "grey80", "Retired" = "magenta")) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  dark_theme +
  labs(
    title = "Net Stake Change vs Maximum Stake (Post-Epoch 257)",
    x = "Maximum Stake (ADA)",
    y = "Total Stake Change (ADA)",
    color = "Pool Status"
  )

# clean up
rm(volatility_df)

##############################################################
# concentration of stake into big pools
##############################################################

stake_concentration <- stakeData %>%
  filter(epoch_no >= 257) %>%
  filter(is.na(pool_status) | pool_status != "retired") %>%      # Exclude retired pools
  group_by(pool_id_bech32) %>%
  summarise(max_stake = min(max(max_stake, na.rm = TRUE), 8e7)) %>%  # Cap at 80M ADA
  mutate(
    stake_bin = cut(
      max_stake,
      breaks = c(seq(0, 8e7, by = 1e7), Inf),
      labels = c(paste0(seq(0, 7) * 10, "-", seq(1, 8) * 10, "M"), "80+M"),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  group_by(stake_bin) %>%
  summarise(
    total_stake = sum(max_stake, na.rm = TRUE),
    num_pools = n()
  ) %>%
  mutate(pct_total_stake = total_stake / sum(total_stake))

# Ensure stake_bin is an ordered factor based on actual bin order
stake_concentration$stake_bin <- factor(
  stake_concentration$stake_bin,
  levels = c(paste0(seq(0, 7) * 10, "-", seq(1, 8) * 10, "M"), "80+M")
)

# plot
conc_plot <- ggplot(stake_concentration, aes(x = stake_bin, y = pct_total_stake, fill = pct_total_stake)) +
  geom_col() +
  geom_text(aes(label = paste0(num_pools, " pools")), vjust = -0.5, color = "white", size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_gradient(low = "#330033", high = "#FF00FF", name = "Percent of Total Stake") +
  labs(
    title = "Stake Concentration by Pool Size (Excluding Retired Pools, Capped at 80M ADA)",
    x = "Max Pool Stake Bin (ADA)",
    y = "Percent of Total Stake"
  ) +
  dark_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# clean up
rm(stake_concentration)

##############################################################
# cumulative % of pools vs stake
##############################################################

# Cap at 80M and filter non-retired
stake_concentration <- stakeData %>%
  filter(epoch_no >= 257) %>%
  filter(is.na(pool_status) | pool_status != "retired") %>%
  group_by(pool_id_bech32) %>%
  summarise(max_stake = max(max_stake, na.rm = TRUE)) %>%
  mutate(
    capped_stake = ifelse(max_stake > 8e7, 8e7 + 1, max_stake),  # ensure 80M+ fits
    stake_bin = cut(
      capped_stake,
      breaks = c(seq(0, 8e7, by = 1e7), Inf),  # now 10 breakpoints → 9 bins
      labels = c("0-10M", "10-20M", "20-30M", "30-40M", "40-50M", "50-60M", "60-70M", "70-80M", "80M+"),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  group_by(stake_bin) %>%
  summarise(
    num_pools = n(),
    total_stake = sum(max_stake, na.rm = TRUE)
  ) %>%
  mutate(
    pct_total_stake = total_stake / sum(total_stake)
  )

# Prepare Pareto curve plot data: Summarise to get one row per pool
pareto_data <- stakeData %>%
  filter(!is.na(max_stake), max_stake > 0, epoch_no > 257, max_stake <= 80e6) %>%
  group_by(pool_id_bech32) %>%
  summarise(max_stake = max(max_stake, na.rm = TRUE)) %>%
  arrange(desc(max_stake)) %>%
  mutate(
    cum_pools = row_number(),
    cum_stake = cumsum(max_stake),
    pct_pools = cum_pools / n(),
    pct_stake = cum_stake / sum(max_stake)
  )

# Get the threshold pool percentage where cumulative stake hits 50%
threshold <- pareto_data %>%
  filter(pct_stake >= 0.5) %>%
  slice(1)

threshold_pct_pools <- threshold$pct_pools
threshold_pct_stake <- threshold$pct_stake  # should be just over 0.5

# Plot Pareto Curve with intersection lines
pareto_plot <- ggplot(pareto_data, aes(x = pct_pools, y = pct_stake)) +
  #geom_line(color = "#00E5FF", size = 1.4) +
  geom_point(color = "white", size = 1) +
  
  # Vertical segment: from (x, 0) to (x, y)
  geom_segment(aes(x = threshold_pct_pools, xend = threshold_pct_pools,
                   y = 0, yend = threshold_pct_stake),
               linetype = "dashed", color = "magenta", linewidth = 0.8) +
  
  # Horizontal segment: from (0, y) to (x, y)
  geom_segment(aes(x = 0, xend = threshold_pct_pools,
                   y = threshold_pct_stake, yend = threshold_pct_stake),
               linetype = "dashed", color = "magenta", linewidth = 0.8) +
  
  # Annotations
  annotate("text", x = threshold_pct_pools + 0.05, y = 0.02,
           label = paste0(round(threshold_pct_pools * 100, 1), "% of pools"),
           color = "magenta", hjust = 0) +
  annotate("text", x = 0.22, y = threshold_pct_stake,
           label = "50% of total stake", color = "magenta", vjust = 0) +
  
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Cumulative Pool Count vs Stake Share",
    x = "Cumulative % of Pools",
    y = "Cumulative % of Stake"
  ) +
  dark_theme

#clean up
rm(pareto_data, stake_concentration, threshold)

##############################################################
# arrange all plots for publication
##############################################################

# figure 1 - ecosystem wide
plots_01 <- list(totAda_plot, gini_plot)
arrange_plots(plots_01, nrow = 1, ncol = 2)
rm(plots_01, totAda_plot, gini_plot)

# figure 2 - top pools
plots_02 <- list(pareto_plot, fifthPercent_plot, top_plot)
arrange_plots(plots_02, nrow = 1, ncol = 3)
rm(plots_02, pareto_plot, top_plot, fifthPercent_plot)

# figure 2.1 - top 100
plots_02_1 <- list(top100_plot, heatmap_plot)
arrange_plots(plots_02_1, nrow = 1, ncol = 2)
rm(plots_02_1, top100_plot, heatmap_plot)

# figure 3 - volatility and concentration
plots_03 <- list(conc_plot, vol_plot)
arrange_plots(plots_03, nrow = 1, ncol = 2)
rm(plots_03, conc_plot, vol_plot)

# figure 4 - retiring pools
plots_04 <- list(sizeRet_plot, smallRet_plot, retPoolDensity_plot)
arrange_plots(plots_04, nrow = 1, ncol = 3)
rm(plots_04, sizeRet_plot, smallRet_plot, retPoolDensity_plot)

# figure 5 - retirement factors
plots_05 <- list(dropRet_plot, timeRet_plot, rateRet_plot)
arrange_plots(plots_05, nrow = 1, ncol = 3)
rm(plots_05, dropRet_plot, timeRet_plot, rateRet_plot)





