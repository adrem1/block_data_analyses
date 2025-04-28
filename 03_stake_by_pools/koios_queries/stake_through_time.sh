#!/bin/bash

# Configurations
POOL_CSV="pool_tickers.csv"  # Input CSV with pool IDs
START_EPOCH=209
END_EPOCH=552
EPOCH_STEP=10  # Create a new file every 10 epochs
API_URL="https://api.koios.rest/api/v1/pool_history"

# Extract pool IDs from CSV, skipping the header
POOL_IDS=($(tail -n +2 "$POOL_CSV" | cut -d ',' -f1 | grep -v '^$')) # Remove empty lines

# Loop in 50-epoch chunks
for ((EPOCH_RANGE_START=START_EPOCH; EPOCH_RANGE_START<=END_EPOCH; EPOCH_RANGE_START+=EPOCH_STEP)); do
    EPOCH_RANGE_END=$((EPOCH_RANGE_START + EPOCH_STEP - 1))
    if [[ $EPOCH_RANGE_END -gt $END_EPOCH ]]; then
        EPOCH_RANGE_END=$END_EPOCH
    fi

    OUTPUT_FILE="pool_stake_history_${EPOCH_RANGE_START}-${EPOCH_RANGE_END}.csv"
    echo "Creating new file: $OUTPUT_FILE"

    # Initialize new CSV file with a header
    echo "epoch_no,pool_id_bech32,active_stake" > "$OUTPUT_FILE"

    # Loop through each pool
    for POOL_ID in "${POOL_IDS[@]}"; do
        echo "Fetching stake data for pool: $POOL_ID"

        # Loop through each epoch in this range
        for ((EPOCH=$EPOCH_RANGE_START; EPOCH<=$EPOCH_RANGE_END; EPOCH++)); do
            echo "  - Epoch $EPOCH"

            # Fetch stake data from Koios API
            RESPONSE=$(curl -s -X GET "$API_URL?_pool_bech32=$POOL_ID&_epoch_no=$EPOCH&select=active_stake" -H "accept: text/csv")

            # Remove header if present and append data
	    echo "$RESPONSE" | tail -n +2 | awk -v epoch="$EPOCH" -v pool="$POOL_ID" '{print epoch "," pool "," $0}' >> "$OUTPUT_FILE"
        done
    done

    echo "Finished processing epochs ${EPOCH_RANGE_START}-${EPOCH_RANGE_END}. Data saved to $OUTPUT_FILE."
done

echo "All epochs processed successfully!"
