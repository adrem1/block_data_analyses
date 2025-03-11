#!/bin/bash

# Input file containing pools without tickers (one per line)
INPUT_FILE="pools_no_ticker.txt"
# Output CSV file
OUTPUT_FILE="pool_info.csv"

# Check if output file exists; if not, add headers
if [ ! -f "$OUTPUT_FILE" ]; then
    echo "pool_id_bech32,meta_json,pool_status,retiring_epoch,block_count,live_pledge,live_stake,live_delegators" > "$OUTPUT_FILE"
fi

# Loop through each pool in the file
while IFS= read -r POOL_ID; do
    echo "Fetching info for: $POOL_ID"
    
    # Query Koios API
    RESPONSE=$(curl -s -X POST "https://api.koios.rest/api/v1/pool_info" \
        -H "accept: application/json" \
        -H "content-type: application/json" \
        -d "{\"_pool_bech32_ids\":[\"$POOL_ID\"]}")

    # Extract values from JSON response using jq
    echo "$RESPONSE" | jq -r --arg pool_id "$POOL_ID" '
        .[] | [
            $pool_id,  
            (.meta_json | tostring), 
            .pool_status, 
            .retiring_epoch,  
            .block_count, 
            .live_pledge, 
            .live_stake, 
            .live_delegators, 
            .live_saturation
        ] | @csv' >> "$OUTPUT_FILE"

    sleep 1  # Add delay to avoid API rate limits
done < "$INPUT_FILE"

echo "Data fetching complete. Saved to $OUTPUT_FILE."


