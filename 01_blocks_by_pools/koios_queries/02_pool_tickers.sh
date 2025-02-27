#!/bin/bash

# Output CSV file
OUTPUT_FILE="pool_tickers.csv"

# API URL
API_URL="https://api.koios.rest/api/v1/pool_list?select=pool_id_bech32,ticker"

# Ensure the output file is empty before starting
> "$OUTPUT_FILE"

HEADER_SAVED=false  # Flag to track if the header has been saved
offset=0

while true; do
  echo "Fetching pool tickers with offset $offset..."

  # Fetch data and store temporarily
  RESPONSE=$(curl -s -X GET "$API_URL&offset=$offset" -H "accept: text/csv")

  # If this is the first request and header hasn't been saved, write everything
  if [[ "$HEADER_SAVED" = false ]]; then
    echo "$RESPONSE" >> "$OUTPUT_FILE"
    HEADER_SAVED=true
  else
    # Remove the first line (header) and append the rest
    echo "$RESPONSE" | tail -n +2 >> "$OUTPUT_FILE"
  fi

  # Count number of lines (subtract 1 if API includes a header row)
  NUM_LINES=$(echo "$RESPONSE" | wc -l)

  # If fewer than 1000 lines, we've reached the last batch of results
  if [[ $NUM_LINES -lt 1000 ]]; then
    break
  fi

  # Increase offset
  offset=$((offset + 1000))

  # Small delay to prevent overwhelming the server
  sleep 1
done

echo "Pool ticker fetching complete. Saved to $OUTPUT_FILE."
