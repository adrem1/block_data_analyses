#!/bin/bash

# Output CSV file
OUTPUT_FILE="epoch_data.csv"

# API URL
API_URL="https://api.koios.rest/api/v1/blocks"

AUTH="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhZGRyIjoic3Rha2UxdTg3ejd6YzhzanIzcmc0bndwOXFxdmt1amV5MHh1NXkzaHM3dDJwZW54N25qOGc0enVuZTAiLCJleHAiOjE3NzE5Mzk3MDAsInRpZXIiOjEsInByb2pJRCI6IkJqWWpqRGo3NFAwUmRPWlcifQ.e-xGqcd3QLGXR03mDzaa6ou2oqxYszg2OYUtHVgytDA"

# Ensure the output file is empty before starting
> "$OUTPUT_FILE"

HEADER_SAVED=false  # Flag to track if the header has been saved

# Loop through epochs 535 to 540
for epoch in {209..299}; do
  offset=0

  while true; do
    echo "Fetching data for epoch $epoch with offset $offset..."

    # Fetch data and store temporarily
    RESPONSE=$(curl -s -X GET "$API_URL?select=epoch_no,pool&epoch_no=eq.$epoch&offset=$offset" -H "accept: text/csv" -H "authorization: Bearer $AUTH")

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

    # If fewer than 1000 lines, we've reached the last block of the epoch
    if [[ $NUM_LINES -lt 1000 ]]; then
      break
    fi

    # Increase offset
    offset=$((offset + 1000))

    # Small delay to prevent overwhelming the server
    sleep 1
  done
done

echo "Data fetching complete. Saved to $OUTPUT_FILE."

