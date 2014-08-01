#!/bin/bash

#################################################
#
#  Download latest FxOS launch info from Mana.
#  Will prompt for password
#  Sanitize and format as CSV. 
# 
#################################################



BASE_URL="https://mana.mozilla.org/wiki/plugins/viewsource/"
BASE_URL+="viewpagesrc.action?pageId"
HISTORY_URL="$BASE_URL=38537642"
FUTURE_URL="$BASE_URL=33100821"

USER="dzeber"
USER+="@mozilla.com"
OUTPUT_DIR=~/fxos/launchstats
OUT_HISTORY=$OUTPUT_DIR/lh.html
OUT_FUTURE=$OUTPUT_DIR/lr.html

# Download from pages.
# Pass URL and output file. 
function download_table {
    wget -w 1 --secure-protocol=auto --user=$USER --ask-password -O $2 $1
}

echo "Processing FxOS launch stats tables."
echo

echo "Downloading history table:"
download_table $HISTORY_URL $OUT_HISTORY
echo "Downloading future table:"
download_table $FUTURE_URL $OUT_FUTURE

echo "Download done."
echo "Processing tables."

R CMD BATCH --no-restore --no-save launchstats.R

echo "Converted to CSV."

    
    


