#!/bin/bash
cd "$(dirname "$0")"
source /root/miniconda3/etc/profile.d/conda.sh
conda activate pytorch18

for p in activemq camel derby groovy hbase hive jruby lucene wicket; do
  echo "==================== $p  REF checkpoint  ===================="
  python generate_prediction.py -dataset $p 2>&1 | grep -E "finished release|error|Error|Traceback" | head -10
  echo ""
done
echo "=== ALL DONE ==="
