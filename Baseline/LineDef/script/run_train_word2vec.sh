#!/bin/bash

datasets=("activemq" "camel" "derby" "groovy" "hbase" "jruby" "lucene" "wicket" "hive")

for dataset in "${datasets[@]}"; do
    python train_word2vec.py "$dataset" 
done

echo "done！"