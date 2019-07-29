#!/bin/bash

for sequenced_file in `ls | grep -e "Sequenced*"`
do
    bartender_single_com -f ${sequenced_file} -o ./cluster_result_${sequenced_file} -c 2 -l 5 -s 1 -z 5 -t 8 -d 3 --forward
done


