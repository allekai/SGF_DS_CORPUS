#!/bin/bash

if [ ! -d "data_splits" ]; then
    mkdir data_splits
fi

for file in *_data.txt
do
    mkdir data_splits/"${file:0:3}"
    cat $file | head -1 >> heading.txt
    cp heading.txt data_splits/${file:0:3}/
    sed -i '1d' $file 
    split -l 100 -d $file Data

    declare -a arr=("HiCS" "TC" "II" "MS" "UDS" "MAC" "CMI")


    echo "MVP" >> MWP_out.txt
    for measure in "${arr[@]}"
    do 
        echo $measure >> ${measure}_out.txt
    done

    for dat in Data*
    do
        java -jar ~/Documents/GitHub/mcde/target/scala-2.11/MCDE-1.0.jar -t EstimateDependency -f $dat -a MWP -m 500 -d 0,1 | tail -4 | head -1 >> MWP_out.txt
        for measure in "${arr[@]}"
        do 
            java -jar ~/Documents/GitHub/mcde/target/scala-2.11/MCDE-1.0.jar -t EstimateDependency -f $dat -a $measure -d 0,1 | tail -4 | head -1 >> ${measure}_out.txt
        done
    done

    paste -d "," *_out.txt >> ${file:0:3}_mcde.csv
    rm *_out.txt *.log

    mv Data* data_splits/${file:0:3}/
    for dat in ./data_splits/${file:0:3}/Data*
    do
        cat data_splits/${file:0:3}/heading.txt | cat - $dat > temp && mv temp $dat
    done
    cat heading.txt | cat - $file > temp && mv temp $file
    rm heading.txt
done
