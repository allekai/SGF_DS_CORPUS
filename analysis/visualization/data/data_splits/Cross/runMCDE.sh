declare -a arr=("HiCS" "TC" "II" "MS" "UDS" "MAC" "CMI")

echo "MVP" >> MWP_out.txt
for measure in "${arr[@]}"
do 
    echo $measure >> ${measure}_out.txt
done

for file in Data*
do
    java -jar ~/Documents/GitHub/mcde/target/scala-2.11/MCDE-1.0.jar -t EstimateDependency -f $file -a MWP -m 500 -d 0,1 | tail -4 | head -1 >> MWP_out.txt
    for measure in "${arr[@]}"
    do 
        java -jar ~/Documents/GitHub/mcde/target/scala-2.11/MCDE-1.0.jar -t EstimateDependency -f $file -a $measure -d 0,1 | tail -4 | head -1 >> ${measure}_out.txt
    done
done

paste -d "," *_out.txt >> cross_mcde.csv
rm *_out.txt *.log
