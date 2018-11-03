rm *_out.txt *.log
declare -a arr=("HiCS" "TC" "II" "MS" "UDS" "MAC" "CMI")

for file in ./Data*
do
    java -jar ~/Documents/GitHub/mcde/target/scala-2.11/MCDE-1.0.jar -t EstimateDependency -f $file -a MWP -m 500 -d 0,1 | grep ^0.[0-9]* >> MWP_out.txt
    for measure in "${arr[@]}"
    do 
        java -jar ~/Documents/GitHub/mcde/target/scala-2.11/MCDE-1.0.jar -t EstimateDependency -f $file -a $measure -d 0,1 | grep ^0.[0-9]* >> ${measure}_out.txt
    done
done
