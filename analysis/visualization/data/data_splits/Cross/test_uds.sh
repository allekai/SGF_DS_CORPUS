for file in ./Data*
do
    java -jar ~/Documents/GitHub/mcde/target/scala-2.11/MCDE-1.0.jar -t EstimateDependency -f $file -a UDS -d 0,1  | tail -4 | head -1 >> test_out.txt
done
