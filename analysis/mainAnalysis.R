# main analysis file

list.of.packages  <- c("batchtools",
                       "dbscan",
                       "parallel",
                       "Rlof",
              "stream",
              "streamgenerator",
              "tictoc",            
              "zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

registry.path <- "analysis/registry"
cores <- detectCores()
if(!dir.exists(registry.path)) {
  reg <- makeExperimentRegistry(file.dir=registry.path, make.default = TRUE)
  reg$cluster.functions <- makeClusterFunctionsSocket(ncpus = cores-1)
} else {
  reg <- loadRegistry(registry.path, writeable = TRUE)
  reg$cluster.functions <- makeClusterFunctionsSocket(ncpus = cores-1)
}


#data.stream.static.list <- c("Sta10N5000Lin2Mar01NumSub2Pro05Con",
#                             "Sta10N5000Wal2Mar01NumSub2Pro05Con",
#                             "Sta10N5000Squ2Mar01NumSub2Pro05Con",
#                             "Sta10N5000Don2Mar01NumSub2Pro05Con",
#                             "Sta10N5000Cro2Mar01NumSub2Pro05Con",
#                             "Sta10N5000Hou2Mar01NumSub2Pro05Con",
#                             "Sta10N5000Sin2Mar01NumSub2Pro05Con")

tmp <- list.files("selected_data/", pattern = "data.txt$", recursive = T, full.names = T)
static.streams <- lapply(tmp, read.csv)  # outlier label as "class"


for (i in 1:length(static.streams)) {
  'static.streams[[i]] <- data.frame(static.streams[[i]],
                                    index = 1:nrow(static.streams[[i]]))'
  addProblem(name=substr(tmp[i], 15, 51), data = static.streams[i], reg = reg)
}

wrapper.LOF <- function(data, job=NULL, k=10, ...) {

}
addAlgorithm("LOF", fun = wrapper.LOF, reg = reg)

ades <- list(LOF = data.table(k = c(5,10)))
addExperiments(algo.designs = ades, repls = 1)
summarizeExperiments()

testJob(1)


stream.conf <- streamgenerator::generate.stream.config(dim=10,
                                                       dependency = "Linear")
stream.conf$dim <- 2
stream.conf$subspaceslist <- list(1:2)
stream.conf$marginslist <- c(0.9)
stream.conf$nstep <- 1

stream <- streamgenerator::generate.static.stream(n=10000, method="Construction",
                                                  stream.config = stream.conf)
write.arff(stream$data, "stream.arff")

