# main file
# Setup
library(batchtools)
library(snow)
library(data.table)
library(devtools)
library(logging)
library(parallel)

basicConfig(level = 'FINEST')
addHandler(writeToFile, file="generation.log", level='DEBUG')


path="C:\\Users\\user\\Documents\\R-streamgenerator"

loginfo(paste(c("Trying to install the streamgenerator package from path: ", path)))
install_local(path, force=TRUE)
loginfo("Successfully installed the streamgenerator package.")


loginfo("Loading or creating registry for batchtools.")
cores <- detectCores()

if(!dir.exists("registry")) {
  reg <- makeRegistry(file.dir="registry", make.default = TRUE)
  reg$cluster.functions <- makeClusterFunctionsSocket(ncpus = cores-1)
} else {
  reg <- loadRegistry("registry", writeable = TRUE)
  reg$cluster.functions <- makeClusterFunctionsSocket(ncpus = cores-1)
}
loginfo("Got registry.")

staticFun <- function(dim,
                dependency,
                subspaceslist,
                marginslist,
                n,
                prop,
                proptype,
                verbose,
                method) {
  if(dependency == "Sine" && max(subspaceslist) > 3) return(TRUE)
  conf <- streamgenerator::generate.stream.config(dim = dim,
                                                  nstep=1,
                                                  maxdim = max(subspaceslist))
  conf$dependency <- dependency
  conf$subspaces <- list(c(subspaceslist))
  conf$margins <- marginslist
  j <- 1
  while(j < 6) {
    # prefix alá Sta20N5000Lin2Mar01NumSub1Pro05Con_V1
    prefix <- sprintf("Sta%iN%i%s%iMar%02.0fNumSub%i%s%02.0f%s_V%i",
                      dim,
                      n,
                      substring(dependency, 1, 3),
                      conf$maxdim,
                      (1 - marginslist[[1]])*10,
                      length(subspaceslist),
                      paste(toupper(substring(proptype, 1,1)),
                            substring(proptype, 2, 3), sep="", collapse = " "),
                      prop * 100,
                      substring(method, 1, 3),
                      j)
    print(prefix)
    stream <- streamgenerator::generate.static.stream(n = n,
                                                      prop = prop,
                                                      proptype = proptype,
                                                      stream.config = conf, 
                                                      verbose = verbose,
                                                      method = method)
    
    
    dir.create(paste("data/", prefix, sep="", collapse = " "), showWarnings = TRUE)
    setwd(paste("data/", prefix, sep="", collapse = " "))
    streamgenerator::output.stream(stream, prefix)
    setwd("../..")
    prefix <- ""
    j <- j+1
  }
}

dynamicFun <- function(dim,
                      dependency,
                      subspaceslist,
                      marginslist,
                      n,
                      prop,
                      proptype,
                      verbose,
                      method,
                      volatility,
                      cycle,
                      nstep) {
  if(dependency == "Sine" && max(subspaceslist) > 3) return(TRUE)
  j <- 1
  while(j < 6) {
    
    conf <- streamgenerator::generate.stream.config(dim = dim,
                                                    mindim = 2,
                                                    maxdim = 5,
                                                    nstep = nstep, 
                                                    cycle = cycle,
                                                    volatility = volatility,
                                                    values = c(0.1, 0.5, 0.9))
    conf$dependency <- dependency
    # prefix alá Dyn20N5000Lin2Mar01NumSub1Pro05Vol05Cyc0Nstep5Con_V1
    prefix <- sprintf("Dyn%iN%i%s%iMar%02.0fNumSub%i%s%02.0fVol%02.0fCyc%iNstep%i%s_V%i",
                      dim,
                      n,
                      substring(dependency, 1, 3),
                      conf$maxdim,
                      (1 - max(unlist(conf$margins)))*10,
                      length(conf$subspaces),
                      paste(toupper(substring(proptype, 1,1)),
                            substring(proptype, 2, 3), sep="", collapse = " "),
                      prop * 100,
                      volatility * 100,
                      cycle,
                      nstep,
                      substring(method, 1, 3),
                      j)
    print(prefix)
    stream <- streamgenerator::generate.dynamic.stream(n = n,
                                                      prop = prop,
                                                      proptype = proptype,
                                                      stream.config = conf, 
                                                      verbose = verbose,
                                                      method = method)
    
   
    dir.create(paste("data/", prefix, sep="", collapse = " "), showWarnings = TRUE)
    setwd(paste("data/", prefix, sep="", collapse = " "))
    streamgenerator::output.stream(stream, prefix)
    setwd("../..")
    prefix <- ""
    j <- j + 1
  }
}



loginfo("Create parameters for static data streams.")
staticParams <- data.frame()
dependencyList <- c("Linear",
                    "Wall",
                    "Square",
                    "Donut",
                    "Cross",
                    "Hourglass",
                    "Sine")
for(i in dependencyList) {
  dependency <- i
  result <- source("generation/staticStreamConfigs.R")
  staticParams <- rbind(staticParams, result$value)
}
loginfo("Created static parameters.")


loginfo("Clearing registry from provious jobs.")
clearRegistry()
loginfo("Creating jobs for static streams.")
staticIds <- batchMap(staticFun, args = staticParams, reg = reg)
loginfo("Submitting jobs for static streams.")
submitJobs(reg=reg)
waitForJobs(ids = staticIds)
loginfo("Done with jobs for static streams.")



loginfo("Creating parameter for dynamic streams.")
dynamicParams <- data.frame()
for(i in dependencyList) {
  dependency <- i
  result <- source("generation/dynamicStreamConfigs.R")
  dynamicParams <- rbind(dynamicParams, result$value)
}
loginfo("Created dynamic parameters.")
loginfo("Clearing registry from provious jobs.")
clearRegistry()
loginfo("Creating jobs for dynamic streams.")
dynamicIds <- batchMap(dynamicFun, args = dynamicParams, reg = reg) 
loginfo("Submitting jobs for dynamic streams.")
submitJobs(reg=reg)
waitForJobs(ids = dynamicIds)
loginfo("Done with jobs for dynamic streams.")

loginfo("Done with the data stream generation.")

