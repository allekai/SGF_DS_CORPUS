# main file
library(batchtools)
library(snow)
library(data.table)
source("generation/setup.R")

loginfo("Loading or creating registry for batchtools.")
if(!dir.exists("registry")) {
  reg <- makeRegistry(file.dir="registry", make.default = TRUE)
  reg$cluster.functions <- makeClusterFunctionsSocket()
} else {
  reg <- loadRegistry("registry", writeable = TRUE)
  reg$cluster.functions <- makeClusterFunctionsSocket()
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
  subspaceslist <- list(c(subspaceslist))
  marginslist <- list(c(marginslist))
  conf <- streamgenerator::generate.stream.config(dim = dim, nstep=1)
  conf$dependency <- dependency
  conf$subspaces <- subspaceslist
  conf$margins <- marginslist
  j <- 1
  while(j < 6) {
    stream <- streamgenerator::generate.static.stream(n = n,
                                                      prop = prop,
                                                      proptype = proptype,
                                                      stream.config = conf, 
                                                      verbose = verbose,
                                                      method = method)
    
    # prefix alá Sta20Lin2Mar01NumSub1Pro05_V1
    prefix <- sprintf("Sta%i%s%iMar%02.0fNumSub%i%s%02.0f_V%i",
                      dim,
                      substring(dependency, 1, 3),
                      length(subspaceslist[[1]]),
                      (1 - marginslist[[1]])*10,
                      length(subspaceslist),
                      paste(toupper(substring(proptype, 1,1)),
                            substring(proptype, 2, 3), sep="", collapse = " "),
                      prop * 100,
                      j)
    dir.create(paste("data/", prefix, sep="", collapse = " "), showWarnings = TRUE)
    setwd(paste("data/", prefix, sep="", collapse = " "))
    loginfo(paste(c("Saving stream: ", prefix)))
    streamgenerator::output.stream(stream, prefix)
    setwd("../..")
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
    stream <- streamgenerator::generate.dynamic.stream(n = n,
                                                      prop = prop,
                                                      proptype = proptype,
                                                      stream.config = conf, 
                                                      verbose = verbose,
                                                      method = method)
    
    # prefix alá Dyn20Lin2Mar01NumSub1Pro05Vol05Cyc0_V1
    prefix <- sprintf("Dyn%i%s%iMar%02.0fNumSub%i%s%02.0fVol%02.0fCyc%i_V%i",
                      dim,
                      substring(dependency, 1, 3),
                      length(subspaceslist[[1]]),
                      (1 - marginslist[[1]])*10,
                      length(subspaceslist),
                      paste(toupper(substring(proptype, 1,1)),
                            substring(proptype, 2, 3), sep="", collapse = " "),
                      prop * 100,
                      volatility * 100,
                      cycle,
                      i)
    dir.create(paste("data/", prefix, sep="", collapse = " "), showWarnings = TRUE)
    setwd(paste("data/", prefix, sep="", collapse = " "))
    loginfo(paste(c("Saving stream: ", prefix)))
    streamgenerator::output.stream(stream, prefix)
    setwd("../..")
    j <- j + 1
  }
}

loginfo("Create parameters for static data streams.")
staticParams <- data.frame()
dependencyList <- c("Linear",
                    "Wall",
                    "Suqare",
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

