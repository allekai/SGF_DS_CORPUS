# main file
library(batchtools)
library(data.table)
source("setup.R")


tmp = makeRegistry(file.dir = NA, make.default = FALSE)

fun <- function(dim,
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
  stream <- streamgenerator::generate.static.stream(n = n,
                                                    prop = prop,
                                                    proptype = proptype,
                                                    stream.config = conf, 
                                                    verbose = verbose,
                                                    method = method)
}

params <- data.frame()
dependencyList <- c("Linear",
                    "Wall",
                    "Suqare",
                    "Donut",
                    "Cross",
                    "Hourglass",
                    "Sine")
for(i in dependencyList) {
  dependency <- i
  result <- source("grid_source.R")
  params <- rbind(params, result$value)
}


ids = batchMap(fun, args = params, reg = tmp)
unwrap(getJobTable(reg=tmp))
submitJobs(reg=tmp)

