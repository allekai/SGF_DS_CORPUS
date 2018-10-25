# Hacky version

list.of.packages  <- c("batchtools",
                       "dbscan",
                       "ggplot2",
                       "microbenchmark",
                       "parallel",
                       "reshape2",
                       "Rlof",
                       "stream",
                       "streamgenerator",
                       "tictoc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


# make a test run
#source("analysis/testAnalysis.R")

# setup experiment
# first we test the influence of the number of points on the generation process
# static streams -- Linear 3D

experiment.static <- function(dep) {
  list.of.packages  <- c("batchtools",
                         "dbscan",
                         "ggplot2",
                         "microbenchmark",
                         "parallel",
                         "reshape2",
                         "Rlof",
                         "stream",
                         "streamgenerator",
                         "tictoc")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only=T)
  getTimings.n <- function(n.static.conf,
                           list.n = list(1000,
                                         2000),
                           num.reps = 5) {
    x <- 1
    static.timings.list <- list()
    names <- list()
    for (i in 1:length(list.n)) {
      for (j in 1:num.reps){
        tic(x)
        invisible(streamgenerator::generate.static.stream(n = list.n[[i]],
                                                          stream.config = n.static.conf,
                                                          method = "Construction"))
        toc(log = TRUE)
        x <- x + 1
      }
      n.static.log.raw <- tic.log(format = F)
      tic.clearlog()
      static.timings.list[[i]] <- unlist(lapply(n.static.log.raw, function(x) x$toc - x$tic))
      names[[i]] <- paste("n=", list.n[[i]], sep = "")
    }
    df <- data.frame(static.timings.list)
    names(df) <- names
    df
  }
  n.static.conf <- streamgenerator::generate.stream.config(dim = 10,
                                                           dependency = dep,
                                                           nstep = 1)
  n.static.conf$subspaces <- list(1:3)
  n.static.conf$margins <- 0.9
  static.times <- getTimings.n(n.static.conf = n.static.conf,
                               list.n = list(1000,2000,3000,4000,5000,
                                             6000,7000,8000,9000,10000), num.reps = 5)
  static.means <- lapply(static.times, mean)
  pdf(paste("analysis/Static", dep, "Times.pdf", sep="", collapse = " "))
  plot(unlist(static.means), type="l",
       xlab="n * 1000", ylab="time in seconds",
       main = paste("Static", dep, "stream generation time with varying n"))
  dev.off()
  list(static.means, static.times)
}

dependency.list <- c("Linear",
                     "Wall",
                     "Square",
                     "Donut",
                     "Cross",
                     "Hourglass",
                     "Sine")
cl <- makeCluster(detectCores()-1)
result.list <- parLapply(cl, dependency.list, experiment.static)
stopCluster(cl)