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

experiment.static <- function(dep, variable) {
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
  
  getTimings.subs <- function(n.static.conf,
                           list.s = list(1:2,
                                         1:3,
                                         1:4,
                                         1:5),
                           num.reps = 5) {
    x <- 1
    static.timings.list <- list()
    names <- list()
    for (i in 1:length(list.s)) {
      for (j in 1:num.reps){
        n.static.conf$subspaces <- list.s[i] 
        tic(x)
        invisible(streamgenerator::generate.static.stream(n = 2500,
                                                          stream.config = n.static.conf,
                                                          method = "Construction"))
        toc(log = TRUE)
        x <- x + 1
      }
      n.static.log.raw <- tic.log(format = F)
      tic.clearlog()
      static.timings.list[[i]] <- unlist(lapply(n.static.log.raw, function(x) x$toc - x$tic))
      names[[i]] <- paste("Dimension=", list.s[[i]], sep = "")
    }
    df <- data.frame(static.timings.list)
    names(df) <- names
    df
  }
  
  getTimings.mar <- function(n.static.conf,
                              list.m = list(0.1, 0.9),
                              num.reps = 5) {
    x <- 1
    static.timings.list <- list()
    names <- list()
    for (i in 1:length(list.m)) {
      for (j in 1:num.reps){
        n.static.conf$margins <- list.m[[i]] 
        tic(x)
        invisible(streamgenerator::generate.static.stream(n = 2500,
                                                          stream.config = n.static.conf,
                                                          method = "Construction"))
        toc(log = TRUE)
        x <- x + 1
      }
      n.static.log.raw <- tic.log(format = F)
      tic.clearlog()
      static.timings.list[[i]] <- unlist(lapply(n.static.log.raw, function(x) x$toc - x$tic))
      names[[i]] <- paste("Margin=", list.m[[i]], sep = "")
    }
    df <- data.frame(static.timings.list)
    names(df) <- names
    df
  }
  
  getTimings.outlierVarlue <- function(n.static.conf,
                             list.ov = list(0.01, 0.1),
                             num.reps = 5) {
    x <- 1
    static.timings.list <- list()
    names <- list()
    for (i in 1:length(list.ov)) {
      for (j in 1:num.reps){
        tic(x)
        invisible(streamgenerator::generate.static.stream(n = 2500,
                                                          stream.config = n.static.conf,
                                                          prop = list.ov[[i]],
                                                          method = "Construction"))
        toc(log = TRUE)
        x <- x + 1
      }
      n.static.log.raw <- tic.log(format = F)
      tic.clearlog()
      static.timings.list[[i]] <- unlist(lapply(n.static.log.raw, function(x) x$toc - x$tic))
      names[[i]] <- paste("Outlier Proportion=", list.ov[[i]], sep = "")
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
  num.reps <- 5
  if (variable == "n") {
    list <- as.list(seq(1000, 10000, 1000))
    static.times <- getTimings.n(n.static.conf = n.static.conf, list.n = list, num.reps = num.reps)
  } else if (variable == "subspace") {
    list <- list(1:2, 1:3, 1:4, 1:5, 1:6, 1:7)
    if(dep == "Sine") return(TRUE)
    static.times <- getTimings.subs(n.static.conf = n.static.conf, list.s = list, num.reps = num.reps)
  } else if (variable == "margin") {
    list <- list(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    static.times <- getTimings.mar(n.static.conf = n.static.conf, list.m = list, num.reps = num.reps)
  } else if (variable == "outlierValue") {
    list <- as.list(seq(0.01, 0.2, by=0.01))
    static.times <- getTimings.outlierVarlue(n.static.conf = n.static.conf, list.ov = list, num.reps = num.reps)
  } else {
    stop("wrong variable")
  }

  static.means <- lapply(static.times, mean)
#  pdf(paste("analysis/plots/Static", dep, "GenerationTimes.pdf", sep="", collapse = " "))
#  plot(unlist(static.means), type="l",
#       xlab="n * 1000", ylab="time in seconds",
#       main = paste("Static", dep, "stream generation time with varying n"))
#  dev.off()
  mean.time.df <- data.frame(Dependency = dep,
                             value = t(as.vector(unlist(static.means))),
                             row.names = NULL,
                             stringsAsFactors = FALSE)
  names(mean.time.df) <- c(names(mean.time.df)[1], list)
  list(mean.time.df, paste(dep, "times", static.times))
}

dependency.list <- c("Linear",
                     "Wall",
                     "Square",
                     "Donut",
                     "Cross",
                     "Hourglass",
                     "Sine")

if(!dir.exists("analysis/plots")) dir.create("analysis/plots")




##### Variable n
cl <- makeCluster(detectCores()-1)
result.list.n <- parLapply(cl, dependency.list, experiment.static, variable="n")
stopCluster(cl)

time.frame.n <- do.call(rbind.data.frame, lapply(result.list.n, function(x) x[[1]]))
time.frame.n.melt <- melt(time.frame.n, id.vars = "Dependency")

static.n.plot <- ggplot(data = time.frame.n.melt, mapping = aes(x = variable, y = value, group = Dependency, color = Dependency)) +
  geom_line(size=0.8) +
  xlab("n") +
  ylab("Time in seconds")
ggsave(filename = "StaticGenerationTimesN.pdf", plot = static.n.plot, device = "pdf", path = "analysis/plots/", dpi = 300) 



##### Variable subspaces
dependency.list <- c("Linear",
                     "Wall",
                     "Square",
                     "Donut",
                     "Cross",
                     "Hourglass")
cl <- makeCluster(detectCores()-1)
result.list.s <- parLapply(cl, dependency.list, experiment.static, variable="subspace")
stopCluster(cl)

time.frame.s <- do.call(rbind.data.frame, lapply(result.list.s, function(x) x[[1]]))
#names(time.frame) <- c(names(time.frame)[1], seq(1000, (ncol(time.frame) - 1)*1000, by = 1000))
time.frame.s.melt <- melt(time.frame.s, id.vars = "Dependency")

static.s.plot <- ggplot(data = time.frame.s.melt, mapping = aes(x = variable, y = value, group = Dependency,color = Dependency)) +
  geom_line(size=0.8) +
  xlab("Number of subspace dimensions") +
  ylab("Time in seconds")
ggsave(filename = "StaticGenerationTimesSubspace.pdf", plot = static.s.plot, device = "pdf", path = "analysis/plots/", dpi = 300) 
dependency.list <- c("Linear",
                     "Wall",
                     "Square",
                     "Donut",
                     "Cross",
                     "Hourglass",
                     "Sine")



##### Variable margin
cl <- makeCluster(detectCores()-1)
result.list.m <- parLapply(cl, dependency.list, experiment.static, variable="margin")
stopCluster(cl)

time.frame.m <- do.call(rbind.data.frame, lapply(result.list.m, function(x) x[[1]]))
time.frame.m.melt <- melt(time.frame.m, id.vars = "Dependency")

static.m.plot <- ggplot(data = time.frame.m.melt, mapping = aes(x = variable, y = value, group = Dependency,color = Dependency)) +
  geom_line(size=0.8) +
  xlab("Margin") +
  ylab("Time in seconds")
ggsave(filename = "StaticGenerationTimesMargin.pdf", plot = static.m.plot, device = "pdf", path = "analysis/plots/", dpi = 300) 


##### Variable outlier value
cl <- makeCluster(detectCores()-1)
result.list.ov <- parLapply(cl, dependency.list, experiment.static, variable="outlierValue")
stopCluster(cl)

time.frame.ov <- do.call(rbind.data.frame, lapply(result.list.ov, function(x) x[[1]]))
time.frame.ov.melt <- melt(time.frame.ov, id.vars = "Dependency")

static.ov.plot <- ggplot(data = time.frame.ov.melt, mapping = aes(x = variable, y = value, group = Dependency,color = Dependency)) +
  geom_line(size=0.8) +
  xlab("Proportion of outlier") +
  ylab("Time in seconds")
ggsave(filename = "StaticGenerationTimesOV.pdf", plot = static.ov.plot, device = "pdf", path = "analysis/plots/", dpi = 300) 

