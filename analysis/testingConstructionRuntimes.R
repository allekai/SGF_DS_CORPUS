source("analysis/constructionFunctions.R")



#### Time the duration of isInHiddenSpace ####
library(microbenchmark)
library(tictoc)
library(ggplot2)
library(reshape2)


makeBenchmarkPlot.isInHiddenSpace <- function(subspaces = list(1:2),
                                              margins = 0.9,
                                              dependency.list = c("Linear", "Sine"),
                                              times = 100,
                                              unit = "ns"){
  results.mean <- results.mbm <- list()
  

  for (dep in dependency.list) {
    f <- get(paste("isInHiddenSpace.", dep, sep=""))
    for (i in 1:length(subspaces)){
      print(sprintf("Current dep %s with dim %i", dep, max(subspaces[[i]])))
      x <- runif(n = length(subspaces[[i]]))
      if (dep %in% c("Cross", "Hourglass") && i > 5) times <- 5
      results.mbm[[dep]][[i]] <- microbenchmark(f(row = x, subspace = i), unit = unit, times = times)
      results.mean[[dep]][[i]] <- summary(results.mbm[[dep]][[i]])$median
    }
  }
  
  
  df.mean <- as.data.frame(results.mean, stringsAsFactors = F)
  df.mean$Dim <- as.integer(lapply(subspaces,FUN = max))
  
  m <- melt(df.mean, id.vars = "Dim", factorsAsStrings = T)
  i <- sapply(m, is.factor)
  m[i] <- lapply(m[i], as.character)
  #View(m)
  
  ggplot(m, aes(x = Dim, y = value, group=variable, color=variable)) + geom_line(size=0.8) +
    xlab("Subspace dimension") + ylab(paste("Time in", unit)) + ggtitle("Average time of isInHiddenSpace") +
    theme(plot.title = element_text(hjust = 0.5))
}


subspaces <- list(1:2, 1:250, 1:500, 1:750, 1:1000)
margins <- rep(x = 0.9, times = length(subspaces))
dependency.list <- c("Linear", "Wall", "Square", "Donut")
system.time(plot.iihs.quick <- makeBenchmarkPlot.isInHiddenSpace(subspaces = subspaces, margins = margins,
                                                dependency.list = dependency.list, unit = "us",
                                                times = 200))
plot.iihs.quick

subspaces <- list(1:2, 1:3, 1:4, 1:5, 1:6, 1:7,1:8, 1:9, 1:10)#, 1:11, 1:12)# 
margins <- rep(x = 0.9, times = length(subspaces))
dependency.list <- c("Linear", "Wall", "Square", "Donut","Cross", "Hourglass")# 
system.time(plot.iihs.slow <- makeBenchmarkPlot.isInHiddenSpace(subspaces = subspaces, margins = margins,
                                               dependency.list = dependency.list, unit = "s",
                                               times = 30))
plot.iihs.slow

subspaces <- list(1:2, 1:3)#, 1:6, 1:7, 1:8, 1:9, 1:10)
margins <- rep(x = 0.9, times = length(subspaces))
dependency.list <- c("Sine")
system.time(plot.iihs.sine <- makeBenchmarkPlot.isInHiddenSpace(subspaces = subspaces, margins = margins,
                                               dependency.list = dependency.list, unit = "ms",
                                               times = 5))
plot.iihs.sine



#### Time the construction of points ####
makeBenchmarkPlot.constructPoint <- function(n.list = seq(1000, 5000, 1000),
                                              dependency.list = c("Linear", "Sine"),
                                              times = 100,
                                              unit = "ns"){
  results.mean <- results.mbm <- results.mean.rmOutlier <- list()
  
  if (unit == "s") {
    factor <- 1e9
  } else if (unit == "ms") {
    factor <- 1e6
  } else if (unit == "us") {
    factor <- 1e3
  } else {
    factor <- 1
  }
  
  for (dep in dependency.list) {
    f <- get(paste("constructPoint.", dep, sep=""))
    h <- function(sub = 1, n = 10) {
      invisible(replicate(n = n, expr = f(subspace = sub)))
    }
    for (i in 1:length(n.list)){
      results.mbm[[dep]][[i]] <- microbenchmark(h(sub = 1, n = n.list[i]),
                                                unit = unit,
                                                times = times)
      results.mean[[dep]][[i]] <- summary(results.mbm[[dep]][[i]])$median
      x <- results.mbm[[dep]][[i]]$time
      results.mean.rmOutlier[[dep]][[i]] <- mean(x[!x %in% boxplot.stats(x)$out]) / factor
    }
  }

  df.mean <- as.data.frame(results.mean, stringsAsFactors = F)
  df.mean$N <- n.list
  
  m <- melt(df.mean, id.vars = "N", factorsAsStrings = T)
  i <- sapply(m, is.factor)
  m[i] <- lapply(m[i], as.character)
  #m
  
  result.plots <- list()
  result.plots$Raw <- ggplot(m, aes(x = N, y = value, group=variable,
                                    color=variable)) + geom_line(size=0.8) +
    xlab("Number of points generated") + ylab(paste("Time in", unit)) +
    ggtitle("Average time of constructPoint") +
    theme(plot.title = element_text(hjust = 0.5))
    #ylim(0, max(m$value))
  
  df.mean <- as.data.frame(results.mean.rmOutlier, stringsAsFactors = F)
  df.mean$N <- n.list
  
  m <- melt(df.mean, id.vars = "N", factorsAsStrings = T)
  i <- sapply(m, is.factor)
  m[i] <- lapply(m[i], as.character)
  #m
  range <- layer_scales(result.plots$Raw)$y$range$range
  
  result.plots$Cleaned <- ggplot(m, aes(x = N, y = value, group=variable,
                                           color=variable)) + geom_line(size=0.8) +
    xlab("Number of points generated") + ylab(paste("Time in", unit)) +
    ggtitle("Average time of constructPoint (without outlier)") +
    theme(plot.title = element_text(hjust = 0.5)) #+ 
    #ylim(0,max(range))
  
  result.plots
}

n.list <- seq(1000, 10000, by=1000)
subspaces <- list(1:3)
margins <- rep(x = 0.9, times = length(subspaces))
dependency.list <- c("Linear", "Wall", "Square", "Donut","Cross", "Hourglass", "Sine")
Sys.time()
system.time(plot.cp <- makeBenchmarkPlot.constructPoint(n.list = n.list,
                                            dependency.list = dependency.list,
                                            unit = "s",
                                            times = 40))
plot.cp$Raw
plot.cp$Cleaned


#### Time the average construction time of a single point ####
microbenchmark(Cross=constructPoint.Cross(1),
               Donut=constructPoint.Donut(1),
               Hourglass=constructPoint.Hourglass(1),
               Linear=constructPoint.Linear(1),
               Sine=constructPoint.Sine(1),
               Square=construcPoint.Square(1),
               Wall=constructPoint.Wall(1), times=100, unit = "us")



#----------------------------------------------------#
#### Plot the time of constructing a single point ####
#----------------------------------------------------#
makeBenchmarkPlot.constructPoint.Single <- function(n.list = seq(1000, 5000, 1000),
                                             dependency.list = c("Linear", "Sine"),
                                             times = 100,
                                             unit = "ns"){
  results.mean <- results.mbm <- results.mean.rmOutlier <- list()
  
  if (unit == "s") {
    factor <- 1e9
  } else if (unit == "ms") {
    factor <- 1e6
  } else if (unit == "us") {
    factor <- 1e3
  } else {
    factor <- 1
  }
  
  for (dep in dependency.list) {
    f <- get(paste("constructPoint.", dep, sep=""))

    for (i in 1:length(n.list)){
      results.mbm[[dep]][[i]] <- microbenchmark(f(1),
                                                unit = unit,
                                                times = times)
      results.mean[[dep]][[i]] <- summary(results.mbm[[dep]][[i]])$mean
      x <- results.mbm[[dep]][[i]]$time
      results.mean.rmOutlier[[dep]][[i]] <- mean(x[!x %in% boxplot.stats(x)$out]) / factor
    }
  }
  
  df.mean <- as.data.frame(results.mean, stringsAsFactors = F)
  df.mean$N <- n.list
  
  m <- melt(df.mean, id.vars = "N", factorsAsStrings = T)
  i <- sapply(m, is.factor)
  m[i] <- lapply(m[i], as.character)
  #m
  
  result.plots <- list()
  result.plots$Raw <- ggplot(m, aes(x = N, y = value, group=variable,
                                    color=variable)) + geom_line(size=0.8) +
    xlab("Number of runs") + ylab(paste("Time in", unit)) +
    ggtitle("Average time of single constructPoint run") +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~variable, scales = "free")
  #ylim(0, max(m$value))
  
  df.mean <- as.data.frame(results.mean.rmOutlier, stringsAsFactors = F)
  df.mean$N <- n.list
  
  m <- melt(df.mean, id.vars = "N", factorsAsStrings = T)
  i <- sapply(m, is.factor)
  m[i] <- lapply(m[i], as.character)
  #m
  range <- layer_scales(result.plots$Raw)$y$range$range
  
  result.plots$Cleaned <- ggplot(m, aes(x = N, y = value, group=variable,
                                        color=variable)) + geom_line() +
    xlab("Number of points generated") + ylab(paste("Time in", unit)) +
    ggtitle("Average time of constructPoint (without outlier)") +
    theme(plot.title = element_text(hjust = 0.5)) #+ 
  #ylim(0,max(range))
  
  result.plots
}


n.list <- seq(1, 100, by=1)
subspaces <- list(1:3)
margins <- rep(x = 0.9, times = length(subspaces))
dependency.list <- c("Linear", "Wall", "Square", "Donut","Cross", "Hourglass", "Sine")
Sys.time()
system.time(plot.cp.single <- makeBenchmarkPlot.constructPoint.Single(n.list = n.list,
                                                        dependency.list = dependency.list,
                                                        unit = "ns",
                                                        times = 1))
plot.cp.single$Raw
ggsave(path = "analysis/plots/", filename = "SingleRuntimesFacet.pdf", dpi = 300, plot = plot.cp.single$Raw, device = "pdf")
