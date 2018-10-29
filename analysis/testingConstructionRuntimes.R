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
      x <- runif(n = length(subspaces[[i]]))
      results.mbm[[dep]][[i]] <- microbenchmark(f(row = x, subspace = i), unit = unit, times = times)
      results.mean[[dep]][[i]] <- summary(results.mbm[[dep]][[i]])$mean
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


subspaces <- list(1:2, 1:3, 1:4, 1:5, 1:6, 1:7, 1:8, 1:9, 1:10)
margins <- rep(x = 0.9, times = length(subspaces))
dependency.list <- c("Linear", "Wall", "Square", "Donut")
plot.quick <- makeBenchmarkPlot.isInHiddenSpace(subspaces = subspaces, margins = margins,
                                                dependency.list = dependency.list, unit = "ns",
                                                times = 10000)


subspaces <- list(1:2, 1:3, 1:4, 1:5)#, 1:6, 1:7, 1:8, 1:9, 1:10)
margins <- rep(x = 0.9, times = length(subspaces))
dependency.list <- c("Cross", "Hourglass")
plot.slow <- makeBenchmarkPlot.isInHiddenSpace(subspaces = subspaces, margins = margins,
                                               dependency.list = dependency.list, unit = "s",
                                               times = 100)

subspaces <- list(1:2, 1:3)#, 1:6, 1:7, 1:8, 1:9, 1:10)
margins <- rep(x = 0.9, times = length(subspaces))
dependency.list <- c("Sine")
plot.sine <- makeBenchmarkPlot.isInHiddenSpace(subspaces = subspaces, margins = margins,
                                               dependency.list = dependency.list, unit = "ms",
                                               times = 100)
