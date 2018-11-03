library(ggplot2)
library(rlist)
library(streamgenerator)
library(reshape2)


applySlidingWindow <- function(x, fun, size, ...) {
    # Apply a function over a data frame with a sliding window.
    #
    # Args:
    #   x       A data frame
    #   fun     The function to apply
    #   size    The window size
    #
    # Return:
    #   List of results of each window.
    #
    # Details:
    #   x needs to be a data frame. Make sure, that fun can
    #   directly be applied to x.
    data.split <- list()
    for (i in 1:(nrow(x)/size)) {
        data.split[[i]] <- x[(((i - 1) * size) + 1):(i * size),]
    }
    fun.results <- lapply(data.splits, fun, ...)
    fun.results
}

applySlidingWindow.Correlation.2D <- function(x, size, method, ...) {
    # Apply a function over a data frame with a sliding window.
    #
    # Args:
    #   x       A data frame
    #   fun     The function to apply
    #   size    The window size
    #
    # Return:
    #   List of results of each window.
    #
    # Details:
    #   x needs to be a data frame. Make sure, that fun can
    #   directly be applied to x.
    data.split <- list()
    for (i in 1:(nrow(x)/size)) {
        data.split <- list.append(data.split, x[(((i - 1) * size) + 1):(i * size),1:2])
    }
    fun.results <- lapply(data.split,
                          function(x, method) {cor(x[,1], x[,2], method=method)},
                          method=method)
    fun.results
}


generate.Correlations.2D <- function(stream.2D,
                                     method.list = c("spearman"),
                                     size = 100) {
    result.list <- list()
    for (meth in method.list) {
        result.list <- list.append(result.list,
                                   applySlidingWindow.Correlation.2D(stream.2D$data[,1:2],
                                                size=size,
                                                method=meth))
    }
    names(result.list) <- method.list
    result <- as.data.frame(lapply(result.list, unlist))
    result <- as.data.frame(cbind((1:nrow(result)), result))
    names(result) <- c("index", method.list)
    result
}

makeCorrelationPlot.2D <- function(stream,
                                   dep = "Linear",
                                   methods = c("spearman"),
                                   size = 100) {
    df <- generate.Correlations.2D(stream.2D = stream,
                                   method.list = methods,
                                   size = size)
    m <- melt(df, id.vars = "index")
    prefix <- paste("Dependency:", dep, "/ Margins:", collapse="")
    suffix <- paste(((unlist(stream$stream.config$marginslist)-1)*(-1)), collapse=", ")
    ggplot(m, aes(x=index, y=variable, fill=value)) +
        geom_raster() +
        xlab("Window Index") +
        ylab("Correlation Measure") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        ggtitle(sprintf("Heatmap of Correlation Measures with window size %i", size)) +
        labs(caption=paste(prefix, suffix), sep=" ")
}

getStream.2D <- function(dep = "Linear", fix = T) {
    conf.2D <- generate.stream.config(dim=10,
                                      mindim = 2,
                                      maxdim = 2,
                                      dependency = dep,
                                      nstep = 5)
    conf.2D$subspaceslist <- rep(list(list(1:2)),
                                      length(conf.2D$subspaceslist))
    if (fix) {
        conf.2D$marginslist <- as.list(c(0.9, 0.5, 0.1, 0.5, 0.9))
    } else {
        conf.2D$marginslist <- as.list(sample(c(0.1, 0.5, 0.9),
                                              length(conf.2D$marginslist),
                                              replace = TRUE))
    }

    stream.2D <- generate.dynamic.stream(n = 1000, method="Construction",
                                         stream.config=conf.2D) 
    stream.2D
}
