
populateLists <- function() {
    for (dep in dependency.list) {
        stream.name <- paste("stream.", dep, collapse="", sep="")
        assign(stream.name,
               getStream.5D(dep))
        correlation.name <- paste("corr.", dep, collapse="",sep="")
        assign(correlation.name, data.frame(rep(dep, times=50)))

        # asign to global variable
        stream.list <<- list.append(stream.list, get(stream.name))
        correlation.list <<- list.append(correlation.list, get(correlation.name))
        if (!dir.exists("data")) dir.create("data")
        setwd("data/")
        output.stream(get(stream.name), paste(dep, "5D", collapse="",sep=""))
        setwd("../")
    }
    names(correlation.list) <<- dependency.list
    #for (dep in names(correlation.list)) {
        #correlation.list[[dep]] <<- data.frame(cbind(correlation.list[[dep]],
                                                  #dep))
    #}
}

coerceCorrelationLists <- function(correlation.list = NULL, madeSplits = FALSE) {
    if (is.null(correlation.list)) stop("Need correlation list!")
    if (!madeSplits) stop("Run the makeSplits.sh first!")
    for (dep in dependency.list) {
        names(correlation.list[[dep]]) <- c("dep")
    }
    files <- list.files(path="data/", pattern="^.*\\.csv$")
    for (file in files) {
        f <- read.csv(paste("data/",file, collapse="", sep=""),
                      stringsAsFactors = FALSE)
        curr.Dep <- switch(substr(file, 1, 3),
                           "Lin" = "Linear",
                           "Wal" = "Wall",
                           "Squ" = "Square",
                           "Don" = "Donut",
                           "Hou" = "Hourglass",
                           "Cro" = "Cross",
                           "Sin" = "Sine")
        correlation.list[[curr.Dep]] <- data.frame(cbind(f, correlation.list[[curr.Dep]]))
    }
    correlation.list
}

generateFacetPlotFromAllStreams <- function(corr.list) {
    for (dep in dependency.list) {
        corr.list[[dep]]$index <- 1:nrow(corr.list[[dep]])
        corr.list[[dep]]$II <- abs(corr.list[[dep]]$II / (max(corr.list[[dep]]$II) - min(corr.list[[dep]]$II)))
        corr.list[[dep]]$TC <- abs(corr.list[[dep]]$TC / (max(corr.list[[dep]]$TC) - min(corr.list[[dep]]$TC)))
        corr.list[[dep]]$MWP <- (corr.list[[dep]]$MWP - 0.5) * 2
        corr.list[[dep]]$MAC <- (corr.list[[dep]]$MAC - 0.5) * 2
    }
    df <- do.call(rbind, corr.list)
    m <- melt(df, id.vars = c("index", "dep"))
    m[["value"]] <- abs(m[["value"]])
    source("facetPlot.R")
    facet.plot <-  makeFacetPlotStreamVisualization.2D(m)
    ggsave(filename="correlationFacetPlot.pdf",
           plot=facet.plot,
           path="plots/",
           device="pdf",
           height = 20,
           width = 15,
           units="cm")
    facet.plot.2 <-  makeFacetPlotStreamVisualization.2D.transposed(m)
    ggsave(filename="correlationFacetPlot2.pdf",
           plot=facet.plot.2,
           path="plots/",
           device="pdf",
           height = 20,
           width = 15,
           units="cm")
}
