dependency.list <- c("Linear",
                     "Wall",
                     "Square",
                     "Donut",
                     "Hourglass",
                     "Cross",
                     "Sine")
source("visualizationMethods.R")

stream.list <- list()
correlation.list <- list()
plot.list <- list()

populateLists <- function() {
    for (dep in dependency.list) {
        stream.name <- paste("stream.", dep, collapse="", sep="")
        assign(stream.name,
               getStream.2D(dep))
        correlation.name <- paste("corr.", dep, collapse="",sep="")
        assign(correlation.name,
               generate.Correlations.2D(stream=get(stream.name),
                                        method.list=c("spearman",
                                                  "kendall",
                                                  "pearson"),
                                        size=100))
        plot.name <- paste("p.", dep, collapse="", sep="")
        assign(plot.name,
               makeCorrelationPlot.2D(stream=get(stream.name),
                                      methods=c("spearman",
                                                "kendall",
                                                "pearson"),
                                      size=100))
        stream.list <- list.append(stream.list, get(stream.name))
        correlation.list <- list.append(correlation.list, get(correlation.name))
        plot.list <- list.append(plot.list, get(plot.name))
        ggsave(filename=paste(plot.name,".pdf",collaps="",sep=""),
               plot=get(plot.name), device="pdf",
               path="plots/",
               height = 5,
               width= 15,
               units = "cm")
        if (!dir.exists("data")) dir.create("data")
        setwd("data/")
        output.stream(get(stream.name), paste(dep, "2D", collapse="",sep=""))
        setwd("../")
    }
}

generateFacetPlotFromAllStreams <- function() {
    names(correlation.list) <- dependency.list
    for (dep in names(correlation.list)) {
        correlation.list[[dep]] <- data.frame(cbind(correlation.list[[dep]],
                                                  dep))
    }
    df <- do.call(rbind, correlation.list)
    m <- melt(df, id.vars = c("index", "dep"),
              measure.vars = c("spearman", "kendall", "pearson"))
    m[["value"]] <- abs(m[["value"]])
    source("facetPlot.R")
    facet.plot <-  makeFacetPlotStreamVisualization.2D(m)
    ggsave(filename="correlationFacetPlot.pdf",
           plot=facet.plot,
           path="plots/",
           device="pdf",
           height = 10,
           width = 15,
           units="cm")
}
