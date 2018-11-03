dependency.list <- c("Linear",
                     "Wall",
                     "Square",
                     "Donut",
                     "Hourglass",
                     "Cross",
                     "Sine")
source("visualizationMethods.R")

stream.list <- list()
plot.list <- list()
for (dep in dependency.list) {
    #browser()
    stream.name <- paste("stream.", dep, collapse="", sep="")
    assign(stream.name,
           getStream.2D(dep))
    plot.name <- paste("p.", dep, collapse="", sep="")
    assign(plot.name,
           makeCorrelationPlot.2D(stream=get(stream.name),
                                  methods=c("spearman",
                                            "kendall",
                                            "pearson"),
                                  size=100))
    stream.list <- list.append(stream.list, get(stream.name))
    plot.list <- list.append(plot.list, get(plot.name))
    ggsave(filename=paste(plot.name,".pdf",collaps="",sep=""),
           plot=get(plot.name), device="pdf",
           path="plots/",
           height = 5,
           width= 15,
           units = "cm")
}
