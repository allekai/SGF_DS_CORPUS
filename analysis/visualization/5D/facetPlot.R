makeFacetPlotStreamVisualization.2D <- function(m) {
    ggplot(m, aes(x=index, y=variable, fill=value)) +
            geom_raster() +
            scale_fill_gradient2(low="blue", high="red") +
            xlab("Window Index") +
            ylab("Correlation Measure") +
            labs(caption="Window size: 100, Nstep = 5, Margins: 0.1, 0.5, 0.9, 0.5, 0.1") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            ggtitle("Heatmaps of Correlation Measures (5D)") +
            facet_wrap(~dep, ncol=3)
}

makeFacetPlotStreamVisualization.2D.transposed <- function(m) {
    ggplot(m, aes(x=index, y=dep, fill=value)) +
            geom_raster() +
            scale_fill_gradient2(low="blue", high="red") +
            xlab("Window Index") +
            ylab("Correlation Measure") +
            labs(caption="Window size: 100, Nstep = 5, Margins: 0.1, 0.5, 0.9, 0.5, 0.1") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            ggtitle("Heatmaps of Correlation Measures (5D)") +
            facet_wrap(~variable, ncol=3)
}
