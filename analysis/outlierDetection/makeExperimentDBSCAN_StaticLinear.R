list.of.packages  <- c("ggplot2",
                       "dbscan",
                       "streamgenerator")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(!onlyPlot) {
prop = 0.01
n = 1000
path.data <- "data/testStaticLinear"

getStaticStream <- function(n=1000,
                            dep="Linear",
                            dim=10,
                            subDim=3,
                            margin=0.9,
                            prop=0.05) {
    conf <- streamgenerator::generate.stream.config(dim=dim,
                                                    dependency=dep,
                                                    nstep=1)
    conf$subspaces <- list(1:subDim)
    conf$margins <- margin
    stream <- streamgenerator::generate.static.stream(n=n,
                                                      stream.config=conf,
                                                      prop=prop,
                                                      method="Construction")
    output.stream(stream, path.data)
}

stream <- read.csv(paste(path.data,
                         "_data.txt",
                         collapse="", sep=""),
                   stringsAsFactors=FALSE)
data <- stream[-ncol(stream)]
data.labels <- stream[ncol(stream)]
names(data.labels) <- "label"

normalizeDBSCAN.Clusters <- function(clusters) {
   indices <- which(clusters == 0)
   outlier.score <- rep(0, length(clusters))
   outlier.score[indices] <- 1
   outlier.score
}


getPrecision <- function(predicted.outlier, labels) {
    # precision = tp /tp + fp
    predicted.outlier.indices <- which(predicted.outlier == 1)
    labels.outlier.indices <- which(labels == 1)
    tp <- length(intersect(predicted.outlier.indices, labels.outlier.indices))
    fp <- length(setdiff(predicted.outlier.indices, labels.outlier.indices))
    precision <- tp / (tp + fp)
    precision
}

getAccuracy <- function(predicted.outlier, labels) {
    # accuracy = (tp + tn) / (N)
    predicted.outlier.indices <- which(predicted.outlier == 1)
    predicted.inlier.indices <- which(predicted.outlier == 0)
    labels.outlier.indices <- which(labels == 1)
    labels.inlier.indices <- which(labels == 0)
    tp <- length(intersect(predicted.outlier.indices, labels.outlier.indices))
    tn <- length(intersect(predicted.inlier.indices, labels.inlier.indices))
    fp <- length(setdiff(predicted.outlier.indices, labels.outlier.indices))
    fn <- length(setdiff(predicted.inlier.indices, labels.inlier.indices))
    acc <- (tp + tn) / (tp + tn + fp + fn)
    acc
}

getRecall <- function(predicted.outlier, labels) {
    # recall = tp / (tp + fn)
    predicted.outlier.indices <- which(predicted.outlier == 1)
    predicted.inlier.indices <- which(predicted.outlier == 0)
    labels.outlier.indices <- which(labels == 1)
    labels.inlier.indices <- which(labels == 0)
    tp <- length(intersect(predicted.outlier.indices, labels.outlier.indices))
    tn <- length(intersect(predicted.inlier.indices, labels.inlier.indices))
    fp <- length(setdiff(predicted.outlier.indices, labels.outlier.indices))
    fn <- length(setdiff(predicted.inlier.indices, labels.inlier.indices))
    rec <- tp / (tp + fn)
    rec
}

getF1Score <- function(prec, rec) {
    2 * (prec * rec) / (prec + rec)
}

filepath = "dbscanresults_LINEAR.csv"
eps.steps <- seq(0.1, 0.74, 0.01)
write('"minPts","eps","Precision", "Recall","F1"', file=filepath)
for (pts in 2:10) {
    for (eps in eps.steps) {
        scan <- dbscan(data, eps=eps, minPts=pts)
        scan.outlier.prediction <- normalizeDBSCAN.Clusters(scan$cluster)

        prec <- getPrecision(scan.outlier.prediction, data.labels)
        acc <- getAccuracy(scan.outlier.prediction, data.labels)
        rec <- getRecall(scan.outlier.prediction, data.labels)
        f1 <- getF1Score(prec, rec)

        print(paste("DBSCAN Result for eps = ", eps,
                    " and minPts = ", pts,
                    "    Precision = ", prec,
                    "    Recall = ", rec,
                    "    F1 = ", f1,
                    collapse="", sep=""))
        write(paste(pts, eps, prec, rec, f1, collapse="", sep=","),
              file=filepath, append=TRUE)
    }
    df <- read.csv(filepath)
}

exp.res <- read.csv(filepath)
f1.scores <- vector(mode="integer")
for (i in 1:length(2:10)){
    f1.values <- exp.res[((i-1)*length(eps.steps)+1):(i*length(eps.steps)),"F1"]
    f1.val <- max(f1.values)
    f1.scores <- c(f1.scores, f1.val)
}
write.table(x=exp.res[which(exp.res$F1 %in% f1.scores),],
            file="DBSCAN_F1_Scores_Linear.csv", sep=",")

write('"Index","minPts","eps","Precision","Recall","F1"', file="dbscanF1Scores.csv")
for (i in nrow(exp.res)) {
    if (exp.res[i,"F1"] %in% f1.scores) {
        write(paste(i,
                    exp.res[i,"minPts"],
                    exp.res[i,"eps"],
                    exp.res[i,"Precision"],
                    exp.res[i,"Recall"],
                    exp.res[i,"F1"],
                    collapse="",
                    sep=","),
              file="dbscanF1Scores.csv")
    }
}

}
plot <- ggplot(exp.res, aes(x=Recall, y=Precision)) +
            geom_line() +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.spacing = unit(1, "lines")) +
            ggtitle("DBSCAN with varying minPts parameter") +
            facet_wrap(~minPts)
ggsave(filename="dbscanPrecRecPlot.pdf", plot=plot, device="pdf",
       height = 15,
       width = 20,
       units = "cm")


