# HOW TO USE ROCR
# library (ROCR);
# ...
# 
# y <- ... # logical array of positive / negative cases
# predictions <- ... # array of predictions
# 
# pred <- prediction(predictions, y);
# 
# # Recall-Precision curve
# RP.perf <- performance(pred, "prec", "rec");
# 
# plot (RP.perf);
# 
# # ROC curve
# ROC.perf <- performance(pred, "tpr", "fpr");
# plot (ROC.perf);
# 
# # ROC area under the curve
# auc.tmp <- performance(pred,"auc");
# auc <- as.numeric(auc.tmp@y.values)




# The actual code
list.of.packages  <- c("Rlof",
                       "rlist",
                       "dbscan",
                       "ROCR",
                       "streamgenerator")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


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
    stream
}


#### OKAY TRY THIS. USE LOF FROM DMWR PACKAGE,
#### CALCULATE THEM SCORES, ORDER THEM AND TAKE A FIXED
#### PROPORTION AS OUTLIER. THEN CHECK THE PERFORMANCE FOR
#### DIFFERENT K VALUES.



# Setup
max.k <- 20
prop = 0.01
n = 10000
path.data <- "data/testStaticLinear"
if (!list.files(path="data/", pattern="testStaticLinear_data.txt") > 1) {
    stream <- getStaticStream(n=n,prop=prop)
    output.stream(stream, path.data)
    data <- stream$data[-ncol(stream$data)]
    data.labels <- stream$data[ncol(stream$data)]
} else {
    stream <- read.csv(paste(path.data, "_data.txt", collapse="", sep=""), stringsAsFactors=FALSE)
    data <- stream[-ncol(stream)]
    data.labels <- stream[ncol(stream)]
}
names(data.labels) <- "label"

min.Pts <- 3
max.Pts <- 10
eps <- 0.5

# DBSCAN
for (pts in min.Pts:max.Pts) {
    varname.pred <- paste("dbscan.prediction.pts.",pts,collapse="",sep="")
    assign(varname.pred, list())
    varname.perf <- paste("dbscan.perf.pts.",pts,collapse="",sep="")
    assign(varname.perf, list())
    predicted.data <- dbscan(data, eps=eps, minPts=pts)
#    browser()
    predicted.outlier.indices <- which(predicted.data$cluster == 0)
    dbscan.outlier.prediction <- rep(0, nrow(data))
    dbscan.outlier.prediction[predicted.outlier.indices] <- 1
    pred <- prediction(dbscan.outlier.prediction, data.labels)
    assign(varname.pred, list.append(get(varname.pred), pred))
    perf <- performance(pred, "tpr", "fpr")
    assign(varname.perf, list.append(get(varname.perf), perf))
}

varname.points.plottable <- paste("plottable.points",collapse="",sep="")
assign(varname.points.plottable, list())
for (k in min.Pts:max.Pts) {
    varname.perf <- paste("dbscan.perf.pts.",k,collapse="",sep="")
    varname.points <- paste("points.pts.",k,collapse="",sep="")
    assign(varname.points, list())
    for (i in (1:(length(get(varname.perf))))) {
        assign(varname.points,
               list.append(get(varname.points),
                           c(x=get(varname.perf)[[i]]@x.values[[1]][2],
                             y=get(varname.perf)[[i]]@y.values[[1]][2])))
    }
    assign(varname.points.plottable, 
           list.append(get(varname.points.plottable),
           c(x = unlist(lapply(X=get(varname.points),
                                  FUN=function(x){
                                      res <- x["x"];
                                      names(res) <- NULL;
                                      res})),
                y = unlist(lapply(X=get(varname.points),
                                  FUN=function(x){
                                      res <- x["y"];
                                      names(res) <- NULL;
                                      res})))))
}
x <- unlist(lapply(get(varname.points.plottable), function(x) x["x"]))
names(x) <- NULL
y <- unlist(lapply(get(varname.points.plottable), function(x) x["y"]))
names(y) <- NULL
assign(varname.points.plottable, list(x=x, y=y))

# Generate LOFs
#data.lof <- lof(data, c(1:max.k), 2)
#data.lof.labeled <- data.frame(data.lof, data.labels)
#
## Set the proportion of points specified by outlierProportion to
## 1 (=outlier) others 0. Take the outlierProportion largest LOFs for
## this.
#makePredictions.LOF <- function(lofs, outlierProportion) {
#    outlier.indices <- order(lofs, decreasing=TRUE)[1:outlierProportion]
#    lofs <- rep(0, length(lofs))
#    lofs[outlier.indices] <- 1
#    lofs
#}
#for (k in 1:max.k) {
#    varname.pred <- paste("lof.prediction.k.",k,collapse="",sep="")
#    assign(varname.pred, list())
#    varname.perf <- paste("lof.perf.k.",k,collapse="",sep="")
#    assign(varname.perf, list())
#    predicted.data <- makePredictions.LOF(lofs=data.lof[,k], outlierProportion=prop*nrow(data))
#    pred <- prediction(predicted.data, data.labels)
#    assign(varname.pred, list.append(get(varname.pred), pred))
#    perf <- performance(pred, "tpr", "fpr")
#    assign(varname.perf, list.append(get(varname.perf), perf))
#}
#
#varname.points.plottable <- paste("plottable.points",collapse="",sep="")
#assign(varname.points.plottable, list())
#for (k in 1:max.k) {
#    varname.perf <- paste("lof.perf.k.",k,collapse="",sep="")
#    varname.points <- paste("points.k.",k,collapse="",sep="")
#    assign(varname.points, list())
#    for (i in (1:(length(get(varname.perf))))) {
#        assign(varname.points,
#               list.append(get(varname.points),
#                           c(x=get(varname.perf)[[i]]@x.values[[1]][2],
#                             y=get(varname.perf)[[i]]@y.values[[1]][2])))
#    }
#    assign(varname.points.plottable, 
#           list.append(get(varname.points.plottable),
#           c(x = unlist(lapply(X=get(varname.points),
#                                  FUN=function(x){
#                                      res <- x["x"];
#                                      names(res) <- NULL;
#                                      res})),
#                y = unlist(lapply(X=get(varname.points),
#                                  FUN=function(x){
#                                      res <- x["y"];
#                                      names(res) <- NULL;
#                                      res})))))
#}
#x <- unlist(lapply(get(varname.points.plottable), function(x) x["x"]))
#names(x) <- NULL
#y <- unlist(lapply(get(varname.points.plottable), function(x) x["y"]))
#names(y) <- NULL
#assign(varname.points.plottable, list(x=x, y=y))
#
#pred.list <- lapply(seq(from=1, to=2, by=0.1),
#                    function(x) makePredictions.LOF(threshold=x, lofs=data.lof))
#
## Make plot
#counter <- 1
#filename <- "multiPlot.pdf"
#old.par <- par(mfrow=c(3, 4))
#pdf(filename)
#for (p in pred.list) {
#    filename <- paste("ROC curve for lof value of 1.", counter, collapse="", sep="")
#    plot(NULL, xlab="False positive rate", ylab="True positive rate", xlim=c(0,1), ylim=c(0,1), main=filename)
#    for (k in 1:max.k) {
#        pred <- prediction(p[,k], data.labels)
#        perf <- performance(pred, "tpr", "fpr")
#        plot(perf, add=T, col=k)
#    }
##    dev.off()
#    counter <- counter + 1
#}
#dev.off()
#par(old.par)
#
#
## Make single plot
#pred.mat <- pred.list[[1]]
#pred.mat <- prediction(pred.mat, replicate(ncol(pred.list[[1]]), as.vector(unlist(data.labels), mode="integer")))
#perf.mat <- performance(pred.mat, "tpr", "fpr")
#pdf("test.pdf")
#plot(perf.mat, col=c(1:ncol(pred.list[[1]])), main="single prediction plot")
#dev.off()
