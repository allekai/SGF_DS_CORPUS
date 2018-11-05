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



#### Benchmark LOF ####
# Setup
max.k <- 10
stream <- getStaticStream()
output.stream(stream, "data/testStaticLinear")
data <- stream$data[-ncol(stream$data)]
data.labels <- stream$data[ncol(stream$data)]
names(data.labels) <- "label"

# Generate LOFs
data.lof <- lof(data, c(1:max.k), 2)
data.lof.labeled <- data.frame(data.lof, data.labels)

# Make predictions by setting values to 0 and 1 accordingly
# If LOF > threshold, make 1 (=outlier) else 0.
makePredictions.LOF <- function(threshold, lofs) {
    lofs[lofs < threshold] <- 0
    lofs[lofs  != 0] <- 1
    lofs
}
for (k in 1:max.k) {
    varname.pred <- paste("lof.prediction.k.",k,collapse="",sep="")
    assign(varname.pred, list())
    varname.perf <- paste("lof.perf.k.",k,collapse="",sep="")
    assign(varname.perf, list())
    for (thresh in (seq(from=1,to=2,by=0.1))) {
        predicted.data <- makePredictions.LOF(threshold=thresh,lofs=data.lof[,k])
        pred <- prediction(predicted.data, data.labels)
        assign(varname.pred, list.append(get(varname.pred), pred))
        perf <- performance(pred, "tpr", "fpr")
        assign(varname.perf, list.append(get(varname.perf), perf))
    }
}

for (k in 1:max.k) {
    varname.perf <- paste("lof.perf.k.",k,collapse="",sep="")
    varname.points <- paste("points.k.",k,collapse="",sep="")
    varname.points.plottable <- paste("plottable.points.k.",k,collapse="",sep="")
    assign(varname.points, list())
    for (i in (1:(length(get(varname.perf))))) {
        assign(varname.points,
               list.append(get(varname.points),
                           c(x=get(varname.perf)[[i]]@x.values[[1]][2],
                             y=get(varname.perf)[[i]]@y.values[[1]][2])))
    }
    assign(varname.points.plottable,
           list(x = unlist(lapply(X=get(varname.points),
                                  FUN=function(x){
                                      res <- x["x"];
                                      names(res) <- NULL;
                                      res})),
                y = unlist(lapply(X=get(varname.points),
                                  FUN=function(x){
                                      res <- x["y"];
                                      names(res) <- NULL;
                                      res}))))
}

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
