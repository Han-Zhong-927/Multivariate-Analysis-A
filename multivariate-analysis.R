library(stringdist)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rvest)
library(class)
library(mice)
library(VIM)
library(FactoMineR)
library(randomForest)
library(rpart.plot)
library(rpart)
library(dplyr)
library(caret)
library(ROCR)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(caret)
library(calibrate)




###################################################
## 1 - PRINCIPAL COMPONENTS ANALYSIS
###################################################

nba <- read.table("final_shot_logs.RData")
nba <- nba[,c(16, 3, 6:10, 12, 14:15, 19, 21:27, 11, 13, 17, 18, 20)]

trainIndex <- createDataPartition(nba$success, p=0.7, list=FALSE, times=1)
train <- nba[trainIndex,]
test <- nba[-trainIndex,]
save(train, file = "train.RData")
save(test, file = "test.RData")

active.vars <- c(2:18)
sup.vars <- c(1, 19:23)

# Apply (standardized) PCA to the active individuals and variables
pca <- PCA(nba, scale.unit = TRUE, 
           ncp = 9,
           ind.sup = as.numeric(rownames(test)),
           quali.sup = sup.vars, graph = FALSE)

# Visualize eigenvalues in a screeplot                      DECIDE NUMBER OF SIGNIFICANT PC!
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 35), ncp = 15)
# Using the Last Elbow rule, it looks as though the number of significant components is 9
sc <- 9

var <- get_pca_var(pca)

corrplot(var$cos2, is.corr=FALSE)
fviz_pca_var(pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(pca, axes = c(2,3), col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
  

# INDIVIDUAL ANALYSIS
fviz_pca_ind(pca, col.ind = "cos2", 
             invisible = "ind.sup", # SHOULD WE SHOW TEST INDIVIDUALS?
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             geom = c("point"))

fviz_pca_ind(pca, axes = c(2,3), col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             geom = c("point"))

fviz_pca_ind(pca, geom = c("point"), col.ind = factor(train$success), invisible = "ind.sup")

# BIPLOT
fviz_pca_biplot(pca, repel = TRUE,
                geom = c("point"),
                invisible = "ind.sup", 
                col.var = "black", 
                col.ind = "red"  
)




Psi <- pca$ind$coord
Phi <- pca$var$coord
X <- train[,active.vars]
Xs <- scale(X)

pc.rot <- varimax(Phi)

Phi.rot <- pc.rot$loadings
lmb.rot <- diag(t(Phi.rot) %*% Phi.rot)

Psi_stan.rot <- Xs %*% solve(cor(X)) %*% Phi.rot
Psi.rot <- Psi_stan.rot %*% diag(sqrt(lmb.rot))

plot_variables_factor_map <- function(coords, title, xdim=1, ydim=2) {
  plot(0, 0, col = "white", asp=1, xlab=paste("Dim", toString(xdim)), ylab=paste("Dim", toString(ydim)), main=title)
  x.cercle <- seq(-1, 1, by = 0.01)
  y.cercle <- sqrt(1 - x.cercle^2)
  lines(x.cercle, y = y.cercle)
  lines(x.cercle, y = -y.cercle)
  abline(h=0, lty=2)
  abline(v=0, lty=2)
  
  for (var in c(1:nrow(coords))) {
    arrows(0, 0, x1 = coords[var,xdim], y1 = coords[var,ydim], length = 0.1, angle = 30, col="red")
    text(x=coords[,xdim], y=coords[,ydim], labels=row.names(coords), pos=ifelse(coords[,ydim] > 0, 3, 1))
  }
}

par(mfrow=c(1,2))

plot_variables_factor_map(Phi, "Variables factor map (PCA)")
plot_variables_factor_map(Phi.rot, "Rotated variables factor map (PCA)")

plot_variables_factor_map(Phi, "Variables factor map (PCA)", xdim = 2, ydim = 3)
plot_variables_factor_map(Phi.rot, "Rotated variables factor map (PCA)", xdim = 2, ydim = 3)

plot(Psi)
plot(Psi.rot)

plot(Psi[,c(2,3)])
plot(Psi.rot[,c(2,3)])


n1 <- 14
k1 <- kmeans(Psi.rot,n1)
k2 <- kmeans(Psi.rot,n1)
table(k2$cluster,k1$cluster)
clas <- (k2$cluster-1)*n1+k1$cluster
freq <- table(clas)   # WHAT DO WE HAVE IN VECTOR freq?
cdclas <- aggregate(as.data.frame(Psi.rot),list(clas),mean)[,2:(sc+1)]

# SECOND HIERARCHICAL CLUSTERING UPON THE CENTROIDS OF CROSSING THE 2 KMEANS PARTITIONS
d2 <- dist(cdclas)
h2 <- hclust(d2,method="ward.D2",members=freq)  # COMPARE THE COST
plot(h2)
abline(h=11.2, col = "red")
barplot(h2$height[(length(h2$height)-15):length(h2$height)],
        ylim = c(0,30), main = "Aggregated distance at the last iterations",
        xlab = "Iterations", ylab = "Height")
abline(v=15.7, col = "red")

nc <- 4   # for instance
c2 <- cutree(h2,nc)
cdg <- aggregate((diag(freq/sum(freq)) %*% as.matrix(cdclas)),list(c2),sum)[,2:(sc+1)]  # WHY WEIGHT

# CONSOLIDATION
k4 <- kmeans(Psi,centers=cdg)
Bss <- sum(rowSums(k4$centers^2)*k4$size)
Wss <- sum(k4$withinss)
Ib4 <- 100*Bss/(Bss+Wss)
Ib4

colors <- c("black", "red", "green", "blue")
palette(colors)
clusters <- factor(k4$cluster, labels = colors)

par(mfrow=c(1,1))
plot(Psi.rot, col = clusters, main = "Clusters after consolidation", xlab = "Dim 1", ylab = "Dim 2")
abline(h=0,v=0,col="gray")

plot(Psi[,2:3], col = clusters)

catdes.res <- catdes(cbind(clusters, train), 1)
catdes.res

catdes.res$category$black[1:6,]
rbind(head(catdes.res$quanti$black, 4), tail(catdes.res$quanti$black, 3))

catdes.res$category$red[1:5,]
rbind(head(catdes.res$quanti$red, 3), tail(catdes.res$quanti$red, 1))

catdes.res$category$green[1:6,]
rbind(head(catdes.res$quanti$green, 4), tail(catdes.res$quanti$green, 4))

catdes.res$category$blue[1:4,]
rbind(head(catdes.res$quanti$blue, 2), tail(catdes.res$quanti$blue, 2))






# use the results from PCA
unnecesary.vars <- c("shooter", "defender", "shot_cat_pct", "shot_difficulty_pct", "position_pct")
1 - colnames(train)  %in% unnecesary.vars
train <- train[ ,1 - colnames(train)  %in% unnecesary.vars]
test <- train[ ,1 - colnames(test)  %in% unnecesary.vars]
train <- select(train, -one_of(unnecesary.vars))
test <- select(test, -one_of(unnecesary.vars))


###################################################
## 1 - DECISION TREE
###################################################

tree <- rpart(success ~ ., data = train, method="class", 
              control = rpart.control(cp = 0.0001, xval = 10))
alpha <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]

tree.pruned <- prune(tree, cp=alpha)
printcp(tree.pruned)

bplot <- barplot(tree.pruned$variable.importance[1:10], main = "Variable importance (Decision tree)", xaxt="n")
labs <- names(tree.pruned$variable.importance[1:10])
text(cex=1, x=bplot-.25, y=-250, labs, xpd=TRUE, srt=45)

predictions.dt <- as.data.frame(predict(tree.pruned, test[,-1]))
(tb <- table(round(predictions.dt$`1`), test[,1]))

(accuracy.dt <- sum(diag(tb)) / sum(tb))
(precision.p <- tb[2,2] / sum(tb[,2]))
precision.n <- tb[1,1] / sum(tb[,1])
(precision.dt <- (precision.p + precision.n) / 2)



###################################################
## 2 - RANDOM FOREST
###################################################

# Grid search to tune model's parameters.
nvars.rf <- seq(3, ncol(train)-1, by=2)
ntrees.rf <- c(100, 500, 1000)
rf.parameters <- expand.grid(ntrees=ntrees.rf, nvars=nvars.rf)

res <- vector("numeric", nrow(rf.parameters))
for(i in 1:length(res)) {
  model.rf <- randomForest(success ~ ., data=train, ntree=rf.parameters[i,1], 
                           mtry=rf.parameters[i,2], proximity=FALSE)
  res[i] <- model.rf$err.rate[rf.parameters[i,1], 1]
}

# Fit the model with the set of optimum parameters.
(best.nvars <- rf.parameters[which.min(res),2])
(best.ntrees <- rf.parameters[which.min(res),1])
model.rf <- randomForest(success ~ ., data=train, ntree=best.ntrees, 
                         mtry=best.nvars, proximity=FALSE, importance=TRUE)

# Variable importance plot.
varImpPlot(model.rf, main="Variable importance (Random Forest)")

# Confusion matrix after using the model to predict the test set.
prediction.rf <- predict(model.rf, newdata=test)
(tb <- table(prediction.rf, test$success))

(accuracy.rf <- sum(diag(tb)) / sum(tb))
(precision.p <- tb[2,2] / sum(tb[,2]))
precision.n <- tb[1,1] / sum(tb[,1])
(precision.rf <- (precision.p + precision.n) / 2)



###################################################
## 3 - COMPARISON
###################################################

pred.dt <- prediction(predictions.dt$`1`, test$success)
pred.rf <- prediction(predict(model.rf, type="prob", newdata=test)[,2], test$success)

perf.dt <- performance(pred.dt, "tpr", "fpr")
perf.rf <- performance(pred.rf, "tpr", "fpr")

plot(perf.dt, col="red", main="ROC Curve comparison")
plot(perf.rf, add = TRUE)
legend(0, 1, legend=c("Decision Tree", "Random Forest"),
       col=c("red", "black"), lty=1:2, cex=1)

