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


nba <- read.table(file = '~/Documents/Latex/multivariate paper/code/multivariate analysis/shot_logs.csv', sep = ',', header = TRUE)



###################################################
## 1 - DATA SELECTION
###################################################
# 1- Split "MATCHUP" into date, home_team and away_team
nba <- extract(nba, 'MATCHUP', into=c('date', 'away_team', 'home_team'), 
               '(.*) - ([a-zA-Z]{3}) (?:@|vs\\.) ([a-zA-Z]{3})', remove=TRUE)

# 2- Parse date to YYYY-MM-DD
date <- format(mdy(nba$date),"%Y-%m-%d")

# 3- Turn "GAME_CLOCK" into "remaining_seconds". As it could be better understood by models.
to_seconds <- function(game_clock) {
  minutes_seconds <- unlist(strsplit(as.character(game_clock), ":"))
  minutes <- as.integer(minutes_seconds[1])
  seconds <- as.integer(minutes_seconds[2])
  return(seconds + (minutes*60))
}

remaining_seconds <- sapply(nba$GAME_CLOCK, to_seconds)

# 4- Transform distances from feets to meters.
shot_dist <- nba$SHOT_DIST * 0.3048
defender_dist <- nba$CLOSE_DEF_DIST * 0.3048

# 5- Adjust data types
success <- as.integer(nba$SHOT_RESULT == 'made')
is_home <- as.integer(nba$LOCATION == 'H')
shot_number <- as.ordered(nba$SHOT_NUMBER)
shot_type <- as.ordered(nba$PTS_TYPE)
victory <- as.integer(nba$W == 'W')
period <- as.ordered(nba$PERIOD)

# 6- Rename columns
nba.1 <- data.frame(date, home_team=nba$home_team, away_team=nba$away_team, is_home, victory, final_margin=nba$FINAL_MARGIN, 
                    shot_number, period, remaining_seconds, shot_clock=nba$SHOT_CLOCK,shot_dist, shot_type, shooter=nba$player_name, 
                    touch_time=nba$TOUCH_TIME, defender=nba$CLOSEST_DEFENDER, defender_dist, dribbles=nba$DRIBBLES, success)

summary(nba.1)
rm(list=setdiff(ls(), "nba.1"))



###################################################
## 2 - DATA CLEANSING AND EXPLORATION
###################################################

# 1- final_margin vs victory.
# There are no wrong values in the interaction of these two variables.
# All games that end up with a positive margin must have been won by shooter
par(mfrow=c(1,2))
hist(nba.1$final_margin[nba.1$victory==TRUE], main="Victories", xlab="Magin of victory")
hist(nba.1$final_margin[nba.1$victory==FALSE], main="Losses", xlab="Magin of defeat")

# 2- touch_time
# There are 312 observations with a negative "touch_time", which does not have any sense.
# For the moment, I have decided to change those values to NA and imputed using 1NN.
# In addition, there are some values with "touch_time" higher than 24, which cannot be possible.
# As its values are pretty close to 24, I have rounded them to 24.
par(mfrow=c(1,1))
boxplot(nba.1$touch_time, main="Boxplot of touch_time", xlab="Touch time")

length(which(nba.1$touch_time > 24))
nba.1$touch_time[nba.1$touch_time > 24] <- 24

length(which(nba.1$touch_time < 0))
nba.1$touch_time[nba.1$touch_time < 0] <- NA

aggr_plot <- aggr(nba.1, col=c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

par(mfrow=c(1,2))
boxplot(nba.1$touch_time, main="Boxplot of touch_time", xlab="Touch time")
hist(nba.1$touch_time, main="Histogram of touch_time", xlab="Touch time")

# 3- defender_dist
# There are no wrong values.
boxplot(nba.1$defender_dist, main="Boxplot of defender_dist", xlab="Defender distance (m.)")
hist(nba.1$defender_dist, main="Histogram of defender_dist", xlab="Defender distance (m.)")

# 4- shot_dist vs shot_type
par(mfrow=c(1,1))
ggplot(nba.1, aes(x=shot_dist)) + ggtitle("Shot distance vs Shot type") + geom_density(aes(group=shot_type, colour=shot_type, fill=shot_type), alpha=0.3) + geom_vline(xintercept=7.24)

par(mfrow=c(1,2))
hist(nba.1$shot_dist[nba.1$shot_type==2], main="2-pointers histogram", xlab="Meters")
hist(nba.1$shot_dist[nba.1$shot_type==3], main="3-pointers histogram", xlab="Meters")

length(which(nba.1$shot_dist[nba.1$shot_type==2] >= 23.9))
length(which(nba.1$shot_dist[nba.1$shot_type==3] > 23.9))

# 5- shot_clock vs remaining_seconds
# Weird spike when shot_clock=24, which can be explained easily if we take a look at which players did those kind of shots (https://rpubs.com/BruinRisk/NBA_Shot_Log)
# It is caused by shots done after an offensive rebound (when the clock isn't reset), as they are done by pivots.
# After looking at a few of these, a fan of the game would realize these are all big men playing close to the hoop. Domain knowledge can lead to the conclusion that these are tip-ins/ put-backs where a player near the hoop collects an offensive rebound after a teammate missed a shot. In this situation, the shot clock would reset on hitting the rim!
# In addition, there are 5567 NAs. Most of them occur when game_clock is lower than shot_clock and therefore the shot_clock is turned off, because it has no importance anymore.
# As they are only a 4.34% of the observations, we could delete them from the dataset, but as we don't know which is the goal of this data pre-processing yet, we will handle them later.
# Removing them will make us loose data on significant shots for future anylisis that are not affected by this feature.
par(mfrow=c(1,1))
hist(nba.1$shot_clock, main="Histogram of shot_clock", xlab="Shot clock (s.)") 
summary(nba.1$shot_clock)

nba.1 %>%
  filter(shot_clock > 23) %>%
  group_by(shooter) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

shot_clock_na <- nba.1 %>%
  filter(is.na(shot_clock) == T) %>%
  group_by(remaining_seconds) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(shot_clock_na, aes( x = remaining_seconds, y = count )) + 
  geom_area(stat = 'identity') + 
  coord_cartesian(xlim = c(0,100)) +
  labs(title = 'Distribution of NAs in shot_clock vs remaining_seconds')



###################################################
## 3 - FEATURE CREATION
###################################################
# 1- Shot difficulty (https://stats.nba.com/players/catch-shoot/)
nba.1$shot_difficulty <- 'Open'
nba.1$shot_difficulty[nba.1$defender_dist <= 3.5*0.3048] <- 'Contested'
nba.1$shot_difficulty[nba.1$defender_dist <= 2*0.3048] <- 'Tightly Contested'
nba.1$shot_difficulty[nba.1$defender_dist > 6*0.3048] <- 'Wide Open'
nba.1$shot_difficulty <- as.factor(nba.1$shot_difficulty)

# 2- Shot type
nba.1$shot_cat <- 'Other'
nba.1$shot_cat[nba.1$dribbles <= 1 & nba.1$shot_dist  > 4*0.3048] = 'Catch&Shoot'
nba.1$shot_cat[nba.1$dribbles <= 1 & nba.1$shot_dist <= 4*0.3048] = 'Cut'
nba.1$shot_cat[nba.1$dribbles > 1 & nba.1$shot_dist  <= 4*0.3048] = 'Drive'
nba.1$shot_cat[nba.1$dribbles > 4] = 'ISO/Post up'
nba.1$shot_cat[nba.1$dribbles > 20] = 'Long ISO'
nba.1$shot_cat[nba.1$dribbles <=1 & nba.1$shot_type == 3] = 'Spot Up Three'
nba.1$shot_cat <- as.factor(nba.1$shot_cat)

# 3- Clutch situations
nba.1$clutch <- 0
nba.1$clutch[nba.1$remaining_seconds < 60 & nba.1$period >= 4 & abs(nba.1$final_margin) <= 5] <- 1

save(nba.1, file = "preprocessd_shot_logs.RData")



###################################################
## 4 - MERGE WITH SALARIES DATASET
###################################################
salaries <- data.frame(name=character(), 
                       position=character(), 
                       salary=numeric()) 

url <- "http://www.espn.com/nba/salaries/_/year/2015/page/%d"

for(page in 1:11) {
  webpage <- read_html(sprintf(url, page))
  tmp_salaries <- webpage %>%
    html_nodes(xpath="//table[@class='tablehead']") %>%
    html_table()
  
  tmp_salaries <- tmp_salaries[[1]]
  
  # Remove rows that contain RK in the first column (headers)
  tmp_salaries <- tmp_salaries[tmp_salaries$X1 != 'RK', ]
  
  # Split second column by comma
  tmp_salaries <- extract(tmp_salaries, 'X2', into=c('name', 'position'), '(.*), ([a-zA-Z]*)')
  tmp_salaries$name <- tolower(tmp_salaries$name)
  tmp_salaries$position <- as.factor(tmp_salaries$position)
  
  # Clean last column (salary)
  tmp_salaries$salary <- as.numeric(gsub('[$,]', '', tmp_salaries$X4))
  
  # Add to output DF
  salaries <- rbind(salaries, tmp_salaries[, c(2, 3, 6)])
}

nba.2 <- full_join(nba.1, salaries, by = c("shooter" = "name"))

names1 <- unique(nba.2[is.na(nba.2$salary), 13])
names2 <- nba.2[is.na(nba.2$date), 13]
matches <- amatch(names1, names2, maxDist = 2, nomatch = 0)

for (idx_names1 in 1:length(matches)) {
  idx_names2 <- matches[idx_names1]
  if (idx_names2 > 0) {
    nba.2[nba.2$shooter == names1[idx_names1], c("position", "salary")] <- salaries[salaries$name == names2[idx_names2], c("position", "salary")]
  }
}

nba.2[nba.2$shooter == "otto porter", c("position", "salary")] <- salaries[salaries$name == "otto porter jr.", c("position", "salary")]
nba.2[nba.2$shooter == "jose juan barea", c("position", "salary")] <- salaries[salaries$name == "j.j. barea", c("position", "salary")]
nba.2 <- nba.2[!is.na(nba.2$date) & !is.na(nba.2$salary), ]



###################################################
## 5 - TURN CAT. VARS. INTO CONTINUOUS
###################################################
# As our next step after the pre-processing phase is to apply PCA to our dataset, and this
# technique only takes into account continuous variables, we will turn shot_cat, shot_difficulty 
# and position based on the percentage of success (conditioned to the type of shot) of each 
# categorical variable. Same for the percentage of success in attack/defence of each shooter/defender.
shot.pcts <- nba.2 %>%
  group_by_(.dots=c("shooter","shot_type")) %>% 
  summarize(missed = table(success)[1], made = table(success)[2]) %>%
  replace_na(list(missed = 0, made = 0)) %>%
  mutate(freq = made / (made+missed))

def.pcts <- nba.2 %>%
  group_by_(.dots=c("defender","shot_type")) %>% 
  summarize(missed = table(success)[1], made = table(success)[2]) %>%
  replace_na(list(missed = 0, made = 0)) %>%
  mutate(freq = missed / (made+missed))

shot_cat.pcts <- nba.2 %>%
  group_by_(.dots=c("shot_cat","shot_type")) %>% 
  summarize(missed = table(success)[1], made = table(success)[2]) %>%
  replace_na(list(missed = 0, made = 0)) %>%
  mutate(freq = made / (made+missed))

shot_difficulty.pcts <- nba.2 %>%
  group_by_(.dots=c("shot_difficulty","shot_type")) %>% 
  summarize(missed = table(success)[1], made = table(success)[2]) %>%
  replace_na(list(missed = 0, made = 0)) %>%
  mutate(freq = made / (made+missed))

position.pcts <- nba.2 %>%
  group_by_(.dots=c("position","shot_type")) %>% 
  summarize(missed = table(success)[1], made = table(success)[2]) %>%
  replace_na(list(missed = 0, made = 0)) %>%
  mutate(freq = made / (made+missed))

for (row in 1:nrow(nba.2)) {
  nba.2[row, "shooter_pct"] <- shot.pcts[shot.pcts$shooter == nba.2[row,"shooter"] & shot.pcts$shot_type == nba.2[row,"shot_type"], "freq"]
  nba.2[row, "defender_pct"] <- def.pcts[def.pcts$defender == nba.2[row,"defender"] & def.pcts$shot_type == nba.2[row,"shot_type"], "freq"]
  nba.2[row, "shot_cat_pct"] <- shot_cat.pcts[shot_cat.pcts$shot_cat == nba.2[row,"shot_cat"] & shot_cat.pcts$shot_type == nba.2[row,"shot_type"], "freq"]
  nba.2[row, "shot_difficulty_pct"] <- shot_difficulty.pcts[shot_difficulty.pcts$shot_difficulty == nba.2[row,"shot_difficulty"] & shot_difficulty.pcts$shot_type == nba.2[row,"shot_type"], "freq"]
  nba.2[row, "position_pct"] <- position.pcts[position.pcts$position == nba.2[row,"position"] & position.pcts$shot_type == nba.2[row,"shot_type"], "freq"]
}



###################################################
## 6 - MISSING VALUE HANDLING
###################################################
## NAs in shot_clock
# There are 5567 NAs. Most of them occur when remaining_seconds < shot_clock and therefore the shot_clock is turned off, 
# because it has no importance anymore. These ones will be imputed by a random number from a uniform distribution in the
# range of [0, remaining_seconds]. However, the missing values with remaining_seconds >= shot_clock will be removed.
nba.3 <- filter(nba.2, !is.na(shot_clock) | remaining_seconds < 24)
nba.3[is.na(nba.3$shot_clock), "shot_clock"] <- sapply(nba.3[is.na(nba.3$shot_clock), "remaining_seconds"],
                                                       function(x) round(runif(1, 0, x), digits=1))

## NAs in touch_time
# Create variable whether it is negative or not and apply catdes to see if it is related with certain 
# values in some other feature. Otherwise apply 1 NN.
not_needed_columns <- c("touch_time", "shooter")
result <- catdes(cbind(touch_time_missing=as.factor(is.na(nba.3$touch_time)),
                       nba.3 %>% select(-one_of(not_needed_columns))), 1)
(result$quanti)
# We see in the results that the shots with missing touch_time have a significantly lower and strangely
# exact (0.3) ratio of success compared to the overall mean (0.45), but most importantly, the mean of 
# the number of dribbles before the shot is astonishingly lower than the overall mean (0.007 vs 2.05).
(result$category)
# In the categorical variables we see that the amount of shots of the categories "Cut" and "Catch&Shoot"
# is much higher than the average, as well as the positions "PF" and "C" and the difficulty "Tightly Contested".
# On the other hand, the shot categories "Drive", "ISO/Post up" and "Other" appear fewer times than average
# as well as the position "PG" and the difficulty "Open"

# Seeing this result one interpretation could be that the shots with missing values in touch_time 
# correspond mostly to quick plays where probably this time wasn't measured correctly due to the 
# celerity (swiftness) of the game.

# Options:
#   - Impute random low values following the conclusions extracted after the catdes (maybe too naive)
#   - Impute values using K-NN (with K = 1) and supervise that the values imputed are generally lower 
#     than average, so as to satisfy our conclusions.

# Should we just not use the categorical variables, or should we try to convert them into integers?
# Maybe there are some variables that could be "continuousified" (like shot_difficulty, whose values can
# be ordered), but not all of them (player's names...)
nba.train.touch_time <- nba.3[!is.na(nba.3$touch_time),] %>% 
  select(-one_of("touch_time")) %>%
  select_if(is.numeric)

nba.test.touch_time <- nba.3[is.na(nba.3$touch_time),] %>% 
  select(-one_of("touch_time")) %>%
  select_if(is.numeric)

nba.true_classifications.touch_time <- nba.3$touch_time[!is.na(nba.3$touch_time)]
nba.knn <- knn(nba.train.touch_time, nba.test.touch_time, nba.true_classifications.touch_time)

# The mean is lower in the imputed values, so our hypothesis holds
paste("Mean of the imputed values:", mean(as.numeric(as.character(nba.knn))))
paste("Mean of the original values:", mean(nba.3$touch_time[!is.na(nba.3$touch_time)]))

nba.3$touch_time[is.na(nba.3$touch_time)] <- as.numeric(as.character(nba.knn))

# Turn shot_type (value = 2 or 3) into a binary variable. 1 if it is a triple.
nba.3$triple <- as.numeric(nba.3$shot_type == 3)
nba.3$shot_type <- NULL
nba.3$date <- NULL

nba.3$shot_number <- as.numeric(nba.3$shot_number)
nba.3$period <- as.numeric(nba.3$period)
nba.3$success <- as.factor(nba.3$success)
nba.3$shooter <- as.factor(nba.3$shooter)

save(nba.3, file = "final_shot_logs.RData")
write.csv(nba.3,file = "final_shot_logs.csv")

library(FactoMineR)
library(factoextra)
library(corrplot)
library(caret)
library(calibrate)

set.seed(514124)

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}
nba <- loadRData("data/final_shot_logs.RData")
summary(nba)



nba <- nba.3

###################################################
## 1 - PRINCIPAL COMPONENTS ANALYSIS
###################################################

# PCA
#   - Active variables:        is_home (3), shot_number (6), period (7), remaining_seconds (8), shot_clock (9), shot_dist (10),
#                              touch_time (12), defender_dist(14), dribbles (15), clutch (19), salary (21), shooter_pct (22),
#                              defender_pct (23), shot_cat_pct (24), shot_difficulty (25), position_pct (26), triple (27)
#   - Supplementary variables: shooter (11), defender (13), shot_difficulty (17), shot_cat (18), position (20)
#   - Response variable:       success (16)
# Although victory and final_margin are continuous variables, they are not taken into account as they are known at the end of the match.
# Team codes are useless.
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

# VARIABLE ANALYSIS
# Quality of representation of each variable for each PC (based on the cos2, squared coordinates)
var <- get_pca_var(pca)

corrplot(var$cos2, is.corr=FALSE)
# Representation of variables in the first factorial plane
fviz_pca_var(pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(pca, axes = c(2,3), col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# dimdesc(pca, axes = c(1,2))      'x' must contain finite values only!!! 

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
                invisible = "ind.sup", # SHOULD WE SHOW TEST INDIVIDUALS?
                col.var = "black", # Variables color
                col.ind = "red"  # Individuals color
)


# HIDDEN LATENT FACTORS

# VARIMAX ROTATION

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


# A latent factor is easily interpreted when it just correlates with some variables and it is uncorrelated with the remaining. 
# Principal Components are the factors explaining at most the total variance of the original cloud of points.
# But, not necessarily they clearly identify the underling factors existing in your data.

# In our case though, the better explained variables clearly correlate with the principal components, and therefore no rotation of
# the dimensions is needed. Interpret principal components...




# CLUSTERING OF LARGE DATA SETS
# FIRST 2 KMEANS WITH K=14


## Decide whether to use the original cloud of individuals or the rotated one. 

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








library(randomForest)
library(rpart.plot)
library(rpart)
library(dplyr)
library(caret)
library(ROCR)

set.seed(123)
load(file="data/train.RData")
load(file="data/test.RData")

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

