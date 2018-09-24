#################################
# Final Project                 #
# MATH 4990                     #
# Iurii Shamkin (T00036016)     #
#################################

#######################################
#### Data preparation - Moment app ####
#######################################

library("rjson")
# loading JSON
moment_data = fromJSON(file="moment_data.json")
# binding into dataframe
moment_dataframe = as.data.frame(do.call("rbind", moment_data))

# we got 91 days of observations
dim(moment_dataframe)

# we have dataframe of lists
sapply(moment_dataframe, class)

# converting into actual dataframe
moment_df = data.frame(matrix(unlist(moment_dataframe), nrow=nrow(moment_dataframe)),  stringsAsFactors=FALSE)
# copying column names
colnames(moment_df) = colnames(moment_dataframe)

#all columns are characters, have to convert to numeric
str(moment_df)
moment_df$minuteCount = as.numeric(as.character(moment_df$minuteCount))
moment_df$pickupCount = as.numeric(as.character(moment_df$pickupCount))

# we don't have missing values in our dataframe
complete_rows_ration = nrow(moment_df)/sum(complete.cases(moment_df))

# we have suspicious outliers
summary(moment_df)

library(dplyr)
#Creating a data frame with only the outliers
outlier <- moment_df %>% filter(pickupCount == 0)
outlier

#Removing the observations with sentinel value
moment_df <- moment_df %>% filter(pickupCount != 0)

library(ggplot2)
#Plotting numerical variables distributions
pl1 <- ggplot(moment_df, aes(minuteCount))
pl1 + geom_density(fill = "red", alpha = "0.7")

pl2 <- ggplot(moment_df, aes(pickupCount))
pl2 + geom_density(fill = "red", alpha = "0.7")




###################################
#### Data preparation - Garmin ####
###################################

garmin_df = read.csv(file="garmin_data.csv",sep=",", header = T)

dim(garmin_df)
sapply(garmin_df, class)
str(garmin_df)

# many NA's
summary(garmin_df)

# we have about 26% incomplete rows 
missing_rows_percent = (1 - sum(complete.cases(garmin_df))/nrow(garmin_df))*100


# using mean to replace the missing values in totalSteps variable
meanValue = mean(garmin_df$totalSteps[!is.na(garmin_df$totalSteps)])
garmin_df$totalSteps[is.na(garmin_df$totalSteps)] = meanValue

# using mean to replace the missing values in restingHeartRate variable
meanValue = mean(garmin_df$restingHeartRate[!is.na(garmin_df$restingHeartRate)])
garmin_df$restingHeartRate[is.na(garmin_df$restingHeartRate)] = meanValue

# using mean to replace the missing values in stressLevel variable
meanValue = mean(garmin_df$stressLevel[!is.na(garmin_df$stressLevel)])
garmin_df$stressLevel[is.na(garmin_df$stressLevel)] = meanValue


#Using linear regression to replace NA in deepSleep variable
filteredData = garmin_df %>% filter(!is.na(deepSleep))
deepSleep_model = lm(deepSleep ~ totalSteps + restingHeartRate + stressLevel + lightSleep + awakeSleep, data = filteredData)
summary(deepSleep_model)

#Using log linear regression to replace NA in deepSleep variable
filteredData = garmin_df %>% filter(!is.na(deepSleep))
deepSleep_model = lm(log(deepSleep) ~ totalSteps + restingHeartRate + stressLevel + lightSleep + awakeSleep, data = filteredData)
summary(deepSleep_model)

#Using glm to replace NA in deepSleep variable
filteredData = garmin_df %>% filter(!is.na(deepSleep))
deepSleep_model = glm(deepSleep ~ totalSteps + restingHeartRate + stressLevel + lightSleep + awakeSleep, data = filteredData, family = poisson)
summary(deepSleep_model)
garmin_df$deepSleep[is.na(garmin_df$deepSleep)] = predict(deepSleep_model, newdata = garmin_df[is.na(garmin_df$deepSleep),])

summary(garmin_df)

#Plotting numerical variables distributions
plTotalSteps <- ggplot(garmin_df, aes(totalSteps))
plTotalSteps + geom_density(fill = "red", alpha = "0.7")

plRestingHeartRate <- ggplot(garmin_df, aes(restingHeartRate))
plRestingHeartRate + geom_density(fill = "red", alpha = "0.7")

plStressLevel <- ggplot(garmin_df, aes(stressLevel))
plStressLevel + geom_density(fill = "red", alpha = "0.7")

plDeepSleep <- ggplot(garmin_df, aes(deepSleep))
plDeepSleep + geom_density(fill = "red", alpha = "0.7")

plLightSleep <- ggplot(garmin_df, aes(lightSleep))
plLightSleep + geom_density(fill = "red", alpha = "0.7")

plAwakeSleep <- ggplot(garmin_df, aes(awakeSleep))
plAwakeSleep + geom_density(fill = "red", alpha = "0.7")



# parallel-coordinate plot
blueOrange = colorRampPalette(c("orange","blue")) 
theColours = blueOrange(length(garmin_df[,3]))
coloursToPaint = theColours[rank(garmin_df[,3])]

library(MASS)

parcoord(
  garmin_df[, c(3,2,1,4,5,6)],
  col = coloursToPaint,
  var.label = T
)

# colorelss version
parcoord(
  garmin_df[, c(3,2,1,4,5,6)],
  var.label = T
)

# combining all sleep measurments together
garmin_df$totalSleep = garmin_df$deepSleep + garmin_df$lightSleep + garmin_df$awakeSleep

parcoord(
  garmin_df[, c(3,2,1,8)],
  col = coloursToPaint,
  var.label = T
)




####################################################
#### Data preparation - Combining both datasets ####
####################################################

combined_df = cbind(garmin_df,moment_df)
combined_df = combined_df[, !(names(combined_df) %in% c("date"))]
combined_df = combined_df[, c(1,2,3,4,5,6,8,9,10,7)]

filteredData = combined_df %>% filter(!is.na(deepSleep))

#Using log linear regression to replace NA in deepSleep variable
deepSleep_model = lm(totalSleep ~ totalSteps + restingHeartRate + stressLevel + minuteCount + pickupCount, data = filteredData)
summary(deepSleep_model)

#Using glm to replace NA in deepSleep variable
deepSleep_model = glm(deepSleep ~ totalSteps + restingHeartRate + stressLevel + minuteCount + pickupCount, data = filteredData)
summary(deepSleep_model)




##################################
#### Regression Tree Analysis ####
##################################

filteredData = combined_df %>% filter(!is.na(deepSleep))
rt = rpart(formula= stressLevel~., data = filteredData[, c(1,2,3,4,5,6,8,9)])
prettyTree(rt, fwidth = 0.2, fheight = 0.1, cex = 0.8)

rt = rpart(formula= minuteCount~., data = filteredData[, c(1,2,3,7,8)])
prettyTree(rt, fwidth = 0.2, fheight = 0.1, cex = 0.8)
plotcp(rt)

rt = rpart(formula= pickupCount~., data = filteredData[, c(1,2,3,7,9)])
prettyTree(rt, fwidth = 0.2, fheight = 0.1, cex = 0.8)
plotcp(rt)
printcp(rt)



#####################
#### MDS & t-SNE ####
#####################

# source: https://distill.pub/2016/misread-tsne/

library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot

filteredData = combined_df %>% filter(!is.na(deepSleep))
col <- sort(rnorm(66))

# Classical MDS considering separate sleep stages
distances = dist(filteredData[, c(1,2,3,4,5,6,8,9)])
mds = cmdscale(distances)
colnames(mds) = c('x','y')
qplot(x, y, data=as.data.frame(mds), colour=col) + scale_colour_gradient(low="orange", high="blue") + geom_point() + geom_text(aes(label=filteredData$observationDate))

# Classical MDS considering only totalSleep
distances = dist(filteredData[, c(1,2,3,7,8,9)])
summary(distances)
mds = cmdscale(distances)
colnames(mds) = c('x','y')
qplot(x, y, data=as.data.frame(mds), colour=col) + scale_colour_gradient(low="orange", high="blue") + geom_point() + geom_text(aes(label=filteredData$observationDate))


# t-SNE dimesnion reduction
tsne_out = Rtsne(filteredData[, c(1,2,3,7,8,9)], perplexity = 15, theta = 0.0, eta = 200, max_iter = 5000, verbose = TRUE)

plot(tsne_out$Y, asp = 1, pch = 20, col = "blue", 
     cex = 1.15, cex.axis = 1.25, cex.lab = 1.25, cex.main = 1.5, 
     xlab = "t-SNE dimension 1", ylab = "t-SNE dimension 2", 
     main = "2D t-SNE projection")



##############################################
#### PAM Clustering & its Visualizations #####
##############################################

# Calculate silhouette width for many k using PAM
distances = dist(filteredData[, c(1,2,3,4,5,6,8,9)]) 
#distances = dist(filteredData[, c(1,2,3,7,8,9)]) 

sil_width <- c(NA)

for(i in 2:20){
  pam_fit <- pam(distances, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width (higher is better)
plot(1:20, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:20, sil_width)


# PAM clustering
pam_fit <- pam(distances, diss = TRUE, k = 8)

# t-SNE dimesnion reduction
tsne_obj <- Rtsne(distances, is_distance = TRUE, perplexity = 15, theta = 0.0, eta = 200, max_iter = 5000, verbose = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(Cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = Cluster), size = 2.5)

# classical MDS
another_tsne_data <- mds %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(Cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data=another_tsne_data) + geom_point(aes(color = Cluster), size = 5)

ggplot(aes(x = X, y = Y), data=another_tsne_data) + geom_point(aes(color = Cluster), size = 2.5) + geom_text(aes(label=filteredData$observationDate), nudge_y = -9)




#########################################################
#### Linear Regression, Log-linear Regression and    ####
#### Random Forest - Evaluation of Predictive Models ####
#########################################################

#filteredData[, c(1,2,3,4,5,6,8,9)]
#filteredData[, c(1,2,3,7,8,9)]

data = filteredData[, c(1,2,3,4,5,6,8,9)]

set.seed(101)

for (i in 1:100) {
  
  # randomly pick 70% of the number of observations
  index <- sample(1:nrow(data),size = 0.7*nrow(data)) 
  # subset to include only the elements in the index
  train <- data[index,] 
  # subset to include all but the elements in the index
  test <- data[-index,]
  
  # linear model
  lm.model <- lm(stressLevel ~ totalSteps + restingHeartRate + deepSleep + lightSleep + awakeSleep + pickupCount + minuteCount, data = train)
  lm.test <- predict(lm.model, test)
  RMSE.lm = sqrt(mean((lm.test - test$stressLevel)^2))
  MAE.lm = mean(abs(lm.test - test$stressLevel))
  
  # log linear model
  loglm.model = lm(log(stressLevel) ~ totalSteps + restingHeartRate + deepSleep + lightSleep + awakeSleep + pickupCount + minuteCount, data = train)
  loglm.test <- exp(predict(loglm.model, test))
  RMSE.loglm = sqrt(mean((loglm.test - test$stressLevel)^2))
  MAE.loglm = mean(abs(loglm.test - test$stressLevel))
  
  # random forest
  rf.model <- randomForest(stressLevel ~ totalSteps + restingHeartRate + deepSleep + lightSleep + awakeSleep + pickupCount + minuteCount, data = train, ntree = 10000)
  rf.test <- predict(rf.model, test)
  RMSE.rf = sqrt(mean((rf.test - test$stressLevel)^2))
  MAE.rf = mean(abs(rf.test - test$stressLevel))
}

# Create a data frame with the error metrics for each method
accuracy <- data.frame(Method = c("Linear Regression", "Log Linear Regression", "Random Forest"), RMSE = c(RMSE.lm, RMSE.loglm, RMSE.rf), MAE = c(MAE.lm, MAE.loglm, MAE.rf)) 

# Round the values and print the table
accuracy$RMSE <- round(accuracy$RMSE,2)
accuracy$MAE <- round(accuracy$MAE,2) 

accuracy