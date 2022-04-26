library(tidyverse)
library(ggplot2)
library(GGally)

library(VIM)
library(mice)
library(caret)
library(factoextra)
library(randomForest)
library(ggcorrplot)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(dplyr)

#setwd('')
filename <- "data/passenger_satisfaction.csv"
df <- read_csv(filename)

df <- read_csv(filename, col_types = cols(gender = col_factor(), 
                                          customer_type = col_factor(), 
                                          travel_type = col_factor(),
                                          class = col_factor(),
                                          travel_type = col_factor(),
                                          satisfaction = col_factor(),
                                          wifi_service = col_factor(),
                                          departure_arrival_time_convenient = col_factor(),
                                          online_booking_ease = col_factor(),
                                          gate_location = col_factor(),
                                          food_drink = col_factor(),
                                          online_boarding = col_factor(),
                                          seat_comfort = col_factor(),
                                          inflight_entertainment = col_factor(),
                                          onboard_service = col_factor(),
                                          legroom_service = col_factor(),
                                          baggage_handling = col_factor(),
                                          checkin_service = col_factor(),
                                          inflight_service = col_factor(),
                                          cleanliness = col_factor(),
                                          departure_delay = col_factor(),
                                          arrival_delay = col_factor()
                                          ))

df <- read_csv(filename, col_types = cols(gender = col_factor(), 
                                          customer_type = col_factor(), 
                                          travel_type = col_factor(),
                                          class = col_factor(),
                                          travel_type = col_factor(),
                                          satisfaction = col_factor()
))


summary(df)
str(df)

# gender chart
ggplot(df, aes(x=gender, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Gender", y = "Proportion")

# customer type chart
ggplot(df, aes(x=customer_type, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Customer Types", y = "Proportion")

# travel type chart
ggplot(df, aes(x=travel_type, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Travel Type", y = "Proportion")

# class chart
ggplot(df, aes(x=class, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Class", y = "Proportion")

# class chart
ggplot(df, aes(x=flight_distance)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Flight Distance", y = "Proportion")

# satisfaction chart
ggplot(df, aes(x=satisfaction, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Overall Satisfaction", y = "Proportion")

# wifi service chart
ggplot(df, aes(x=wifi_service, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Wifi Service Satisfaction", y = "Proportion") +
  scale_x_continuous("Wifi Service Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# Departure/Arrival time chart
ggplot(df, aes(x=departure_arrival_time_convenient, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Departure/Arrival Time Convenience", y = "Proportion") +
  scale_x_continuous("Departure/Arrival Time Convenience", breaks = c(0, 1, 2, 3, 4, 5))

# online booking chart
ggplot(df, aes(x=online_booking_ease, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Online Booking Ease", y = "Proportion") +
  scale_x_continuous("Online Booking Ease", breaks = c(0, 1, 2, 3, 4, 5))

# gate location chart
ggplot(df, aes(x=gate_location, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Gate Location Satisfaction", y = "Proportion") +
  scale_x_continuous("Gate Location Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# food drink chart
ggplot(df, aes(x=food_drink, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Food & Drink Satisfaction", y = "Proportion") +
  scale_x_continuous("Food & Drink Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# online boarding chart
ggplot(df, aes(x=online_boarding, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Online Boarding Satisfaction", y = "Proportion") +
  scale_x_continuous("Online Boarding Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# seat comfort chart
ggplot(df, aes(x=seat_comfort, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Seat Comfort Satisfaction", y = "Proportion") +
  scale_x_continuous("Seat Comfort Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# inflight entertainment chart
ggplot(df, aes(x=inflight_entertainment, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Inflight Entertainment Satisfaction", y = "Proportion") +
  scale_x_continuous("Inflight Entertainment Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# onboard service chart
ggplot(df, aes(x=onboard_service, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Onboard Service Satisfaction", y = "Proportion") +
  scale_x_continuous("Onboard Service Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# legroom_service chart
ggplot(df, aes(x=legroom_service, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Legroom Satisfaction", y = "Proportion") +
  scale_x_continuous("Legroom Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# baggage handling chart
ggplot(df, aes(x=baggage_handling, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Baggage Handling Satisfaction", y = "Proportion") +
  scale_x_continuous("Baggage Handling Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# checkin chart
ggplot(df, aes(x=checkin_service, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Checkin Service Satisfaction", y = "Proportion") +
  scale_x_continuous("Checkin Service Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# inflight service chart
ggplot(df, aes(x=inflight_service, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Inflight Service Satisfaction", y = "Proportion") +
  scale_x_continuous("Inflight Service Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# cleanliness chart
ggplot(df, aes(x=cleanliness, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Cleanliness Satisfaction", y = "Proportion") +
  scale_x_continuous("Cleanliness Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# departure delay chart
ggplot(df, aes(x=departure_delay, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Departure Delay Satisfaction", y = "Proportion") +
  scale_x_continuous("Departure Delay Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

# arrival delay chart
ggplot(df, aes(x=arrival_delay, y = stat(count / sum(count)))) +
  geom_bar() +
  labs(x = "Arrival Delay Satisfaction", y = "Proportion") +
  scale_x_continuous("Arrival Delay Satisfaction", breaks = c(0, 1, 2, 3, 4, 5))

ggpairs(df)

sd(df$flight_distance)
sd(df$departure_delay)

arrival_delay <- drop_na(df)

sd(arrival_delay$arrival_delay)

# Travis W.

# Set the random seed
set.seed(100)

summary(df)
str(df)
head(df)

# Handle Missing Data (MICE)
ar_delay_avg <- mean(df$arrival_delay, na.rm = TRUE)

md.pattern(df, rotate.names = TRUE)

aggr_plot <- aggr(df, col=c("forestgreen","darkgoldenrod1"), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

mice_res <- mice(df, method = 'rf', maxit= 10)

summary(mice_res$data)
imp_df <- complete(mice_res,1)
summary(imp_df)

xyplot(mice_res,arrival_delay ~ departure_delay,pch=18,cex=1)

# Data Plots 
ggplot(imp_df, aes(x=reorder(satisfaction, satisfaction, function(x)-length(x)))) +
  geom_bar(fill=c("forestgreen","darkgoldenrod1")) +  labs(x='Satisfaction')

# Stacked
ggplot(imp_df, aes(fill=cleanliness, x=satisfaction)) + 
  geom_bar(position="stack")

ggplot(imp_df, aes(fill=gender, x=satisfaction)) + 
  geom_bar(position="stack")

ggplot(imp_df, aes(fill=class, x=satisfaction)) + 
  geom_bar(position="stack")


ggplot(imp_df, aes(fill=checkin_service, x=satisfaction)) + 
  geom_bar(position="stack")

ggplot(imp_df, aes(x=satisfaction, y=departure_delay)) +  geom_boxplot(fill=c("forestgreen","darkgoldenrod1"))

ggplot(imp_df, aes(x=satisfaction, y=flight_distance)) +  geom_boxplot(fill=c("forestgreen","darkgoldenrod1"))


chisq.test(imp_df$travel_type,imp_df$satisfaction)

numeric_df <- dplyr::select_if(imp_df, is.numeric)

r <- cor(numeric_df,use="complete.obs")

ggcorrplot(r)

# Models


# Dimension Reduction 
df.train.numeric <- imp_df[22:23]
pca <- prcomp(df.train.numeric,center=TRUE,scale.=TRUE)
print(pca)
plot(pca)
summary(pca)

fviz_eig(pca)
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

pred_pca <- predict(pca,newdata = df.train.numeric)

df.pca <- cbind.data.frame(imp_df[,c(2:21,24)],pred_pca[,1])
colnames(df.pca)[22] <- "delay_pca"
# Standard Partition with imputed data
trainIndex <-
  createDataPartition(imp_df$satisfaction,
                      p = .7,
                      list = FALSE,
                      times = 1)

df.train <- imp_df[trainIndex, ]
df.valid <- imp_df[-trainIndex, ]
# PCA Partition with Imputed data
trainIndex <-
  createDataPartition(df.pca$satisfaction,
                      p = .7,
                      list = FALSE,
                      times = 1)

pca.train <- df.pca[trainIndex, ]
pca.valid <- df.pca[-trainIndex, ]

# Logistic Regression
base.model <- train(satisfaction~. -id
                    ,data = df.train
                    ,method = "glm"
                    ,family = "binomial")
summary(base.model)
pca.base.model <- train(satisfaction~.
                        ,data = pca.train
                        ,method = "glm"
                        ,family = "binomial")
summary(pca.base.model)


# Score base model
prediction <-
  predict(base.model, newdata = df.valid, na.action = na.pass)
confusionMatrix(prediction, df.valid$satisfaction)

# Score pca base model
prediction <-
  predict(pca.base.model, newdata = pca.valid, na.action = na.pass)
confusionMatrix(prediction, pca.valid$satisfaction)

trControl <- trainControl(method = 'cv',
                          number = 10,
                          search = 'grid')

# Random Forest Model
tuneGrid <- expand.grid(.mtry = c(1:15))
rf <- train(
  satisfaction~.,
  data = pca.train,
  method = "rf",
  metric = "Accuracy",
  tuneGrid = tuneGrid,
  trControl = trControl,
  ntree = 300
)

rf
prediction <-
  predict(rf, newdata = pca.valid, na.action = na.pass)
confusionMatrix(prediction, pca.valid$satisfaction)


# SVM Model
# grid <- expand.grid(sigma = seq(0,2,by=.25),C = c(0.005,0.01,0.05, 0.25, 0.5, 0.75, 1))
grid <- expand.grid(C = c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 1))

svm <- train(
  satisfaction ~ .,
  data = pca.train,
  method = 'svmLinear',
  trControl = trControl,
  #preProcess = c('center', 'scale'),
  tuneGrid = grid
)

svm
prediction <-
  predict(rf, newdata = pca.valid, na.action = na.pass)
confusionMatrix(prediction, pca.valid$satisfaction)

