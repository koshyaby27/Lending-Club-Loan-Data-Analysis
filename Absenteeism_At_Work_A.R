#Clear R environment of all the variables
rm(list = ls())

## Set working directory where my data is stored in the system
setwd("C:/Users/akoshy/Downloads/Course Project")

####Load required libraries
required_lib = c("inTrees","varhandle","e1071","dummies","corrgram","caret","lsr","rpart.plot","plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","DataCombine","xlsx","rJava","gridExtra","corrplot","tree","caret")
install.packages(required_lib)
lapply(required_lib,require,character.only = TRUE)

#load data 
EmployeeData = read.csv("Absenteeism_at_work_train.csv")
View(EmployeeData)

###################### Exploratory Data Analysis ##################
# Finding the variable types
cnames <- colnames(EmployeeData)

# Check the number of unique variables
for (i in cnames){
  print(i)
  print(aggregate(data.frame(count = EmployeeData[,i]), 
                  list(value = EmployeeData[,i]), length))
}

# Data Preprocessing 
preprocessing <- function(EmployeeData){
  EmployeeData$ID <- as.factor(EmployeeData$ID)
  for (i in (1:nrow(EmployeeData))){
    if (EmployeeData$Absenteeism.time.in.hours[i] != 0 || is.na(EmployeeData$Absenteeism.time.in.hours[i])){
      if(EmployeeData$Reason.for.absence[i] == 0 || is.na(EmployeeData$Reason.for.absence[i])){
        EmployeeData$Reason.for.absence[i] = NA
      }
      if(EmployeeData$Month.of.absence[i] == 0 || is.na(EmployeeData$Month.of.absence[i])){
        EmployeeData$Month.of.absence[i] = NA
      }
    }
  }
  EmployeeData$Reason.for.absence <- as.factor(EmployeeData$Reason.for.absence)
  EmployeeData$Month.of.absence <- as.factor(EmployeeData$Month.of.absence)
  EmployeeData$Day.of.the.week <- as.factor(EmployeeData$Day.of.the.week)
  EmployeeData$Seasons <- as.factor(EmployeeData$Seasons)
  EmployeeData$Disciplinary.failure <- as.factor(EmployeeData$Disciplinary.failure)
  EmployeeData$Education <- as.factor(EmployeeData$Education)
  EmployeeData$Son <- as.factor(EmployeeData$Son)
  EmployeeData$Social.drinker <- as.factor(EmployeeData$Social.drinker)
  EmployeeData$Social.smoker <- as.factor(EmployeeData$Social.smoker)
  EmployeeData$Pet <- as.factor(EmployeeData$Pet)
  return(EmployeeData)
}
EmployeeData <- preprocessing(EmployeeData)

#Selecting only the factor
get_cat <- function(data) {
  return(colnames(data[,sapply(data, is.factor)]))
}
cat_cnames <- get_cat(EmployeeData)

#Selecting only the numeric
get_num <- function(data) {
  return(colnames(data[,sapply(data, is.numeric)]))
}
num_cnames <- get_num(EmployeeData)

#Coverting factor varaible values to labels 
for (i in cat_cnames){
  EmployeeData[,i] <- factor(EmployeeData[,i], 
                             labels = (1:length(levels(factor(EmployeeData[,i])))))
}

############################### Data PreProcessing #########################
# Analysing Missing Values

missingValueCheck <- function(data){
  for (i in colnames(data)){
    print(i)
    print(sum(is.na(data[i])))
  }
  print("Total")
  print(sum(is.na(EmployeeData)))
}
missingValueCheck(EmployeeData)

# Impute values related to ID
Depenedent_ID <- c("ID","Transportation.expense","Service.time","Age","Height",
                   "Distance.from.Residence.to.Work","Education","Son","Weight",
                   "Social.smoker","Social.drinker","Pet","Body.mass.index")
Depenedent_ID_data <- EmployeeData[,Depenedent_ID]
Depenedent_ID_data <- aggregate(. ~ ID, data = Depenedent_ID_data, 
                                FUN = function(e) c(x = mean(e)))
for (i in Depenedent_ID) {
  for (j in (1:nrow(EmployeeData))){
    ID <- EmployeeData[j,"ID"]
    if(is.na(EmployeeData[j,i])){
      EmployeeData[j,i] <- Depenedent_ID_data[ID,i]
    }
  }
}

# Impute values for other variables
EmployeeData = knnImputation(EmployeeData, k = 7)
missingValueCheck(EmployeeData)

# Analysing outliers
# Histogram
for(i in num_cnames){
  hist(EmployeeData[,i], xlab=i, main=" ", col=(c("lightblue","darkgreen")))
}

# BoxPlot
num_cnames <- num_cnames[ num_cnames != "Absenteeism.time.in.hours"]
for(i in num_cnames){
  boxplot(EmployeeData[,i]~EmployeeData$Absenteeism.time.in.hours,
          data=EmployeeData, main=" ", ylab=i, xlab="Absenteeism.time.in.hours",  
          col=(c("lightblue","darkgreen")), outcol="red")
}

# Removing ID related varaibles from num_cnames
for (i in Depenedent_ID){
  num_cnames <- num_cnames[ num_cnames != i]
}

# Replace all outliers with NA and impute
for(i in num_cnames){
  val = EmployeeData[,i][EmployeeData[,i] %in% boxplot.stats(EmployeeData[,i])$out]
  EmployeeData[,i][EmployeeData[,i] %in% val] = NA
}

# Impute Missing Values
missingValueCheck(EmployeeData)
EmployeeData <- knnImputation(EmployeeData, k = 7)

# Copy Employee Data into DataSet for backup
DataSet <- EmployeeData

######################## Fature Selection #################
# Correlation Plot
corrgram(EmployeeData, upper.panel=panel.pie, text.panel=panel.txt, 
         main = "Correlation Plot")

# ANOVA
cnames <- colnames(EmployeeData)
for (i in cnames){
  print(i)
  print(summary(aov(EmployeeData$Absenteeism.time.in.hours~EmployeeData[,i], 
                    EmployeeData)))
}

# Dimensionality Reduction
EmployeeData <- subset(EmployeeData, select = -c(Weight,Education,Service.time,
                                                 Social.smoker,Body.mass.index,
                                                 Work.load.Average.day,Seasons,
                                                 Transportation.expense,Pet,
                                                 Disciplinary.failure,Hit.target,
                                                 Month.of.absence,Social.drinker))

######################### Feature Normalization ######################
cat_cnames <- get_cat(EmployeeData)
num_cnames <- get_num(EmployeeData)

# Feature Scaling
for (i in num_cnames){
  EmployeeData[,i] <- (EmployeeData[,i] - min(EmployeeData[,i])) / 
    (max(EmployeeData[,i]) - min(EmployeeData[,i]))
}



################## Preprocessing the test data ##################
EmployeeData_test = read.csv("Absenteeism_at_work_test.csv")
View(EmployeeData_test)

cnamest <- colnames(EmployeeData_test)
for (i in cnamest){
  print(i)
  print(aggregate(data.frame(count = EmployeeData_test[,i]), 
                  list(value = EmployeeData_test[,i]), length))
}

preprocessing <- function(EmployeeData_test){
  EmployeeData_test$ID <- as.factor(EmployeeData_test$ID)
  for (i in (1:nrow(EmployeeData_test))){
    if (EmployeeData_test$Absenteeism.time.in.hours[i] != 0 || is.na(EmployeeData_test$Absenteeism.time.in.hours[i])){
      if(EmployeeData_test$Reason.for.absence[i] == 0 || is.na(EmployeeData_test$Reason.for.absence[i])){
        EmployeeData_test$Reason.for.absence[i] = NA
      }
      if(EmployeeData_test$Month.of.absence[i] == 0 || is.na(EmployeeData_test$Month.of.absence[i])){
        EmployeeData_test$Month.of.absence[i] = NA
      }
    }
  }
  EmployeeData_test$Reason.for.absence <- as.factor(EmployeeData_test$Reason.for.absence)
  EmployeeData_test$Month.of.absence <- as.factor(EmployeeData_test$Month.of.absence)
  EmployeeData_test$Day.of.the.week <- as.factor(EmployeeData_test$Day.of.the.week)
  EmployeeData_test$Seasons <- as.factor(EmployeeData_test$Seasons)
  EmployeeData_test$Disciplinary.failure <- as.factor(EmployeeData_test$Disciplinary.failure)
  EmployeeData_test$Education <- as.factor(EmployeeData_test$Education)
  EmployeeData_test$Son <- as.factor(EmployeeData_test$Son)
  EmployeeData_test$Social.drinker <- as.factor(EmployeeData_test$Social.drinker)
  EmployeeData_test$Social.smoker <- as.factor(EmployeeData_test$Social.smoker)
  EmployeeData_test$Pet <- as.factor(EmployeeData_test$Pet)
  return(EmployeeData_test)
}
EmployeeData_test <- preprocessing(EmployeeData_test)

get_catt <- function(data) {
  return(colnames(data[,sapply(data, is.factor)]))
}
cat_cnamest <- get_catt(EmployeeData_test)

get_numt <- function(data) {
  return(colnames(data[,sapply(data, is.numeric)]))
}
num_cnamest <- get_numt(EmployeeData_test)

for (i in cat_cnamest){
  EmployeeData_test[,i] <- factor(EmployeeData_test[,i], 
                                  labels = (1:length(levels(factor(EmployeeData_test[,i])))))
}

missingValueCheck <- function(data){
  for (i in colnames(data)){
    print(i)
    print(sum(is.na(data[i])))
  }
  print("Total")
  print(sum(is.na(EmployeeData_test)))
}
missingValueCheck(EmployeeData_test)

Depenedent_ID_t <- c("ID","Transportation.expense","Service.time","Age","Height",
                     "Distance.from.Residence.to.Work","Education","Son","Weight",
                     "Social.smoker","Social.drinker","Pet","Body.mass.index")
Depenedent_ID_t_data <- EmployeeData_test[,Depenedent_ID_t]
Depenedent_ID_t_data <- aggregate(. ~ ID, data = Depenedent_ID_t_data, 
                                  FUN = function(e) c(x = mean(e)))
for (i in Depenedent_ID_t) {
  for (j in (1:nrow(EmployeeData_test))){
    ID <- EmployeeData_test[j,"ID"]
    if(is.na(EmployeeData_test[j,i])){
      EmployeeData_test[j,i] <- Depenedent_ID_t_data[ID,i]
    }
  }
}

EmployeeData_test = knnImputation(EmployeeData_test, k = 7)
missingValueCheck(EmployeeData_test)

for (i in Depenedent_ID_t){
  num_cnamest <- num_cnamest[ num_cnamest != i]
}

for(i in num_cnamest){
  val = EmployeeData_test[,i][EmployeeData_test[,i] %in% boxplot.stats(EmployeeData_test[,i])$out]
  EmployeeData_test[,i][EmployeeData_test[,i] %in% val] = NA
}


missingValueCheck(EmployeeData_test)
EmployeeData_test <- knnImputation(EmployeeData_test, k = 7)

DataSet_test <- EmployeeData_test

cnamest <- colnames(EmployeeData_test)
for (i in cnamest){
  print(i)
  print(summary(aov(EmployeeData_test$Absenteeism.time.in.hours~EmployeeData_test[,i], 
                    EmployeeData_test)))
}

EmployeeData_test <- subset(EmployeeData_test, select = -c(Weight,Education,Service.time,
                                                           Social.smoker,Body.mass.index,
                                                           Work.load.Average.day,Seasons,
                                                           Transportation.expense,Pet,
                                                           Disciplinary.failure,Hit.target,
                                                           Month.of.absence,Social.drinker))

cat_cnamest <- get_catt(EmployeeData_test)
num_cnamest <- get_numt(EmployeeData_test)

for (i in num_cnamest){
  EmployeeData_test[,i] <- (EmployeeData_test[,i] - min(EmployeeData_test[,i])) / 
    (max(EmployeeData_test[,i]) - min(EmployeeData_test[,i]))
}


##########################################################

set.seed(1)
train_index = sample(1:nrow(EmployeeData),)
test_index = sample(1:nrow(EmployeeData_test),)

train = EmployeeData[train_index,]
train = unfactor(train)
test = EmployeeData_test[test_index,]
test = unfactor(test)


########################################DECISION TREE########################################


#Build decsion tree using rpart
dt_model = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")

#Plot the tree
rpart.plot(dt_model)

#Perdict for test cases
dt_predictions = predict(dt_model, test)

#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test, "dt_pred"=dt_predictions)
head(df_pred)

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")


########################################RANDOM FOREST########################################

names(train) <- make.names(names(train))
names(test) <- make.names(names(test))

##Train the model using training data

rf_model = randomForest(Absenteeism.time.in.hours ~ ., data = train, ntree=100)

#Predict the test cases
rf_predictions = predict(rf_model, test)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)


#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

########################################LINEAR REGRESSION########################################

##Train the model using training data
lr_model = lm(formula = Absenteeism.time.in.hours~., data = train)

#Get the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model, test)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)
head(df_pred)


#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

########################################DIMENSION REDUCTION USING PCA########################################
#Principal component analysis
prin_comp = prcomp(train)

#Compute standard deviation of each principal component
pr_stdev = prin_comp$sdev

#Compute variance
pr_var = pr_stdev^2

#Proportion of variance explained
prop_var = pr_var/sum(pr_var)

#Cumulative scree plot
plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#Add a training set with principal components
train.data = data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, prin_comp$x)


#Transform test data into PCA
test.data = predict(prin_comp, newdata = test)
test.data = as.data.frame(test.data)


########################################DECISION TREE########################################

#Build decsion tree using rpart
dt_model = rpart(Absenteeism.time.in.hours ~., data = train.data, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model,test.data)

#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test, "dt_pred"=dt_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")

########################################RANDOM FOREST########################################

#Train the model using training data
rf_model = randomForest(Absenteeism.time.in.hours~., data = train.data, ntrees = 500)

#Predict the test cases
rf_predictions = predict(rf_model,test.data)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

########################################LINEAR REGRESSION########################################

#Train the model using training data
lr_model = lm(Absenteeism.time.in.hours ~ ., data = train.data)

#Get the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model,test.data)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs =test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

