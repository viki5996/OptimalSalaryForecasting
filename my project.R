setwd("C:/Users/Vikram Arikath/Desktop/Internship/")
data = read.csv("Mith.csv", sep = ",", header = T, na.strings = " ")

str(data)
library(stringr)
library(lubridate)
install.packages("rockchalk")
library(rockchalk)
library(plyr)
#install.packages("mice")
library(mice)
install.packages("DMwR")
library(missForest)
library(caret)
library(rpart)
library(DMwR)
#install.packages("missRanger")
library(missRanger)
library(corrplot)
library(dplyr)



sum(is.na(data))
str(data)
summary(data)





data$Date.Of.Birth = as.character(data$Date.Of.Birth)
data$D.O.B = str_sub(data$Date.Of.Birth, 1,-6)
data$Birth.Time = str_sub(data$Date.Of.Birth, start = -5)

data$D.O.B = as_date(data$D.O.B)

data$Y.O.B = year(data$D.O.B)
x = format(Sys.Date(), "%Y")

data$Age = as.numeric(x) - data$Y.O.B
rm(x)




levels(data$Board.in.Twelth)[levels(data$Board.in.Twelth)=="0"] = NA
levels(data$School.Board.in.Tenth)[levels(data$School.Board.in.Tenth)=="0"] = NA
data$Score.in.Domain[data$Score.in.Domain == -1] = NA




sum(is.na(data))
nrow(data[complete.cases(data),])


data$CityTier = as.factor(data$CityTier)
data$CollegeTier = as.factor(data$CollegeTier)



data$Gap_in_education = data$Year.of.Graduation.Completion - data$Year.Of.Twelth.Completion
x = c(5,6)
data$Gap_Scenario = ifelse(data$Graduation == "B.Tech/B.E." & data$Gap_in_education == 4, "ideal",
                           ifelse(data$Graduation == "M.Tech./M.E." & data$Gap_in_education == 6,"ideal",
                                  ifelse(data$Graduation == "MCA" & data$Gap_in_education %in% x,"ideal","non-ideal")))

data$Gap_Scenario = as.factor(data$Gap_Scenario)




data$Y.O.B = NULL
data$D.O.B = NULL
data$Birth.Time = NULL
data$Date.Of.Birth = NULL
data$Candidate.ID = NULL
data$CityCode = NULL
data$CollegeCode = NULL
data$Year.Of.Twelth.Completion = NULL

# 1) Combine Levels
# 2) Imputations

str(newdata)
summary(newdata)
colnames(newdata)


newdata$CP.opt = as.factor(ifelse(newdata$Score.in.ComputerProgramming == -100, '0','1'))
newdata$ESem.opt = as.factor(ifelse(newdata$Score.in.ElectronicsAndSemicon == -100, '0','1'))
newdata$CS.opt = as.factor(ifelse(newdata$Score.in.ComputerScience == -100, '0','1'))
newdata$Mech.opt = as.factor(ifelse(newdata$Score.in.MechanicalEngg == -100, '0','1'))
newdata$ElEng.opt = as.factor(ifelse(newdata$Score.in.ElectricalEngg == -100, '0','1'))
newdata$Tel.opt = as.factor(ifelse(newdata$Score.in.TelecomEngg == -100, '0','1'))
newdata$Civ.opt = as.factor(ifelse(newdata$Score.in.CivilEngg == -100, '0','1'))



numeric_data = Filter(is.numeric, newdata)
categ_data = Filter(is.factor, newdata)

numeric_data$Score.in.ComputerProgramming[numeric_data$Score.in.ComputerProgramming == -100] = 0
numeric_data$Score.in.ElectronicsAndSemicon[numeric_data$Score.in.ElectronicsAndSemicon == -100] = 0
numeric_data$Score.in.ComputerScience[numeric_data$Score.in.ComputerScience == -100] = 0
numeric_data$Score.in.MechanicalEngg[numeric_data$Score.in.MechanicalEngg == -100] = 0
numeric_data$Score.in.ElectricalEngg[numeric_data$Score.in.ElectricalEngg == -100] = 0
numeric_data$Score.in.TelecomEngg[numeric_data$Score.in.TelecomEngg == -100] = 0
numeric_data$Score.in.CivilEngg[numeric_data$Score.in.CivilEngg == -100] = 0
std_data = scale(numeric_data[,-1])
numeric_data = cbind(std_data,"Pay_in_INR" = numeric_data[,1])
numeric_data = as.data.frame(numeric_data)


final_data = as.data.frame(cbind(numeric_data, categ_data))
final_data1 = final_data[,c(1:8,9,32,10,33,11,34,12,35,13,36,14,37,15,38,16:31,39)]
final_data = final_data1[,c(1:29,31:39,30)]

bilbo = createDataPartition(final_data$Pay_in_INR, p = 0.7, list = F)
train_final = final_data[bilbo,]
test_final = final_data[-bilbo,]
samwise = createDataPartition(train_final$Pay_in_INR, p = 0.9, list = F)
train_final = train_final[samwise,]
valid_final = train_final[-samwise,]


tuneRF(train_final[,-39], train_final[,39], ntreeTry = 120)
rf_one = randomForest(train_final[,-39], train_final[,39], ntree = 120, mtry = 12)
plot(rf_one)
results = predict(rf_one, valid_final[,-39])
regr.eval(valid_final[,39], results)
#         mae          mse         rmse         mape 
#4.462108e+04 6.798863e+09 8.245522e+04 9.654843e-02 

varImpPlot(rf_one)
str(train_final)


model_lm = lm(Pay_in_INR ~ ., data = train_final)
summary(model_lm)
results_lm = predict(model_lm, valid_final[,-39])
regr.eval(valid_final[,39],results_lm)
#         mae          mse         rmse         mape 
#1.771934e+05 8.565079e+10 2.926616e+05 3.488214e-01 

plot(model_lm)

model_tree = rpart(Pay_in_INR ~ ., data = train_final, method = "anova")
results_tree = predict(model_tree, valid_final[,-39])
actual_tree = valid_final[,39]
regr.eval(actual_tree, results_tree)
#         mae          mse         rmse         mape 
#1.625819e+05 7.531035e+10 2.744273e+05 3.405208e-01 

train_final_dum = dummyVars("~.", data = train_final)
dummy_train = data.frame(predict(train_final_dum, train_final))

valid_final_dum = dummyVars("~.", data = valid_final)
dummy_valid = data.frame(predict(valid_final_dum, valid_final))


prin_comp = prcomp(dummy_train[,-137])


std_dev = prin_comp$sdev
pr_var = std_dev^2

prop_varex = pr_var/sum(pr_var)

#SCREE PLOT
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")



#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


train_pca = data.frame("Pay_in_INR" = train_final$Pay_in_INR, prin_comp$x)
train_pca = train_pca[,1:51]

valid_pca = predict(prin_comp, newdata = dummy_valid[,-137])
valid_pca = valid_pca[,1:50]
valid_pca = as.data.frame(valid_pca)

actual_pay = valid_final$Pay_in_INR

model_rf = randomForest(train_pca[,-1], train_pca[,1], ntree = 120, mtry = 16)
results_pay = predict(model_rf, valid_pca)
regr.eval(actual_pay, results_pay)
#         mae          mse         rmse         mape 
#4.579697e+04 7.117918e+09 8.436775e+04 9.982875e-02 

plot(model_rf)
varImpPlot(model_rf)


pca_tree = rpart(Pay_in_INR~., data = train_pca, method = "anova")
results_pca_tree = predict(pca_tree, newdata = valid_pca)
regr.eval(actual_tree, results_pca_tree)
#         mae          mse         rmse         mape 
#1.625569e+05 7.425971e+10 2.725064e+05 3.404169e-01