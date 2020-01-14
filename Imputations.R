names = colnames(data)[colSums(is.na(data)) > 0]
names
#"School.Board.in.Tenth" "Board.in.Twelth"  "Age" "Score.in.Domain"


imputers = data[complete.cases(data),]
imputers_miss = data[!complete.cases(data),]


xc = chisq.test(imputers$School.Board.in.Tenth, imputers$State)
xc
corrplot(xc$residuals, is.corr = F)
#data:  imputers$School.Board.in.Tenth and imputers$State
#X-squared = 47953, df = 552, p-value < 2.2e-16


xd = chisq.test(imputers$School.Board.in.Tenth, imputers$Board.in.Twelth)
xd
corrplot(xd$residuals, is.corr = F)
#data:  imputers$School.Board.in.Tenth and imputers$Board.in.Twelth
#X-squared = 185820, df = 552, p-value < 2.2e-16


xe = chisq.test(imputers$Board.in.Twelth, imputers$State)
xe
corrplot(xe$residuals, is.corr =F)
#data:  imputers$Board.in.Twelth and imputers$State
#X-squared = 54817, df = 576, p-value < 2.2e-16
rm(xc,xd,xe)



'%!in%' = function(x,y)!('%in%'(x,y))

for(i in 1:nrow(imputers_miss)){
  if(imputers_miss$State[i] %in% levels(data$School.Board.in.Tenth)){
    imputers_miss$School.Board.in.Tenth[is.na(imputers_miss$School.Board.in.Tenth)] = imputers_miss$State[is.na(imputers_miss$School.Board.in.Tenth)]
  }
  if(imputers_miss$State[i] %!in% levels(data$School.Board.in.Tenth)){
    imputers_miss$School.Board.in.Tenth[is.na(imputers_miss$School.Board.in.Tenth)] = "others"
  }
}
rm(i)

imputers_miss$School.Board.in.Tenth[is.na(imputers_miss$School.Board.in.Tenth)] = "other"
sum(is.na(imputers_miss$School.Board.in.Tenth))



imputers_miss$Board.in.Twelth[is.na(imputers_miss$Board.in.Twelth)] = imputers_miss$School.Board.in.Tenth[is.na(imputers_miss$Board.in.Twelth)]
sum(is.na(imputers_miss$Board.in.Twelth))

newdata = rbind(imputers, imputers_miss)

names = colnames(newdata)[colSums(is.na(newdata)) > 0]
names
#"Score.in.Domain" "Age" 

set.seed(001)
rand_ind = sample(nrow(newdata))
newdata = newdata[rand_ind,]
rm(rand_ind, imputers, imputers_miss)


nrow(newdata[complete.cases(newdata),])
imputers = newdata[complete.cases(newdata),]
imputers_miss = newdata[!(complete.cases(newdata)),]

exp = imputers
exp_num = exp[,c(30,17)]
exp_miss = prodNA(exp_num, noNA = 0.1) #10% missing values introduced in exp_miss



exp_age = cbind(exp[,-c(1,30,17)],"Age" = exp_miss$Age)
#created a df with 'Age' containing NA's and all other columns excluding 'Pay' and
#'Score.in.Domain' named exp_age
exp_score = cbind(exp[,-c(1,30,17)],"Score.in.Domain" = exp_miss$Score.in.Domain)


age_train = exp_age[complete.cases(exp_age),]
age_valid = exp_age[!complete.cases(exp_age),]


score_train = exp_score[complete.cases(exp_score),]
score_valid = exp_score[!complete.cases(exp_score),]



rpart_age = rpart(Age ~ ., data= age_train, method="anova")
age_rpart = predict(rpart_age, age_valid)
age_rpart = round(age_rpart)
age_actu = exp_num[is.na(exp_miss$Age),"Age"]
regr.eval(age_actu, age_rpart)
#       mae        mse       rmse       mape 
#0.59126365 0.76287051 0.87342459 0.02082801 

rm(rpart_age, age_rpart)

knn_age = knnImputation(exp_age)
age_knn = knn_age[is.na(exp_age$Age), "Age"]
age_knn = round(age_knn)
regr.eval(age_actu, age_knn)
#        mae         mse        rmse        mape 
#0.258970359 0.375975039 0.613168035 0.009108568 

rm(knn_age, age_knn)

ci_age = centralImputation(exp_age)
age_ci = ci_age[is.na(exp_age$Age), "Age"]
age_ci = round(age_ci)
regr.eval(age_actu, age_ci)
#       mae        mse       rmse       mape 
#1.32917317 2.85881435 1.69080287 0.04624213

rm(ci_age, age_ci)



rpart_score = rpart(Score.in.Domain ~ ., data= score_train, method="anova")
score_rpart = predict(rpart_score, score_valid)
score_actu = exp_num[is.na(exp_miss$Score.in.Domain),"Score.in.Domain"]
regr.eval(score_actu, score_rpart)
#       mae        mse       rmse       mape 
#0.09481679 0.01733919 0.13167835 0.17521269

rm(rpart_score, score_rpart)


knn_score = knnImputation(exp_score)
score_knn = knn_score[is.na(exp_score$Score.in.Domain), "Score.in.Domain"]
regr.eval(score_actu, score_knn)
#        mae         mse        rmse        mape 
#0.054368466 0.008540666 0.092415726 0.100080182

rm(knn_score, score_knn)


ci_score = centralImputation(exp_score)
score_ci = ci_score[is.na(exp_score$Score.in.Domain),"Score.in.Domain"]
regr.eval(score_actu, score_ci)
#       mae        mse       rmse       mape 
#0.17427507 0.04557488 0.21348273 0.34012735

rm(ci_score, score_ci)


rf_age = randomForest(age_train[,-30], age_train[,30], ntree = 120)
age_rf = predict(rf_age, age_valid[,-30])
age_rf = round(age_rf)
regr.eval(age_actu, age_rf)
#        mae         mse        rmse        mape 
#0.170826833 0.208268331 0.456364252 0.006040812
plot(rf_age)
varImpPlot(rf_age)


rf_score = randomForest(score_train[,-30], score_train[,30], ntree = 120)
score_rf = predict(rf_score, score_valid[,-30])
regr.eval(score_actu, score_rf)
#        mae         mse        rmse        mape 
#0.034948234 0.004105507 0.064074227 0.067670623
plot(rf_score)
varImpPlot(rf_score)


age_newdata = predict(rf_age, newdata[is.na(newdata$Age),-c(1,17,30)])
age_newdata = round(age_newdata)
newdata$Age[is.na(newdata$Age)] = age_newdata


score_newdata = predict(rf_score, newdata[is.na(newdata$Score.in.Domain), -c(1,17,30)])
newdata$Score.in.Domain[is.na(newdata$Score.in.Domain)] = score_newdata
sum(is.na(newdata))
