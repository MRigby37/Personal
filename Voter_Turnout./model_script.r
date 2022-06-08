library(readxl)
VOTER_FILE_NO_NAN <- read_excel("~/Desktop/VOTER FILE NO NAN.xlsx")
#View(VOTER_FILE_NO_NAN)
df.1 <- VOTER_FILE_NO_NAN
View(df.1)
install.packages("rockchalk")
library(rockchalk)

#Shuffle to create a Random Sample for Splitting
shuffle_index <- sample(1:nrow(df.1))
head(shuffle_index)
df.1 <-df.1[shuffle_index, ]
head(df.1)

#Split dataset into "training" (70%) and "validation" (30%)

create_train_test <- function(df.1, size = 0.7, train = TRUE) {
  n_row = nrow(df.1)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (df.1[train_sample, ])
  } else {
    return (df.1[-train_sample, ])
  }
}

data_train <- create_train_test(df.1, 0.7, train = TRUE)
data_test <- create_train_test(df.1, 0.7, train = FALSE)
dim(data_train)
View(data_train)

#Decision Tree Trained to Consolidate Categorical Variables into Fewer Levels
#Party

install.packages("rpart")
install.packages("rpart.plot")	
install.packages("rattle")
library(rpart)
library(rpart.plot)
df.2 <- data_train[c(3,15)]
#View(df.2)
party_dt <- rpart(vh10g~., data = df.2, method = 'class')
rpart.plot(party_dt, extra = 106)

#Beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#view1
prp(party_dt, faclen = 0, cex = 0.7, extra = 1)

#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(party_dt, faclen = 0, cex = 0.7, node.fun=tot_count)

#Decision Tree Trained to Consolidate Categorical Variables into Fewer Levels
#Net Worth
df.3 <- data_train[c(31,15)]
networth_dt <- rpart(vh10g~., data = df.3, method = 'class')
rpart.plot(networth_dt, extra = 106)

#view1
prp(networth_dt, faclen = 0, cex = 0.7, extra = 1)

#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(networth_dt, faclen = 0, cex = 0.7, node.fun=tot_count)

#Decision Tree Trained to Consolidate Categorical Variables into Fewer Levels
#Education
df.4 <- data_train[c(8,15)]
education_dt <- rpart(vh10g~., data = df.4, method = 'class')
rpart.plot(education_dt, extra = 106)

#view1
prp(education_dt, faclen = 0, cex = 0.7, extra = 1)

#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(education_dt, faclen = 0, cex = 0.7, node.fun=tot_count)

#Rename Variables
#Training Data
names(data_train)[names(data_train) == "vh10g"] <- "target"
names(data_train)[names(data_train) == "vh10p"] <- "current_cycle_p"
names(data_train)[names(data_train) == "2nd_g_precinct_turnout"] <- "second_g_precinct_turnout"
names(data_train)[names(data_train) == "2nd_p_precinct_turnout"] <- "second_p_precinct_turnout"
data_train

#Validation Data
names(data_test)[names(data_test) == "vh10g"] <- "target"
names(data_test)[names(data_test) == "vh10p"] <- "current_cycle_p"
names(data_test)[names(data_test) == "2nd_g_precinct_turnout"] <- "second_g_precinct_turnout"
names(data_test)[names(data_test) == "2nd_p_precinct_turnout"] <- "second_p_precinct_turnout"
data_test

#Recode Cat Variables with New Levels
#Party
#Training Data

data_train$party <- as.character(data_train$party)
data_train$party[data_train$party %in% c("Republican")] <- "Group 1"
data_train$party[data_train$party %in% c("Democratic")] <- "Group 2"
data_train$party[data_train$party %in% c("Other","Non-Partisan","Libertarian","Green","Natural Law")] <- "Group 3"
data_train$party[data_train$party %in% c("American Independent")] <- "Group 4"
data_train$party <- factor(data_train$party)
#View(data_train$party)
#View(data_train)

#Validation Data

data_test$party <- as.character(data_test$party)
data_test$party[data_test$party %in% c("Republican")] <- "Group 1"
data_test$party[data_test$party %in% c("Democratic")] <- "Group 2"
data_test$party[data_test$party %in% c("Other","Non-Partisan","Libertarian","Green","Natural Law")] <- "Group 3"
data_test$party[data_test$party %in% c("American Independent")] <- "Group 4"
data_test$party <- factor(data_test$party)

#DMA
#Training Data

data_train$dma <- as.character(data_train$dma)
data_train$dma[data_train$dma %in% c("LAS VEGAS DMA (EST.)","LOS ANGELES DMA (EST.)")] <- "South"
data_train$dma[data_train$dma %in% c("RENO DMA (EST.)","SALT LAKE CITY DMA (EST.)")] <- "North"
data_train$dma <- factor(data_train$dma)

#Validation Data

data_test$dma <- as.character(data_test$dma)
data_test$dma[data_test$dma %in% c("LAS VEGAS DMA (EST.)","LOS ANGELES DMA (EST.)")] <- "South"
data_test$dma[data_test$dma %in% c("RENO DMA (EST.)","SALT LAKE CITY DMA (EST.)")] <- "North"
data_test$dma <- factor(data_test$dma)

#Marital Status
#Training Data

data_train$maritalstatus <- as.character(data_train$maritalstatus)
data_train$maritalstatus[data_train$maritalstatus %in% c("nan")] <- "Not Married"
data_train$maritalstatus <- factor(data_train$maritalstatus)

#Validation Data

data_test$maritalstatus <- as.character(data_test$maritalstatus)
data_test$maritalstatus[data_test$maritalstatus %in% c("nan")] <- "Not Married"
data_test$maritalstatus <- factor(data_test$maritalstatus)

#Education
#Training Data

data_train$education <- as.character(data_train$education)
data_train$education[data_train$education %in% c("Grad Degree - Likely", "Less than HS Diploma - Extremely Likely")] <- "Group 1"
data_train$education[data_train$education %in% c("Bach Degree - Extremely Likely", "HS Diploma - Extremely Likely", "Some College - Extremely Likely", "Vocational Technical Degree - Extremely Likely")] <- "Group 2"
data_train$education[data_train$education %in% c("Grad Degree - Extremely Likely")] <- "Group 3"
data_train$education[data_train$education %in% c("Some College - Likely", "Less than HS Diploma - Likely")] <- "Group 4"
data_train$education[data_train$education %in% c("Bach Degree - Likely", "HS Diploma - Likely")] <- "Group 5"
data_train$education <- factor(data_train$education)

#Validation Data

data_test$education <- as.character(data_test$education)
data_test$education[data_test$education %in% c("Grad Degree - Likely", "Less than HS Diploma - Extremely Likely")] <- "Group 1"
data_test$education[data_test$education %in% c("Bach Degree - Extremely Likely", "HS Diploma - Extremely Likely", "Some College - Extremely Likely", "Vocational Technical Degree - Extremely Likely")] <- "Group 2"
data_test$education[data_test$education %in% c("Grad Degree - Extremely Likely")] <- "Group 3"
data_test$education[data_test$education %in% c("Some College - Likely", "Less than HS Diploma - Likely")] <- "Group 4"
data_test$education[data_test$education %in% c("Bach Degree - Likely", "HS Diploma - Likely")] <- "Group 5"
data_test$education <- factor(data_test$education)

#Net Worth
#Training Data

data_train$net_worth <- as.character(data_train$net_worth)
data_train$net_worth[data_train$net_worth %in% c("$1-4999")] <- "Group 1"
data_train$net_worth[data_train$net_worth %in% c("$25000-49999", "$10000-24999", "$5000-9999")] <- "Group 2"
data_train$net_worth[data_train$net_worth %in% c("$100000-249999", "$50000-99999")] <- "Group 3"
data_train$net_worth[data_train$net_worth %in% c("$250000-499999")] <- "Group 4"
data_train$net_worth[data_train$net_worth %in% c("$499999+")] <- "Group 5"
data_train$net_worth <- factor(data_train$net_worth)

#Validation Data

data_test$net_worth <- as.character(data_test$net_worth)
data_test$net_worth[data_test$net_worth %in% c("$1-4999")] <- "Group 1"
data_test$net_worth[data_test$net_worth %in% c("$25000-49999", "$10000-24999", "$5000-9999")] <- "Group 2"
data_test$net_worth[data_test$net_worth %in% c("$100000-249999", "$50000-99999")] <- "Group 3"
data_test$net_worth[data_test$net_worth %in% c("$250000-499999")] <- "Group 4"
data_test$net_worth[data_test$net_worth %in% c("$499999+")] <- "Group 5"
data_test$net_worth <- factor(data_test$net_worth)

#Pet Owner
#Training Data

data_train$petowner_dog <- as.character(data_train$petowner_dog)
data_train$petowner_dog[data_train$petowner_dog %in% c("nan")] <- "No"
data_train$petowner_dog <- factor(data_train$petowner_dog)

#Validation Data

data_test$petowner_dog <- as.character(data_test$petowner_dog)
data_test$petowner_dog[data_test$petowner_dog %in% c("nan")] <- "No"
data_test$petowner_dog <- factor(data_test$petowner_dog)

#NASCAR
#Training Data

data_train$intrst_nascar_in_hh <- as.character(data_train$intrst_nascar_in_hh)
data_train$intrst_nascar_in_hh[data_train$intrst_nascar_in_hh %in% c("nan")] <- "No"
data_train$intrst_nascar_in_hh <- factor(data_train$intrst_nascar_in_hh)

#Validation Data

data_test$intrst_nascar_in_hh <- as.character(data_test$intrst_nascar_in_hh)
data_test$intrst_nascar_in_hh[data_test$intrst_nascar_in_hh %in% c("nan")] <- "No"
data_test$intrst_nascar_in_hh <- factor(data_test$intrst_nascar_in_hh)

#Instrument
#Training Data

data_train$intrst_musical_instruments_in_hh <- as.character(data_train$intrst_musical_instruments_in_hh)
data_train$intrst_musical_instruments_in_hh[data_train$intrst_musical_instruments_in_hh %in% c("nan")] <- "No"
data_train$intrst_musical_instruments_in_hh <- factor(data_train$intrst_musical_instruments_in_hh)

#Validation Data

data_test$intrst_musical_instruments_in_hh <- as.character(data_test$intrst_musical_instruments_in_hh)
data_test$intrst_musical_instruments_in_hh[data_test$intrst_musical_instruments_in_hh %in% c("nan")] <- "No"
data_test$intrst_musical_instruments_in_hh <- factor(data_test$intrst_musical_instruments_in_hh)

#Liberal Causes
#Training Data

data_train$donates_to_liberal_causes <- as.character(data_train$donates_to_liberal_causes)
data_train$donates_to_liberal_causes[data_train$donates_to_liberal_causes %in% c("nan")] <- "No"
data_train$donates_to_liberal_causes<- factor(data_train$donates_to_liberal_causes)

#Validation Data

data_test$donates_to_liberal_causes <- as.character(data_test$donates_to_liberal_causes)
data_test$donates_to_liberal_causes[data_test$donates_to_liberal_causes %in% c("nan")] <- "No"
data_test$donates_to_liberal_causes<- factor(data_test$donates_to_liberal_causes)

#Conservative Causes
#Training Data

data_train$donates_to_conservative_causes <- as.character(data_train$donates_to_conservative_causes)
data_train$donates_to_conservative_causes[data_train$donates_to_conservative_causes %in% c("nan")] <- "No"
data_train$donates_to_conservative_causes <- factor(data_train$donates_to_conservative_causes)

#Validation Data

data_test$donates_to_conservative_causes <- as.character(data_test$donates_to_conservative_causes)
data_test$donates_to_conservative_causes[data_test$donates_to_conservative_causes %in% c("nan")] <- "No"
data_test$donates_to_conservative_causes<- factor(data_test$donates_to_conservative_causes)

#Home Owner
#Training Data

data_train$home_owner_or_renter <- as.character(data_train$home_owner_or_renter)
data_train$home_owner_or_renter[data_train$home_owner_or_renter %in% c("nan")] <- "Likely Renter"
data_train$home_owner_or_renter <- factor(data_train$home_owner_or_renter)

#Validation Data

data_test$home_owner_or_renter <- as.character(data_test$home_owner_or_renter)
data_test$home_owner_or_renter[data_test$home_owner_or_renter %in% c("nan")] <- "Likely Renter"
data_test$home_owner_or_renter<- factor(data_test$home_owner_or_renter)

#View(data_train)
#View(data_test)

#Remove Unwanted Variables from Datasets

library(dplyr)
data_train <- subset(data_train, select = -c(occupationindustry, p12_precinct_turnout, g12_precinct_turnout, vh14p, vh12p, vh12g, vh08p, vh08g, vh06p, vh06g, vh04p, vh04g, vh02p, vh02g, vh00p, vh00g))
data_test <- subset(data_test, select = -c(occupationindustry, p12_precinct_turnout, g12_precinct_turnout, vh14p, vh12p, vh12g, vh08p, vh08g, vh06p, vh06g, vh04p, vh04g, vh02p, vh02g, vh00p, vh00g))
data_train <- subset(data_train, select = -c(optimus_id))
data_test <- subset(data_test, select = -c(optimus_id))
#View(data_train)
#View(data_test)

#Converting Numeric to Factor Variables

data_train$target <- factor(data_train$target)
data_test$target <- factor(data_test$target)
data_train$current_cycle_p <- factor(data_train$current_cycle_p)
data_test$current_cycle_p <- factor(data_test$current_cycle_p)
data_train$cd <- factor(data_train$cd)
data_test$cd <- factor(data_test$cd)
data_train$ethnicity <- factor(data_train$ethnicity)
data_test$ethnicity <- factor(data_test$ethnicity)
data_train$dwellingtype <- factor(data_train$dwellingtype)
data_test$dwellingtype <- factor(data_test$dwellingtype)
data_train$income <- factor(data_train$income)
data_test$income <- factor(data_test$income)
data_train$party <- factor(data_train$party)
data_test$party <- factor(data_test$party)
View(data_train)

#Maximal Tree for Determining Variable Importance


install.packages("caret")
library(caret)
set.seed(12345)
rPartMod <- rpart(target~., data = data_train, method = 'class')
rpartImp <- varImp(rPartMod)
print(rpartImp)

#Random Forest for Determining Variable Importance
#100 Trees

install.packages("randomForest")
library(randomForest)
set.seed(12345)
rf_classifier <- randomForest(target ~ ., data=data_train, ntree=100, mtry=2, importance=TRUE, na.action=na.roughfix)
print(rf_classifier)
varImpPlot(rf_classifier)

#500 trees

set.seed(12345)
rf_classifier <- randomForest(target ~ ., data=data_train, ntree=500, mtry=2, importance=TRUE, na.action=na.roughfix)
print(rf_classifier)
varImpPlot(rf_classifier)

#750 trees

set.seed(12345)
rf_classifier <- randomForest(target ~ ., data=data_train, ntree=750, mtry=2, importance=TRUE, na.action=na.roughfix)
print(rf_classifier)
varImpPlot(rf_classifier)

#Remove Observations with Missing Values

data_train_no_missing <- na.omit(data_train)
data_test_no_missing <-na.omit(data_test)
#glimpse(data_train_no_missing)
#View(data_train_no_missing)

#LASSO Regression for Variable Selection
install.packages("glmnet")
library(glmnet)
x <- data.matrix(data_train_no_missing[, c('total_mid_g','current_cycle_p', 'age', 'total_pres_g', 'total_pres_p', 'most_recent_p_precinct_turnout', 'second_p_precinct_turnout', 'second_g_precinct_turnout', 'education','most_recent_g_precinct_turnout', 'cd', 'total_mid_p', 'dma', 'home_owner_or_renter', 'net_worth', 'party', 'income', 'ethnicity', 'petowner_dog', 'intrst_musical_instruments_in_hh', 'intrst_nascar_in_hh', 'dwellingtype', 'maritalstatus', 'donates_to_conservative_causes', 'donates_to_liberal_causes')]) 
y <- as.numeric(data_train_no_missing$target)
set.seed(12345)
cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

plot(cv.lasso)

cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]


#Logistic Stepwise Regression for Variable Selection

install.packages('tidyverse')
library(tidyverse)
library(MASS)

full.model <- glm(target ~., data = data_train_no_missing, family = binomial)
coef(full.model)

step.model <- full.model %>% stepAIC(trace = FALSE)
coef(step.model)

#PLS for Variable Selection

install.packages('plsgenomics')
library(plsgenomics)

#View(x)
variable.selection(x, y,nvar=26)


#Sensitivty Analysis to Determine worth of Keeping or Removing Education and/or Net Worth in the Dataset

# df_for_comp_train <- subset(data_train_no_missing, select = -c(income, dwellingtype))
# df_for_comp_test <- subset(data_test_no_missing, select = -c(income, dwellingtype))
# df_sans_edu_train <- subset(data_train_no_missing, select = -c(income, dwellingtype, education))
# df_sans_edu_test <- subset(data_test_no_missing, select = -c(income, dwellingtype, education))
# df_sans_networth_train <- subset(data_train_no_missing, select = -c(income, dwellingtype, net_worth))
# df_sans_networth_test <- subset(data_test_no_missing, select = -c(income, dwellingtype, net_worth))
# df_sans_both_train <- subset(data_train_no_missing, select = -c(income, dwellingtype, net_worth, education))
# df_sans_both_test <- subset(data_test_no_missing, select = -c(income, dwellingtype, net_worth, education))

df_for_comp_train <- subset(data_train_no_missing, select = -c(income, dwellingtype))
df_for_comp_test <- subset(data_test_no_missing, select = -c(income, dwellingtype))
dff_for_comp_train <- subset(data_train, select = -c(income, dwellingtype))
dff_for_comp_test <- subset(data_test, select = -c(income, dwellingtype))
# dff_sans_edu_train <- subset(data_train, select = -c(income, dwellingtype, education))
# dff_sans_edu_test <- subset(data_test, select = -c(income, dwellingtype, education))
# dff_sans_networth_train <- subset(data_train, select = -c(income, dwellingtype, net_worth))
# dff_sans_networth_test <- subset(data_test, select = -c(income, dwellingtype, net_worth))
dff_sans_both_train <- subset(data_train, select = -c(income, dwellingtype, net_worth, education))
dff_sans_both_test <- subset(data_test, select = -c(income, dwellingtype, net_worth, education))

#Sans Education
# full.model <- glm(vh10g ~., data = df_sans_edu_train, family = binomial)
# coef(full.model)
# print(full.model)
# summary(full.model)
# #glimpse(df_sans_edu_test)
# prob = predict(full.model, df_sans_edu_test, type="response")
# pred = rep("0", dim(df_sans_edu_test)[1])
# pred[prob > .5] = "1"
# table(pred, df_sans_edu_test$vh10g)
# 
# 1 - mean(pred == df_sans_edu_test$vh10g)
# 
# #Sans Net Worth
# full.model <- glm(vh10g ~., data = df_sans_networth_train, family = binomial)
# coef(full.model)
# print(full.model)
# summary(full.model)
# #glimpse(df_sans_networth_test)
# prob = predict(full.model, df_sans_networth_test, type="response")
# pred = rep("0", dim(df_sans_networth_test)[1])
# pred[prob > .5] = "1"
# table(pred, df_sans_networth_test$vh10g)
# 
# 1 - mean(pred == df_sans_networth_test$vh10g)
# 
# #Sans Both
# full.model <- glm(vh10g ~., data = df_sans_both_train, family = binomial)
# coef(full.model)
# print(full.model)
# summary(full.model)
# #glimpse(df_sans_both_test)
# prob = predict(full.model, df_sans_both_test, type="response")
# pred = rep("0", dim(df_sans_both_test)[1])
# pred[prob > .5] = "1"
# table(pred, df_sans_both_test$vh10g)

#1 - mean(pred == df_sans_both_test$vh10g)

#With Both
full.model <- glm(target ~., data = df_for_comp_train, family = binomial)
coef(full.model)
print(full.model)
summary(full.model)
#glimpse(df_for_comp_test)
prob = predict(full.model, df_for_comp_test, type="response")
pred = rep("0", dim(df_for_comp_test)[1])
pred[prob > .5] = "1"
table(pred, df_for_comp_test$target)

1 - mean(pred == df_for_comp_test$target)

#Sans Both
data_train2 <- na.omit(dff_sans_both_train)
data_test2 <- na.omit(dff_sans_both_test)
glimpse(data_train2)

full.model <- glm(target ~., data = data_train2, family = binomial)
coef(full.model)
print(full.model)
summary(full.model)
#glimpse(df_sans_both_test)
prob = predict(full.model, data_test2, type="response")
pred = rep("0", dim(data_test2)[1])
pred[prob > .5] = "1"
table(pred, data_test2$target)

1 - mean(pred == data_test2$target)

#Final Variable Datasets

final_train_with_missing <- subset(data_train, select = -c(net_worth, dwellingtype, income, education, intrst_nascar_in_hh, intrst_musical_instruments_in_hh, donates_to_liberal_causes, donates_to_conservative_causes, second_p_precinct_turnout, dma, maritalstatus, petowner_dog))
final_test_with_missing <- subset(data_test, select = -c(net_worth, dwellingtype, income, education, intrst_nascar_in_hh, intrst_musical_instruments_in_hh, donates_to_liberal_causes, donates_to_conservative_causes, second_p_precinct_turnout, dma, maritalstatus, petowner_dog))
final_train_no_missing <- na.omit(final_train_with_missing)
final_test_no_missing <- na.omit(final_test_with_missing)

View(final_train_no_missing)
View(final_train_with_missing) 
glimpse(final_test_with_missing)
glimpse(final_train_with_missing)

#Final Random Forest Model 
#500 Trees
set.seed(12345)
rf_classifier <- randomForest(target ~ ., data=final_train_with_missing, ntree=500, mtry=2, importance=TRUE, na.action=na.roughfix)
print(rf_classifier)
varImpPlot(rf_classifier)
summary(rf_classifier)

#Final Decision Tree Model
library(caret)
set.seed(12345)
rPartMod2 <- rpart(target~., data =final_train_with_missing, method = 'class')
print(rPartMod2)
summary(rPartMod2)
prob = predict(rPartMod2, final_train_with_missing, type="response")
pred = rep("0", dim(final_train_with_missing)[1])
pred[prob > .5] = "1"
table(pred, final_train_with_missing$target)
1 - mean(pred == final_train_with_missing$target)

#Save Final Model
save(file="modelfile",rf_classifier)
