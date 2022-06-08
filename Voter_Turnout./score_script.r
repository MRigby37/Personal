library(readxl)
install.packages("writexl")
library(writexl)
Book2 <- read_excel("~/Desktop/Book2.xlsx")
df.1 <- Book2
#View(df.1)

#Rename and Remove Variables
names(df.1)[names(df.1) == "2nd_g_precinct_turnout"] <- "second_g_precinct_turnout"
names(df.1)[names(df.1) == "2nd_p_precinct_turnout"] <- "second_p_precinct_turnout"
data_train <- subset(df.1, select = -c(optimus_id,maritalstatus, dwellingtype, income, education, dma, occupationindustry, vh12g, vh12p, vh10g, vh10p, vh08p, vh08g, vh06p, vh06g, vh04p, vh04g, vh02p, vh02g, vh00p, vh00g, net_worth, petowner_dog, intrst_nascar_in_hh, intrst_musical_instruments_in_hh, donates_to_conservative_causes, donates_to_liberal_causes, second_p_precinct_turnout))
View(data_train)

#Home Owner
#Training Data

data_train$home_owner_or_renter <- as.character(data_train$home_owner_or_renter)
data_train$home_owner_or_renter[data_train$home_owner_or_renter %in% c("nan")] <- "Likely Renter"
data_train$home_owner_or_renter <- factor(data_train$home_owner_or_renter)

#Party
#Training Data
data_train$party <- as.character(data_train$party)
data_train$party[data_train$party %in% c("Republican")] <- "Group 1"
data_train$party[data_train$party %in% c("Democratic")] <- "Group 2"
data_train$party[data_train$party %in% c("Other","Non-Partisan","Libertarian","Green","Natural Law")] <- "Group 3"
data_train$party[data_train$party %in% c("American Independent")] <- "Group 4"
data_train$party <- factor(data_train$party)


#Converting Numeric to Factor Variables
data_train$current_cycle_p <- factor(data_train$current_cycle_p)
data_train$cd <- factor(data_train$cd)
data_train$ethnicity <- factor(data_train$ethnicity)
data_train$home_owner_or_renter <- factor(data_train$home_owner_or_renter)

#Load File
load(file="modelfile")

#Score and Predict Data
data_train$pred <- predict(rf_classifier, newdata=data_train, type='response')
data_train$prob <- predict(rf_classifier, newdata=data_train, type="prob")
View(data_train)
head(data_train$pred)
glimpse(data_train$pred)
summary(data_train$pred)


#Write to Excel 
write_xlsx(data_train,"~/Desktop/data with predictions.xlsx")
