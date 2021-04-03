minors.mlr <- read_csv("NoShow_Cleaned_Minors.csv")
View(minors.mlr)


#Remove unnecessary columns

minors.mlr <- minors.mlr %>% subset(select = -AppointmentID)
minors.mlr <- minors.mlr %>% subset(select = -AppointmentDay)
minors.mlr <- minors.mlr %>% subset(select = -PatientId)
minors.mlr <- minors.mlr %>% subset(select = -ScheduledDate)


#Convert logicals back to numericals

minors.mlr$No_show <- as.numeric(minors.mlr$No_show)
minors.mlr$Scholarship <- as.numeric(minors.mlr$Scholarship)
minors.mlr$Hypertension <- as.numeric(minors.mlr$Hypertension)
minors.mlr$Diabetes <- as.numeric(minors.mlr$Diabetes)
minors.mlr$Alcoholism <- as.numeric(minors.mlr$Alcoholism)
minors.mlr$SMS_received <- as.numeric(minors.mlr$SMS_received)
minors.mlr$Handicap <- as.numeric(minors.mlr$Handicap)

#Convert factors to numericals

str(minors.mlr)

minors.mlr$Gender <- as.factor(minors.mlr$Gender)
minors.mlr$Gender <- as.numeric(minors.mlr$Gender)

minors.mlr$Neighbourhood <- as.factor(minors.mlr$Neighbourhood)
minors.mlr$Neighbourhood <- as.numeric(minors.mlr$Neighbourhood)

#Add number of conditions 

minors.mlr$num_conditions <- minors.mlr$Diabetes + minors.mlr$Alcoholism + minors.mlr$Hypertension + minors.mlr$Handicap

#Reorder columns 

minors.mlr[,c(8, 1, 2, 3, 11, 12, 7, 4, 5, 6, 9, 10)]

#Create training and validation sets 

minors.mlr$id <- c(1:27379)

train.minors <- minors.mlr %>% sample_frac(.7)
validate.minors  <- anti_join(minors.mlr, train.minors, by = 'id')

train.minors <- train.minors %>% subset(select = -id)
validate.minors <- validate.minors %>% subset(select = -id)

#Create model

model.minors <- lm(No_show ~ ., train.minors)

#Check for accuracy 

pred.minors <- predict(model.minors)
pred.minors.v <- predict(model.minors, validate.minors)

accuracy(pred.minors, train.minors$No_show)
accuracy(pred.minors.v, validate.minors$No_show)

#Create new csv for mlr

write_excel_csv(minors.mlr, "NoShow_Cleaned_Minors_MLR.csv")



################################################################################
#Same procedure for adults




adults.mlr <- read_csv("NoShow_Cleaned_Adults.csv")
View(adults.mlr)


#Remove unnecessary columns

adults.mlr <- adults.mlr %>% subset(select = -AppointmentID)
adults.mlr <- adults.mlr %>% subset(select = -AppointmentDay)
adults.mlr <- adults.mlr %>% subset(select = -PatientId)
adults.mlr <- adults.mlr %>% subset(select = -ScheduledDate)


#Convert logicals back to numericals

adults.mlr$No_show <- as.numeric(adults.mlr$No_show)
adults.mlr$Scholarship <- as.numeric(adults.mlr$Scholarship)
adults.mlr$Hypertension <- as.numeric(adults.mlr$Hypertension)
adults.mlr$Diabetes <- as.numeric(adults.mlr$Diabetes)
adults.mlr$Alcoholism <- as.numeric(adults.mlr$Alcoholism)
adults.mlr$SMS_received <- as.numeric(adults.mlr$SMS_received)
adults.mlr$Handicap <- as.numeric(adults.mlr$Handicap)

#Convert factors to numericals

adults.mlr$Gender <- as.factor(adults.mlr$Gender)
adults.mlr$Gender <- as.numeric(adults.mlr$Gender)

adults.mlr$Neighbourhood <- as.factor(adults.mlr$Neighbourhood)
adults.mlr$Neighbourhood <- as.numeric(adults.mlr$Neighbourhood)

#Add number of conditions 

adults.mlr$num_conditions <- adults.mlr$Diabetes + adults.mlr$Alcoholism + adults.mlr$Hypertension + adults.mlr$Handicap

#Reorder columns 

adults.mlr[,c(8, 1, 2, 3, 11, 12, 7, 4, 5, 6, 9, 10)]

#Create training and validation sets 

adults.mlr$id <- c(1:83147)

train.adults <- adults.mlr %>% sample_frac(.7)
validate.adults  <- anti_join(adults.mlr, train.adults, by = 'id')

train.adults <- train.adults %>% subset(select = -id)
validate.adults <- validate.adults %>% subset(select = -id)

#Create model

model.adults <- lm(No_show ~ ., train.adults)

#Check for accuracy 

pred.adults <- predict(model.adults)
pred.adults.v <- predict(model.minors, validate.adults)

accuracy(pred.adults, train.adults$No_show)
accuracy(pred.adults.v, validate.adults$No_show)

#Create new csv for mlr

write_excel_csv(adults.mlr, "NoShow_Cleaned_Adults_MLR.csv")