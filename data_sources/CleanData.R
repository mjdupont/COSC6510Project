if(!require('tidyverse')) {
  install.packages('tidyverse') 
  library(tidyverse)
}

if(!require('reshape')) {
  install.packages('reshape')
  library(reshape)
}

if(!require('chron')) {
  install.packages('chron')
  library(chron)
}


df <- read_csv("data_sources/KaggleV2-May-2016.csv")
#Creates directory if it doesn't exist, otherwise does nothing
dir.create("images", showWarnings = FALSE) 

########################################
#Renaming Columns:
########################################
df$No_show <- df$`No-show`
df <- df %>% subset(select = -`No-show`)
df$Hypertension <- df$Hipertension
df <- df %>% subset(select = -Hipertension)
df$Handicap <- df$Handcap
df <- df %>% subset(select = -Handcap)

########################################
#End Renaming Columns
########################################

########################################
#Add Supplemental Columns:
########################################

#Destructuring physical illness into all categories
condition_as_str <- function(condition) {
  return(switch(condition+1,
                #0
                "No Condition",
                #1
                "Alcoholism Only",
                #2
                "Hypertension Only",
                #3
                "Alcoholism + Hypertension",
                #4
                "Diabetes Only",
                #5
                "Alcoholism + Diabetes",
                #6
                "Hypertension + Diabetes",
                #7
                "Alcoholism + Hypertension + Diabetes"
  ))
}

df$AlcoholismN <- 2^0*df$Alcoholism
df$HypertensionN <- (2^1)*df$Hypertension
df$DiabetesN <- 2^2*df$Diabetes
df$PhysicalConditionN <- df$HypertensionN + df$AlcoholismN + df$DiabetesN
df$PhysicalCondition <- as.factor(sapply(df$PhysicalConditionN, condition_as_str))
df <- df %>% subset(select = -c(AlcoholismN, HypertensionN, DiabetesN, PhysicalConditionN))

#Calculating Different relevant date information
df$AppointmentDay = as.Date(df$AppointmentDay)
df$ScheduledDate = as.Date(df$ScheduledDay)
df$ScheduledDelay = 
  (df$AppointmentDay - df$ScheduledDate) %>%
  as.numeric(units = "days")

########################################
#End Supplemental Columns:
########################################

########################################
#Normalize data types:
########################################

df$No_show <- +(df$No_show=="Yes")
df$No_show <- as.logical(df$No_show)

#One OR MORE SMS messages about appointment received.
df$SMS_received <- as.logical(df$SMS_received)

#Whether the patient is receiving financial assistance.
df$Scholarship <- as.logical(df$Scholarship)

#Presence of Hypertension
df$Hypertension <- as.logical(df$Hypertension)
#Presence of Diabetes
df$Diabetes <- as.logical(df$Diabetes)
#Presence of Alcoholism
df$Alcoholism <- as.logical(df$Alcoholism)

#Presumed to be equal to sex?
df$Gender <- as.factor(df$Gender)

#Patient is handicapped
df$Handicap <- as.logical(df$Handicap)

########################################
#End Normalize data types:
########################################

########################################
#Remove bad data:
########################################
df <- 
  df %>% 
  filter(Age >= 0) %>%
  filter(ScheduledDelay >= 0) %>%
  filter(Handicap >= 0)

########################################
#Create partition for children under 18
########################################
age.df <- 
  df %>%
  filter(Age < 18 & Age > 0)

########################################
#End Remove bad data:
########################################
write.csv(df, "data_warehouse/NoShow_Cleaned.csv", row.names = FALSE)

