library(tidyverse)

df <- read_csv("C:/Users/m-mar/Desktop/R/project_v1.csv")


#Convert No-show to integer

df$`No-show` <- +(df$`No-show`=="Yes")

#Convert all integers to logicals

df$`No-show` <- as.logical(df$`No-show`)

df$Scholarship <- as.logical(df$Scholarship)

df$Hipertension <- as.logical(df$Hipertension)

df$Diabetes <- as.logical(df$Diabetes)

df$Alcoholism <- as.logical(df$Alcoholism)

df$SMS_received <- as.logical(df$SMS_received)

#Convert characters to factors 

df$Gender <- as.factor(df$Gender)

df$Neighbourhood <- as.factor(df$Neighbourhood)

df$Handcap <- as.factor(df$Handcap)

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

library(lubridate)

#Transform dates and add ScheduledDelay

df$AppointmentDay = as.Date(df$AppointmentDay)
df$ScheduledDate = as.date(df$ScheduledDay)
df$ScheduledDelay = df$AppointmentDay - df$ScheduledDate
df <- df %>% subset(select = -ScheduledDay)


#Look for missing values  

table(is.na(df$ScheduledDate))

#None found

# Data frame used for tableau visualizations 
write_excel_csv(df, "project_draft1.2.csv")

View(df)



#select only conditions
x1 <- df_v1 %>%
  select(Scholarship, Hipertension, Diabetes, Alcoholism, SMS_received, `No-show`)

#filter only no-shows and conditions
x1.1 <- x1 %>% filter(`No-show` == TRUE)

#filter shows and conditions
x1.2 <- x1 %>% filter(`No-show` == FALSE)

#find number of conditions of no-shows
x1.1$num_conditions <- rowSums(x1.1) - 1

#find number of conditions of shows
x1.2$num_conditions <- rowSums(x1.2)

#arrange no-shows by number of conditions

x1.1 <- x1.1 %>% arrange(desc(num_conditions))

#arrange shows by number of conditions

x1.2 <- x1.2 %>% arrange(desc(num_conditions))

#count of SMS_recieved 

table(x1.2$SMS_received)





#ways of finding uniques

neighboorhood_unique <- unique(df$Neighbourhood)

neighboorhood_unique

length(table(df$Age))


#find shows vs no-shows of different conditions

table(df_v1$Alcoholism)
table(df_v1$Gender)


#Age summary
summary(df_v1, Age)

df_v1$Age[df_v1$Age == '115']


#Look for na's
table(is.na(df_v1$Age))












library(GGally)

cat_v1 <- df_v1[, c("Gender", "SMS_received", "No-show")]
ggpairs(cat_v1)
