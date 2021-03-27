library(tidyverse)

df <- read_csv("C:/Users/m-mar/Desktop/R/project_v1.csv")
View(df)


#Rename columns:


df$No_show <- df$`No-show`
df <- df %>% subset(select = -`No-show`)

df$Hypertension <- df$Hipertension
df <- df %>% subset(select = -Hipertension)

df$Handicap <- df$Handcap
df <- df %>% subset(select = -Handcap)


#Convert No-show to integer


df$No_show <- +(df$No_show=="Yes")


#Convert all higher levels of handicap to 1


df$Handicap <- df$Handicap %>% pmin(df$Handicap, 1)


#Convert all integers to logicals


df$No_show <- as.logical(df$No_show)

df$Scholarship <- as.logical(df$Scholarship)

df$Hypertension <- as.logical(df$Hypertension)

df$Diabetes <- as.logical(df$Diabetes)

df$Alcoholism <- as.logical(df$Alcoholism)

df$SMS_received <- as.logical(df$SMS_received)

df$Handicap <- as.logical(df$Handicap)


#Convert characters to factors 


df$Gender <- as.factor(df$Gender)

df$Neighbourhood <- as.factor(df$Neighbourhood)


#Transform dates and add ScheduledDelay


library(lubridate)

df$AppointmentDay = as.Date(df$AppointmentDay)

df$ScheduledDate = as.Date(df$ScheduledDay)

df$ScheduledDelay = df$AppointmentDay - df$ScheduledDate

df <- df %>% subset(select = -ScheduledDay)


#Remove unborn children


df <- df %>% subset(df$Age >= 0)


# Data frame used for tableau visualizations 


write_excel_csv(df, "project_draft1.2.csv")
View(df)


#Partion into minors/adults

df_minors <- df %>% subset(df$Age < 18)
View(df_minors)

df_adults <- df %>% subset(df$Age >= 18)
View(df_adults)

write_excel_csv(df_minors, "NoShow_Cleaned_Minors.csv")
write_excel_csv(df_adults, "NoShow_Cleaned_Adults.csv")








#Look for missing values  

table(is.na(df$ScheduledDate))

#Checked all columns, none found

#####################################################
#Explorations


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
#did this for all, no na's found

##################################################
#End of explorations





#################################################
#testing section

df.repeats <- df %>%
  group_by(PatientId) %>%
  summarize(N=n()) %>%
  filter(N>1) %>%
  
  head(df.repeats)
#remove the extra column N here

library(GGally)

cat_v1 <- df_v1[, c("Gender", "SMS_received", "No-show")]
ggpairs(cat_v1)


#################################################
#end of testing seciton 
