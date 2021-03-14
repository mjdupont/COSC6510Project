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


df <- read_csv("KaggleV2-May-2016.csv")
#Creates directory if it doesn't exist, otherwise does nothing
dir.create("images", showWarnings = FALSE) 

#summary(df)
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
#Generating Additional Columns:
########################################
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

df$AppointmentDay = as.Date(df$AppointmentDay)
df$ScheduledDate = as.Date(df$ScheduledDay)
df$ScheduledDelay = df$AppointmentDay - df$ScheduledDate

View(df)
#Presumed to be equal to sex?
df$Gender <- as.factor(df$Gender)

#Handicap indicates degree of handicapped status - see discussion in Kaggle post.
#df$Handicap <- as.factor(df$Handicap)

########################################
#End Adding additional columns
########################################

df.Handicap_freq <- df %>% count(Handicap)

df %>% mutate(Total = n()) %>% group_by(No_show) %>% summarise(n = n(), ratio = n()/mean(Total))

#Overall No-Show Percentage
noShowByFactor <- 
  df %>% filter(PhysicalCondition != "No Condition")

#Appointments by Physical condition
NoShowByPhysicalCondition <- ggplot(noShowByFactor, aes(x = No_show, color = PhysicalCondition)) + 
  geom_bar(aes(fill = PhysicalCondition)) +
  labs(title = "Appointments of patients with chronic illness", 
       x = "No-show status", 
       y = "Appointments")
noShowByFactor
ggsave("images/ChronicIllnessAppts.png", height = 3, width = 4.5, units = "in")



# Examining how many patients have a given physical condition, and what proportion 
# of patients with this condition no show, relative to the whole population.

conditions <- df %>% group_by(PhysicalCondition, No_show) %>%
  summarise(n = n()) %>%
  group_by(PhysicalCondition) %>% 
  mutate(Total = sum(n),
         Ratio = n/Total)

conditions.NoShow <- 
  df %>% 
  group_by(PhysicalCondition, No_show) %>%
  summarise(n = n()) %>%
  cast(PhysicalCondition ~ No_show ) %>% 
  tibble %>%
  transmute (PhysicalCondition = PhysicalCondition,
             NoShow = `TRUE`,
             Show = `FALSE`,
             Total = `FALSE`+`TRUE`,
             Ratio= `TRUE`/Total)

#Graphing percentage of no-show appointments by condition
conditions.NoShow %>% 
  ggplot(aes(x = reorder(PhysicalCondition, Ratio), y = Ratio*100)) +
  geom_col(aes(fill = PhysicalCondition)) +
  ylim(0,100) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Percentage of No-Show Appointments by physical condition",
       y = "Percentage of No-Show Appointments")
ggsave("images/PercentageNoShowPhysicalCondition.png", height = 3, width = 4.5, units = "in")

# (trying) to graph Total numbers of each physical condition, split by no-show.
conditions %>% 
  ggplot(aes(x = reorder(PhysicalCondition, Total), y = Total)) + 
  geom_col()
ggsave("images/TotalsOfPhysicalConditions.png", height = 3, width = 4.5, units = "in")


noShowByDatePlot <- df %>% ggplot(aes(x=ScheduledDelay, fill=No_show)) + 
  geom_histogram() +
  labs(title = "No-Show Appointments by Delay since Scheduling",
       y = "Appointments", x="")
ggsave("images/NoShowByDate.png", height=3, width=4.5, units="in")

noShowByDate <- 
  df %>% 
  group_by(ScheduledDelay, No_show) %>% 
  summarise(n=n()) %>%
  cast(castable, ScheduledDelay ~ No_show ) %>% 
  tibble %>%
  transmute (ScheduledDelay = ScheduledDelay,
             NoShow = `TRUE`,
             Show = `FALSE`,
             Total = `FALSE`+`TRUE`,
             Ratio= `TRUE`/Total)
noShowByDate
