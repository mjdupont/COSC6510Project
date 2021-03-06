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

df <- read_csv("data_warehouse/NoShow_Cleaned.csv")

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
  cast(ScheduledDelay ~ No_show ) %>% 
  tibble %>%
  transmute (ScheduledDelay = ScheduledDelay,
             NoShow = `TRUE`,
             Show = `FALSE`,
             Total = `FALSE`+`TRUE`,
             Ratio= `TRUE`/Total)
noShowByDate

