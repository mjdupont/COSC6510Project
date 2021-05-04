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


noShowByDelayPlot <- df %>% ggplot(aes(x=ScheduledDelay, fill=No_show)) + 
  geom_histogram() +
  labs(title = "No-Show Appointments by Delay since Scheduling",
       y = "Appointments", x="")
ggsave("images/NoShowByDelay.png", height=3, width=4.5, units="in")

noShowByDelay <- 
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
noShowByDelay



df$weekdayOfAppt <- as.factor(weekdays(df$AppointmentDay))
df$monthOfAppt <- as.factor(format(df$AppointmentDay, "%m"))


noShowByWeekDay <-
  df %>% group_by(weekdayOfAppt, No_show) %>%
  summarise(n=n()) %>%
  cast(weekdayOfAppt ~ No_show) %>%
  tibble %>%
  transmute(weekdayOfAppt = weekdayOfAppt,
            NoShow = `TRUE`,
            Show = `FALSE`,
            Total = `FALSE` + `TRUE`,
            Ratio = `TRUE`/Total)
noShowByWeekDay

noShowByWeekDayPlot <- noShowByWeekDay %>%
  ggplot(aes(x=reorder(weekdayOfAppt, Ratio), y=Ratio)) +
  geom_col()
noShowByWeekDayPlot

noShowBySex <-
  df %>% group_by(Gender, No_show) %>%
  summarise(n=n()) %>%
  cast(Gender ~ No_show) %>%
  tibble %>%
  transmute(Gender = Gender,
            NoShow = `TRUE`,
            Show = `FALSE`,
            Total = `FALSE` + `TRUE`,
            Ratio = `TRUE`/Total)
noShowBySex

noShowBySexPlot <- noShowBySex %>%
  ggplot(aes(x=reorder(Gender, Ratio), y=Ratio)) +
  geom_col()
noShowBySexPlot

df$Neighbourhood <- as.factor(iconv(df$Neighbourhood))

noShowByNeighbourhood <-
  df %>% group_by(Neighbourhood, No_show) %>%
  summarise(n=n()) %>%
  cast(Neighbourhood ~ No_show) %>%
  tibble %>%
  transmute(Neighbourhood = Neighbourhood,
            NoShow = `TRUE`,
            Show = `FALSE`,
            Total = `FALSE` + `TRUE`,
            Ratio = `TRUE`/Total) %>%
  na.omit()
noShowByNeighbourhood

noShowByNeighbourhoodPlot <- noShowByNeighbourhood %>%
  ggplot(aes(x=reorder(Neighbourhood, Ratio), y=Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))
noShowByNeighbourhoodPlot

noShowByNeighbourhoodPlotAbs <- noShowByNeighbourhood %>%
  ggplot(aes(x=reorder(Neighbourhood, Total), y=Total, fill = Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))
noShowByNeighbourhoodPlotAbs

noShowByDate <- df %>%
  group_by(AppointmentDay, No_show) %>%
  count() %>%
  cast(AppointmentDay ~ No_show) %>%
  tibble %>%
  transmute(
    Date = AppointmentDay,
    NoShow = `TRUE`,
    Show = `FALSE`,
    Total = Show + NoShow,
    Ratio = NoShow/Total
  )

allDates <- seq.Date(
  min(noShowByDate$Date),
  max(noShowByDate$Date),
  "day")

allValues <- merge(
  x=data.frame(Date=allDates),
  y=noShowByDate,
  all.x=TRUE)

allValues$NoShow <- coalesce(allValues$NoShow, 0)
allValues$Show <- coalesce(allValues$Show, 0)
allValues$Total <- coalesce(allValues$Total, 0)
allValues$Ratio <- coalesce(allValues$Ratio, 0)

dateTS <- ts(allValues$NoShow, start=c(1,4), end=c(1,44), 7)
decomposition <- decompose(dateTS)
plot(decomposition)

dateTSTotal <- ts(allValues$Total, start=c(1,4), end=c(1,44), 7)
decompositionTotal <- decompose(dateTSTotal)
plot(decomposition)

RatioTS <- ts(allValues$Ratio, start=c(1,4), end=c(1,44), 7)
decompositionRatio <- decompose(RatioTS)
plot(decompositionRatio)

