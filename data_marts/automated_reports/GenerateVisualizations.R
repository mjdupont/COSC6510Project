
args <- commandArgs(trailingOnly=TRUE)
args

working.dir <- args[1]


#if(!require('tidyverse')) {
#  install.packages('tidyverse') 
#  library(tidyverse)
#}

#if(!require('reshape')) {
#  install.packages('reshape')
#  library(reshape)
#}

#if(!require('chron')) {
#  install.packages('chron')
#  library(chron)
#}

#if(!require('usethis')) {
#  install.packages('usethis')
#  library(usethis)
#}

usethis::edit_r_environ('project')
user_renviron = path.expand(file.path("~", ".Renviron"))

df <- read_csv("../../data_warehouse/NoShow_Cleaned.csv")

startDate <- as.Date('2016-05-01')
endDate <- as.Date('2016-05-30')

df.subset <- df %>% filter(AppointmentDay > startDate & AppointmentDay < endDate)

df.subset.apptsByDate <- df.subset %>%
  group_by(AppointmentDay, No_show) %>%
  count() 

df.subset.apptsByDate %>% 
  ggplot(aes(x=AppointmentDay, y=n, color = No_show)) +
  geom_point() +
  geom_line()

df.subset.apptsExpanded <- df.subset.apptsByDate %>%
  cast(AppointmentDay ~ No_show) %>% 
  tibble %>%
  transmute(
    AppointmentDay = AppointmentDay,
    NoShow = `TRUE`,
    Show = `FALSE`,
    Total = `TRUE`+`FALSE`,
    Ratio = `TRUE`/Total
  )

df.subset.apptsExpanded %>% ggplot(aes(x=AppointmentDay, y=Ratio)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits=c(0,1)) 
