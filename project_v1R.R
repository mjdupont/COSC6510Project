library(tidyverse)

df <- read_csv("C:/Users/m-mar/Desktop/R/project_v1.csv")

df$`No-show` <- +(df$`No-show`=="Yes")

df$`No-show` <- as.logical(df$`No-show`)

df$Scholarship <- as.logical(df$Scholarship)

df$Hipertension <- as.logical(df$Hipertension)

df$Diabetes <- as.logical(df$Hipertension)

df$Alcoholism <- as.logical(df$Alcoholism)

df$SMS_received <- as.logical(df$SMS_received)

View(df)

df$Gender <- as.factor(df$Gender)

df$Neighbourhood <- as.factor(df$Neighbourhood)

df$Handcap <- as.factor(df$Handcap)

df_v1 <- df

View(df_v1)

write_csv(df_v1, "project_draft1.csv")
write_excel_csv(df_v1, "project_draft1.1.csv")
