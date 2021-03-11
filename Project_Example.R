library(tidyverse)

df <- read_csv("KaggleV2-May-2016.csv")

summary(df)

df$`No-show` <- +(df$`No-show`=="Yes")
df$`No-show` <- as.logical(df$`No-show`)
#Whether the patient is receiving financial assistance.
df$Scholarship <- as.logical(df$Scholarship)
#Presence of Hypertension
df$Hipertension <- as.logical(df$Hipertension)
#Presence of Diabetes
df$Diabetes <- as.logical(df$Hipertension)
#Presence of Alcoholism
df$Alcoholism <- as.logical(df$Alcoholism)
#One OR MORE SMS messages about appointment received.
df$SMS_received <- as.logical(df$SMS_received)

View(df)
#Presumed to be equal to sex?
df$Gender <- as.factor(df$Gender)
#Handicap indicates degree of handicapped status - see discussion in Kaggle post.
df$Handcap <- as.factor(df$Handcap)

df_v1 <- df

View(df_v1)
write_csv(df_v1, "project_draft1.csv")
write_excel_csv(df_v1, "project_draft1.1.csv")