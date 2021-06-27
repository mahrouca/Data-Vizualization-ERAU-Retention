library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(readxl)

df1 <-read_excel("AAC_Card_Swipe_Data_new.xlsx", sheet=1)
df1$Date<-format(as.Date(df1$Date),"%Y")
df2 <- group_by(df1,`Person Campus ID`,Date)%>%
  summarize(count=n())%>%
  spread(Date,count)
df2[is.na(df2)] <- 0
write.csv(df2,file="AAC_Card_Swipe_Data_summarized.csv",quote=F,row.names = F)