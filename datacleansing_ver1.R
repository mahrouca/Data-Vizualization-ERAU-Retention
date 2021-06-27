library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(readxl)
not_all_na <- function(x) any(!is.na(x))#logic expression:any not all NA

retention <-read_excel("retention.xlsx", sheet=1)%>%
  filter(cohort==2013)%>%
  select_if(not_all_na)#select_if for selecting columns whic don't have all NA by applying above logic expression


retention$SATM_fromACT<-0
for (i in 1:nrow(retention)){
  if (!is.na(retention$ActMathScore[i])) {
    retention$SATM_fromACT[i] <- switch(retention$ActMathScore[i]-9,260,280,310,330,360,400,430,470,500,510,520,530,540,
                                        560,580,590,610,640,660,680,700,710,720,740,760,780,800)
  }
}

retention$SATERW_fromACT<-0
for (i in 1:nrow(retention)){
  if (!is.na(retention$ActEnglScore[i])&!is.na(retention$ActReadScore[i])) {
    retention$SATERW_fromACT[i] <- switch(retention$ActEnglScore[i]+retention$ActReadScore[i]-13,280,290,300,310,320,330,
                                        340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,500,510,520,
                                        530,540,540,550,560,570,580,580,590,600,610,610,620,630,630,640,640,650,660,660,
                                        670,680,680,690,700,700,710,720,730,740,750,770,790)
  }
}

retention$SatERWScoreNew<-retention$SatScoreNew-retention$SatMathScoreNew#some cases don't have SatERWScoreNew but have the other two variables

retention$SatMathScoreNew[is.na(retention$SatMathScoreNew)]<-0 
retention$SatERWScoreNew[is.na(retention$SatERWScoreNew)]<-0 
retention <- mutate(retention,SATM_Superset = ifelse(SATM_fromACT>SatMathScoreNew,SATM_fromACT,SatMathScoreNew))%>%
  mutate(SATERW_Superset = ifelse(SATERW_fromACT>SatERWScoreNew,SATERW_fromACT,SatERWScoreNew))%>%
  mutate(SAT_Superset=SATERW_Superset+SATM_Superset)%>%
  filter(SATERW_Superset!=0&SatMathScoreNew!=0&HSConvertedGpa!=0)%>%
  select(-ActCompScore,-ActEnglScore,-ActMathScore,-ActReadScore,-ActScienceScore,-SatERWScore.ConvertedFlag,
         -SatERWScoreNew,-SATM_fromACT,-SATERW_fromACT,-SatMathScore,-SatMathScore.ConvertedFlag,-SatMathScoreNew,
         -SatScore.ConvertedFlag,-SatScoreNew,-SatVerbalScore,-SatWritingScore,-SAT)

idx <- c()
n <- 1
for (i in 1:ncol(retention)){
  if (length(unique(retention[[i]]))==1) {
    idx[n]<-i
    n<-n+1
  }
}
retention <- retention[,-idx]
