library(readxl)
library(lubridate)
library(chron)
library(tidyverse)
library(stringr)


#4.1

df <- read_excel("~/Collect, Store and Retrieve Data/Week 4/2013 Geographric Coordinate Spreadsheet for U S  Farmers Markets 8'3'1013.xlsx", 
                 skip = 2)

#4.2

df[is.na(df)] <- 0

df <- subset(df, df[,9]!=0) # All blanks/NA's removed.

#Multiple dataframes derived from original dataframe depending upon their date format.

df1<-subset(df,sapply(strsplit(df$Season1Date, "\\s+"), length) == 3)
df2<-subset(df,sapply(strsplit(df$Season1Date, "\\s+"), length) == 7)
df3<-subset(df,sapply(strsplit(df$Season1Date, "\\s+"), length) == 5)
df4<-subset(df,sapply(strsplit(df$Season1Date, "\\s+"), length) == 6)
df5<-subset(df,sapply(strsplit(df$Season1Date, "\\s+"), length) == 8)
df6<-subset(df,sapply(strsplit(df$Season1Date, "\\s+"), length) == 9)

#df1 further divided in 2 parts.
df1a<-subset(df1, str_count(df1$Season1Date, boundary("word")) == 3)
df1b<-subset(df1, str_count(df1$Season1Date, boundary("word")) == 7)


#df1a



df1ap<- word(df1a$Season1Date,1) # First word of string extracted
df1aq<- word(df1a$Season1Date,-1)# Last word of string extracted


p1<<-c() #starting month
q1<<-c() #ending month

#Month identified and replaced by its numeric value. 1 for Jan, 2 for Feb.. so on.

for(i1 in 1:length(df1a$FMID)) {

if (str_detect(df1ap[i1],"January")) {
  
  p1[i1]<-1
}
if (str_detect(df1ap[i1],"February")) {
  
  p1[i1]<-2
}
if (str_detect(df1ap[i1],"March")) {
  
  p1[i1]<-3
}

if (str_detect(df1ap[i1],"April")) {
  
  p1[i1]<-4
}
if (str_detect(df1ap[i1],"May")) {
  
  p1[i1]<-5
}
if (str_detect(df1ap[i1],"June")) {
  
  p1[i1]<-6
}
if (str_detect(df1ap[i1],"July")) {
  
  p1[i1]<-7
}
if (str_detect(df1ap[i1],"August")) {
  
  p1[i1]<-8
}
if (str_detect(df1ap[i1],"September")) {
  
  p1[i1]<-9
}
if (str_detect(df1ap[i1],"October")) {
  
  p1[i1]<-10
}
if (str_detect(df1ap[i1],"November")) {
  
  p1[i1]<-11
}
if (str_detect(df1ap[i1],"December")) {
  
  p1[i1]<-12
}
}
p<-p1


for(j1 in 1:length(df1a$FMID)) {
  
  if (str_detect(df1aq[j1],"January")) {
    
    q1[j1]<-1
  }
  if (str_detect(df1aq[j1],"February")) {
    
    q1[j1]<-2
  }
  if (str_detect(df1aq[j1],"March")) {
    
    q1[j1]<-3
  }
  
  if (str_detect(df1aq[j1],"April")) {
    
    q1[j1]<-4
  }
  if (str_detect(df1aq[j1],"May")) {
    
    q1[j1]<-5
  }
  if (str_detect(df1aq[j1],"June")) {
    
    q1[j1]<-6
  }
  if (str_detect(df1aq[j1],"July")) {
    
    q1[j1]<-7
  }
  if (str_detect(df1aq[j1],"August")) {
    
    q1[j1]<-8
  }
  if (str_detect(df1aq[j1],"September")) {
    
    q1[j1]<-9
  }
  if (str_detect(df1aq[j1],"October")) {
    
    q1[j1]<-10
  }
  if (str_detect(df1aq[j1],"November")) {
    
    q1[j1]<-11
  }
  if (str_detect(df1aq[j1],"December")) {
    
    q1[j1]<-12
  }
}
q<-q1
df14<-cbind(df1a,p,q)
rm(p,q)


#df1b

#Numeric month extracted.

p1b<-month(as.Date.character(df1b$Season1Date[], format = "%m/%d/%Y"))
q1b<-month(as.Date.character(df1b$Season1Date[], format = "%m/%d/%Y to %m/%d/%Y"))

p<-p1b
q<-q1b

df13<-cbind(df1b,p,q) #Numeric start and end months combined with dataframe.

rm(p,q)






#df2
df2[] <- lapply(df2, gsub, pattern = "Oct.", replacement = "October", fixed = TRUE) #Outlier replaced

p<-0
q<-0

#Numeric of month extracted.

for(i in 1:60) {
  p[i]<-month(as.Date.character(df2[i,9],'%B %d,%Y'))
  q[i]<-month(as.Date.character(df2[i,9],'%B %d,%Y to %B %d,%Y'))
  is.na(q[i]) 
  q[i]<-month(as.Date.character(df2[i,9],'%B %d,%Y to %b %d,%Y'))
}
q[is.na(q)] <- 9 #Outlier
df8<-cbind(df2,p,q)

rm(p,q)






#df3


i3<<-1
p3<-c()
for(i3 in 1:13) {
  if (str_detect(df3[i3,9],"April")) {
    
    p3[i3]<-4
  }
  if (str_detect(df3[i3,9],"May")) {
    
    p3[i3]<-5
  }
  if (str_detect(df3[i3,9],"June")) {
    
    p3[i3]<-6
  }
  if (str_detect(df3[i3,9],"July")) {
    
    p3[i3]<-7
  }
  
  
}
j3<<-1
q3<<-c()
for(j3 in 1:13) {
  if (str_detect(df3[j3,9],"August")) {
    
    q3[j3]<-8
  }
  if (str_detect(df3[j3,9],"September")) {
    
    q3[j3]<-9
  }
  if (str_detect(df3[j3,9],"Sept")) {
    
    q3[j3]<-9
  }
  if (str_detect(df3[j3,9],"October")) {
    
    q3[j3]<-10
  }
  if (str_detect(df3[j3,9],"November")) {
    
    q3[j3]<-11
  }
  if (str_detect(df3[j3,9],"December")) {
    
    q3[j3]<-12
  }
  
  
}

p<-p3
q<-q3
df12<-cbind(df3,p,q)

rm(p,q)




#df4

p4<-month(as.Date.character(df4[1,9],'%B %d,%Y'))
q4<-month(as.Date.character(df4[1,9],'%B %d,%Y to %B %d,%Y'))

p<-p4
q<-q4
df9<-cbind(df4,p,q)

rm(p,q)




#df5
p5<-0
q5<-0
for(j in 1:4) {
  p5[j]<-month(as.Date.character(df5[j,9],'%B %d , %Y'))
  q5[j]<-month(as.Date.character(df5[j,9],'%B %d , %Y to %B %d , %Y'))
}
q5[is.na(q5)] <- 9 #Outlier

p<-p5
q<-q5
df10<-cbind(df5,p,q)

rm(p5,q5)






#df6
p6<-6
q6<-10

p<-p6
q<-q6
df11<-cbind(df6,p,q)

rm(p,q)





dff<-rbind(df8,df9,df10,df11,df12,df13,df14)

#Spring: 1-3
#Summer: 4-6
#Fall  : 7-9
#Winter: 10-12
#Markets are categorized on their starting month season.
#Markets running for 6 months all classified as half-yearly. (or every ODD multiple of 6)
#Markets running for 12 months are classified as yearly. (or every multiple of 12)
#Markets running for 2,3,4,5,7,8,9,10,11 months are classified as per their starting season, since they cannot be yearly or half-yearly.
#Markets running for more than 12 are classified as per their starting season.

season<-c()
i<-0
for (i in 1:length(dff$FMID)) {
  
  if (dff$p[i]==1) {
    if (dff$q[i]==1 | dff$q[i]==2 |dff$q[i]==3 |dff$q[i]==4 |dff$q[i]==5 | dff$q[i]==7 |dff$q[i]==8 |dff$q[i]==9 |dff$q[i]==10 |dff$q[i]==11) season[i]<-"Spring"
    if (dff$q[i]==6)  season[i]<-"Half-Yearly"
    if (dff$q[i]==12) season[i]<-"Yearly"
  }
  
  if (dff$p[i]==2) {
    if (dff$q[i]==12 | dff$q[i]==2 |dff$q[i]==3 |dff$q[i]==4 |dff$q[i]==5 | dff$q[i]==6 |dff$q[i]==8 |dff$q[i]==9 |dff$q[i]==10 |dff$q[i]==11) season[i]<-"Spring"
    if (dff$q[i]==7)  season[i]<-"Half-Yearly"
    if (dff$q[i]==1) season[i]<-"Yearly"
  }
  
  if (dff$p[i]==3) {
    if (dff$q[i]==12 | dff$q[i]==1 |dff$q[i]==3 |dff$q[i]==4 |dff$q[i]==5 | dff$q[i]==6 |dff$q[i]==7 |dff$q[i]==9 |dff$q[i]==10 |dff$q[i]==11) season[i]<-"Spring"
    if (dff$q[i]==8)  season[i]<-"Half-Yearly"
    if (dff$q[i]==2) season[i]<-"Yearly"
  }
  
  if (dff$p[i]==4) {
    if (dff$q[i]==12 | dff$q[i]==1 |dff$q[i]==2 |dff$q[i]==4 |dff$q[i]==5 | dff$q[i]==6 |dff$q[i]==7 |dff$q[i]==8 |dff$q[i]==10 |dff$q[i]==11) season[i]<-"Summer"
    if (dff$q[i]==9)  season[i]<-"Half-Yearly"
    if (dff$q[i]==3) season[i]<-"Yearly"
  }
  
  if (dff$p[i]==5) {
    if (dff$q[i]==12 | dff$q[i]==1 |dff$q[i]==2 |dff$q[i]==3 |dff$q[i]==5 | dff$q[i]==6 |dff$q[i]==7 |dff$q[i]==8 |dff$q[i]==9|dff$q[i]==11 ) season[i]<-"Summer"
    if (dff$q[i]==10)  season[i]<-"Half-Yearly"
    if (dff$q[i]==4) season[i]<-"Yearly"
  }
  
  if (dff$p[i]==6) {
    if (dff$q[i]==12 | dff$q[i]==1 |dff$q[i]==2 |dff$q[i]==3 |dff$q[i]==4 | dff$q[i]==6 |dff$q[i]==7 |dff$q[i]==8 |dff$q[i]==9|dff$q[i]==10 ) season[i]<-"Summer"
    if (dff$q[i]==11)  season[i]<-"Half-Yearly"
    if (dff$q[i]==5) season[i]<-"Yearly"
  }
  
  if (dff$p[i]==7) {
    if (dff$q[i]==11 | dff$q[i]==1 |dff$q[i]==2 |dff$q[i]==3 |dff$q[i]==4 | dff$q[i]==5 |dff$q[i]==7 |dff$q[i]==8 |dff$q[i]==9|dff$q[i]==10 ) season[i]<-"Fall"
    if (dff$q[i]==12)  season[i]<-"Half-Yearly"
    if (dff$q[i]==6) season[i]<-"Yearly"
  }
  
  if (dff$p[i]==8) {
    if (dff$q[i]==11 | dff$q[i]==12 |dff$q[i]==2 |dff$q[i]==3 |dff$q[i]==4 | dff$q[i]==5 |dff$q[i]==6 |dff$q[i]==8 |dff$q[i]==9|dff$q[i]==10 ) season[i]<-"Fall"
    if (dff$q[i]==1)  season[i]<-"Half-Yearly"
    if (dff$q[i]==7) season[i]<-"Yearly"
  }
  
  if (dff$p[i]==9) {
    if (dff$q[i]==11 | dff$q[i]==12 |dff$q[i]==1 |dff$q[i]==3 |dff$q[i]==4 | dff$q[i]==5 |dff$q[i]==6 |dff$q[i]==7 |dff$q[i]==9|dff$q[i]==10 ) season[i]<-"Fall"
    if (dff$q[i]==2)  season[i]<-"Half-Yearly"
    if (dff$q[i]==8) season[i]<-"Yearly"
  }
  
  if (dff$p[i]==10) {
    if (dff$q[i]==11 | dff$q[i]==12 |dff$q[i]==1 |dff$q[i]==2 |dff$q[i]==4 | dff$q[i]==5 |dff$q[i]==6 |dff$q[i]==7 |dff$q[i]==8|dff$q[i]==10 ) season[i]<-"Winter"
    if (dff$q[i]==3)  season[i]<-"Half-Yearly"
    if (dff$q[i]==9) season[i]<-"Yearly"
  }
 
  if (dff$p[i]==11) {
    if (dff$q[i]==11 | dff$q[i]==12 |dff$q[i]==1 |dff$q[i]==2 |dff$q[i]==3 | dff$q[i]==5 |dff$q[i]==6 |dff$q[i]==7 |dff$q[i]==8|dff$q[i]==9 ) season[i]<-"Winter"
    if (dff$q[i]==4)  season[i]<-"Half-Yearly"
    if (dff$q[i]==10) season[i]<-"Yearly"
  }
  
  if (dff$p[i]==12) {
    if (dff$q[i]==10 | dff$q[i]==11 |dff$q[i]==12 |dff$q[i]==2 |dff$q[i]==3 | dff$q[i]==4 |dff$q[i]==6 |dff$q[i]==7 |dff$q[i]==8|dff$q[i]==9 ) season[i]<-"Winter"
    if (dff$q[i]==5)  season[i]<-"Half-Yearly"
    if (dff$q[i]==1) season[i]<-"Yearly"
  }
  
}
  
dfWithSeasons<-cbind(dff,season)

View(dfWithSeasons)


#Plot starting month and duration of the market

ggplot(data = dfWithSeasons) + 
  geom_point(mapping = aes(x = p, y = q), color = "red", position = 'jitter') + 
  facet_wrap(~ season, nrow = 3) + xlim(0,13) + ylim(0,13) + labs(title = " Start, end and season analysis", x = 'Starting Month', y='Ending Month')


ggplot(data = dfWithSeasons) + 
  geom_bar(mapping = aes(x = p, fill = season)) + xlim(0,13) + labs(title = "Starting month and market duration", x = 'Starting Month')


# State and its market


stateMarket <- function(stateName) {
    
  dfState<-subset(dfWithSeasons, dfWithSeasons$State == stateName)
  
  return(dfState)
}


dfStateWithSeasons <- stateMarket('Massachusetts')

ggplot(data = dfStateWithSeasons) + 
  geom_bar(mapping = aes(x = p, fill = season))  + xlim(0,13) + labs(title = "Starting month and market duration for state", x = 'Starting Month')
