library(data.table)
library(dplyr)
library(RSQLite)
library(tidyverse)
library(lubridate)
library(chron)
library(plyr)

mydata<-fread('https://www.google.org/flutrends/about/data/flu/data.txt')

# Omit NAs to avoid inconsistency.
mydata<-na.omit(mydata)
mydata<-rename(mydata, c("New Zealand"="NewZealand", "South Africa"="SouthAfrica","United States"="UnitedStates"))

# Removing data for year 2015 because of inconsistency.
trainData <- subset(mydata,year(mydata$Date)<2014)
testData  <- subset(mydata,year(mydata$Date)==2014)


# Converting date format
monthInsertion<-function(dataframe) {
  
    Month<-0
    
    for(i in 1:nrow(dataframe)){
    
    Month[i]<-month(as.Date.character(dataframe[i,1],format='%Y-%m-%d'))
    
    }
 
  tempDF<-subset(dataframe[,-1])
  return(cbind(Month,tempDF))
}

trainMonthData <- monthInsertion(trainData)

testMonthData <- monthInsertion(testData)

# aggregating data for individual countries.
trainArgentina <- aggregate(Argentina ~ Month, trainMonthData, sum)
trainAustralia <- aggregate(Australia ~ Month, trainMonthData, sum)
trainAustria <- aggregate(Austria ~ Month, trainMonthData, sum)
trainBelgium <- aggregate(Belgium ~ Month, trainMonthData, sum)
trainBolivia <- aggregate(Bolivia ~ Month, trainMonthData, sum)
trainBrazil <- aggregate(Brazil ~ Month, trainMonthData, sum)
trainBulgaria <- aggregate(Bulgaria ~ Month, trainMonthData, sum)
trainCanada <- aggregate(Canada ~ Month, trainMonthData, sum)
trainChile <- aggregate(Chile ~ Month, trainMonthData, sum)
trainFrance <- aggregate(France ~ Month, trainMonthData, sum)
trainHungary <- aggregate(Hungary ~ Month, trainMonthData, sum)
trainGermany <- aggregate(Germany ~ Month, trainMonthData, sum)
trainJapan <- aggregate(Japan ~ Month, trainMonthData, sum)
trainMexico <- aggregate(Mexico ~ Month, trainMonthData, sum)
trainNetherlands <- aggregate(Netherlands ~ Month, trainMonthData, sum)
trainNewZealand <- aggregate(NewZealand ~ Month, trainMonthData, sum)
trainNorway <- aggregate(Norway ~ Month, trainMonthData, sum)
trainParaguay <- aggregate(Paraguay ~ Month, trainMonthData, sum)
trainPeru <- aggregate(Peru ~ Month, trainMonthData, sum)
trainPoland <- aggregate(Poland ~ Month, trainMonthData, sum)
trainRomania <- aggregate(Romania ~ Month, trainMonthData, sum)
trainRussia <- aggregate(Russia ~ Month, trainMonthData, sum)
trainSouthAfrica <- aggregate(SouthAfrica ~ Month, trainMonthData, sum)
trainSpain <- aggregate(Spain ~ Month, trainMonthData, sum)
trainSweden <- aggregate(Sweden ~ Month, trainMonthData, sum)
trainSwitzerland <- aggregate(Switzerland ~ Month, trainMonthData, sum)
trainUkraine <- aggregate(Ukraine ~ Month, trainMonthData, sum)
trainUnitedStates <- aggregate(UnitedStates ~ Month, trainMonthData, sum)
trainUruguay <- aggregate(Uruguay ~ Month, trainMonthData, sum)






testArgentina <- aggregate(Argentina ~ Month, testMonthData, sum)
testAustralia <- aggregate(Australia ~ Month, testMonthData, sum)
testAustria <- aggregate(Austria ~ Month, testMonthData, sum)
testBelgium <- aggregate(Belgium ~ Month, testMonthData, sum)
testBolivia <- aggregate(Bolivia ~ Month, testMonthData, sum)
testBrazil <- aggregate(Brazil ~ Month, testMonthData, sum)
testBulgaria <- aggregate(Bulgaria ~ Month, testMonthData, sum)
testCanada <- aggregate(Canada ~ Month, testMonthData, sum)
testChile <- aggregate(Chile ~ Month, testMonthData, sum)
testFrance <- aggregate(France ~ Month, testMonthData, sum)
testHungary <- aggregate(Hungary ~ Month, testMonthData, sum)
testGermany <- aggregate(Germany ~ Month, testMonthData, sum)
testJapan <- aggregate(Japan ~ Month, testMonthData, sum)
testMexico <- aggregate(Mexico ~ Month, testMonthData, sum)
testNetherlands <- aggregate(Netherlands ~ Month, testMonthData, sum)
testNewZealand <- aggregate(NewZealand ~ Month, testMonthData, sum)
testNorway <- aggregate(Norway ~ Month, testMonthData, sum)
testParaguay <- aggregate(Paraguay ~ Month, testMonthData, sum)
testPeru <- aggregate(Peru ~ Month, testMonthData, sum)
testPoland <- aggregate(Poland ~ Month, testMonthData, sum)
testRomania <- aggregate(Romania ~ Month, testMonthData, sum)
testRussia <- aggregate(Russia ~ Month, testMonthData, sum)
testSouthAfrica <- aggregate(SouthAfrica ~ Month, testMonthData, sum)
testSpain <- aggregate(Spain ~ Month, testMonthData, sum)
testSweden <- aggregate(Sweden ~ Month, testMonthData, sum)
testSwitzerland <- aggregate(Switzerland ~ Month, testMonthData, sum)
testUkraine <- aggregate(Ukraine ~ Month, testMonthData, sum)
testUnitedStates <- aggregate(UnitedStates ~ Month, testMonthData, sum)
testUruguay <- aggregate(Uruguay ~ Month, testMonthData, sum)

# Creating tables for individual countries.

Argentina<-cbind(trainArgentina,testArgentina[2],'SouthAmerica')
colnames(Argentina) <- c("Month","Train","Test","Continent")
Argentina[,2]<-ceiling(Argentina[,2]/8)

Australia<-cbind(trainAustralia,testAustralia[2],'Oceania')
colnames(Australia) <- c("Month","Train","Test","Continent")
Australia[,2]<-ceiling(Australia[,2]/8)

Austria<-cbind(trainAustria,testAustria[2],'Europe')
colnames(Austria) <- c("Month","Train","Test","Continent")
Austria[,2]<-ceiling(Austria[,2]/8)

Belgium<-cbind(trainBelgium,testBelgium[2],'Europe')
colnames(Belgium) <- c("Month","Train","Test","Continent")
Belgium[,2]<-ceiling(Belgium[,2]/8)

Bolivia<-cbind(trainBolivia,testBolivia[2],'SouthAmerica')
colnames(Bolivia) <- c("Month","Train","Test","Continent")
Bolivia[,2]<-ceiling(Bolivia[,2]/8)

Brazil<-cbind(trainBrazil,testBrazil[2],'SouthAmerica')
colnames(Brazil) <- c("Month","Train","Test","Continent")
Brazil[,2]<-ceiling(Brazil[,2]/8)

Bulgaria<-cbind(trainBulgaria,testBulgaria[2],'Europe')
colnames(Bulgaria) <- c("Month","Train","Test","Continent")
Bulgaria[,2]<-ceiling(Bulgaria[,2]/8)

Canada<-cbind(trainCanada,testCanada[2],'NorthAmerica')
colnames(Canada) <- c("Month","Train","Test","Continent")
Canada[,2]<-ceiling(Canada[,2]/8)

Chile<-cbind(trainChile,testChile[2],'SouthAmerica')
colnames(Chile) <- c("Month","Train","Test","Continent")
Chile[,2]<-ceiling(Chile[,2]/8)

France<-cbind(trainFrance,testFrance[2],'Europe')
colnames(France) <- c("Month","Train","Test","Continent")
France[,2]<-ceiling(France[,2]/8)

Germany<-cbind(trainGermany,testGermany[2],'Europe')
colnames(Germany) <- c("Month","Train","Test","Continent")
Germany[,2]<-ceiling(Germany[,2]/8)

Hungary<-cbind(trainHungary,testHungary[2],'Europe')
colnames(Hungary) <- c("Month","Train","Test","Continent")
Hungary[,2]<-ceiling(Hungary[,2]/8)

Japan<-cbind(trainJapan,testJapan[2],'Asia')
colnames(Japan) <- c("Month","Train","Test","Continent")
Japan[,2]<-ceiling(Japan[,2]/8)

Mexico<-cbind(trainMexico,testMexico[2],'NorthAmerica')
colnames(Mexico) <- c("Month","Train","Test","Continent")
Mexico[,2]<-ceiling(Mexico[,2]/8)

Netherlands<-cbind(trainNetherlands,testNetherlands[2],'Europe')
colnames(Netherlands) <- c("Month","Train","Test","Continent")
Netherlands[,2]<-ceiling(Netherlands[,2]/8)

NewZealand<-cbind(trainNewZealand,testNewZealand[2],'Oceania')
colnames(NewZealand) <- c("Month","Train","Test","Continent")
NewZealand[,2]<-ceiling(NewZealand[,2]/8)

Norway<-cbind(trainNorway,testNorway[2],'Europe')
colnames(Norway) <- c("Month","Train","Test","Continent")
Norway[,2]<-ceiling(Norway[,2]/8)

Paraguay<-cbind(trainParaguay,testParaguay[2],'SouthAmerica')
colnames(Paraguay) <- c("Month","Train","Test","Continent")
Paraguay[,2]<-ceiling(Paraguay[,2]/8)

Peru<-cbind(trainPeru,testPeru[2],'SouthAmerica')
colnames(Peru) <- c("Month","Train","Test","Continent")
Peru[,2]<-ceiling(Peru[,2]/8)

Poland<-cbind(trainPoland,testPoland[2],'Europe')
colnames(Poland) <- c("Month","Train","Test","Continent")
Poland[,2]<-ceiling(Poland[,2]/8)

Romania<-cbind(trainRomania,testRomania[2],'Europe')
colnames(Romania) <- c("Month","Train","Test","Continent")
Romania[,2]<-ceiling(Romania[,2]/8)

Russia<-cbind(trainRussia,testRussia[2],'Europe')
colnames(Russia) <- c("Month","Train","Test","Continent")
Russia[,2]<-ceiling(Russia[,2]/8)

SouthAfrica<-cbind(trainSouthAfrica,testSouthAfrica[2],'Africa')
colnames(SouthAfrica) <- c("Month","Train","Test","Continent")
SouthAfrica[,2]<-ceiling(SouthAfrica[,2]/8)

Spain<-cbind(trainSpain,testSpain[2],'Europe')
colnames(Spain) <- c("Month","Train","Test","Continent")
Spain[,2]<-ceiling(Spain[,2]/8)

Sweden<-cbind(trainSweden,testSweden[2],'Europe')
colnames(Sweden) <- c("Month","Train","Test","Continent")
Sweden[,2]<-ceiling(Sweden[,2]/8)

Switzerland<-cbind(trainSwitzerland,testSwitzerland[2],'Europe')
colnames(Switzerland) <- c("Month","Train","Test","Continent")
Switzerland[,2]<-ceiling(Switzerland[,2]/8)

Ukraine<-cbind(trainUkraine,testUkraine[2],'Europe')
colnames(Ukraine) <- c("Month","Train","Test","Continent")
Ukraine[,2]<-ceiling(Ukraine[,2]/8)

UnitedStates<-cbind(trainUnitedStates,testUnitedStates[2],'NorthAmerica')
colnames(UnitedStates) <- c("Month","Train","Test","Continent")
UnitedStates[,2]<-ceiling(UnitedStates[,2]/8)

Uruguay<-cbind(trainUruguay,testUruguay[2],'SouthAmerica')
colnames(Uruguay) <- c("Month","Train","Test","Continent")
Uruguay[,2]<-ceiling(Uruguay[,2]/8)

# combining data from all countries to perform continental analysis

continentData<-rbind(Argentina,Australia,Austria,Belgium,Bolivia,Brazil,Bulgaria,Canada,Chile,France,Germany,Hungary,Japan,Mexico,Netherlands,NewZealand,Norway,Paraguay,Peru,Poland,Romania,Russia,SouthAfrica,Spain,Sweden,Switzerland,Ukraine,UnitedStates,Uruguay)


groupedContinentData<-continentData%>%group_by(Month,Continent)%>%summarise_each(funs(sum))

# Creating a DATABASE

db<-dbConnect(SQLite(),dbname="db")

dbWriteTable(conn = db, name = "Argentinadb", value = Argentina, row.names = FALSE)
dbWriteTable(conn = db, name = "Australiadb", value = Australia, row.names = FALSE)
dbWriteTable(conn = db, name = "Austriadb", value = Austria, row.names = FALSE)
dbWriteTable(conn = db, name = "Belgiumdb", value = Belgium, row.names = FALSE)
dbWriteTable(conn = db, name = "Boliviadb", value = Bolivia, row.names = FALSE)
dbWriteTable(conn = db, name = "Brazildb", value = Brazil, row.names = FALSE)
dbWriteTable(conn = db, name = "Bulgariadb", value = Bulgaria, row.names = FALSE)
dbWriteTable(conn = db, name = "Canadadb", value = Canada, row.names = FALSE)
dbWriteTable(conn = db, name = "Chiledb", value = Chile, row.names = FALSE)
dbWriteTable(conn = db, name = "Francedb", value = France, row.names = FALSE)
dbWriteTable(conn = db, name = "Germanydb", value = Germany, row.names = FALSE)
dbWriteTable(conn = db, name = "Hungarydb", value = Hungary, row.names = FALSE)
dbWriteTable(conn = db, name = "Japandb", value = Japan, row.names = FALSE)
dbWriteTable(conn = db, name = "Mexicodb", value = Mexico, row.names = FALSE)
dbWriteTable(conn = db, name = "Netherlandsdb", value = Netherlands, row.names = FALSE)
dbWriteTable(conn = db, name = "NewZealanddb", value = NewZealand, row.names = FALSE)
dbWriteTable(conn = db, name = "Norwaydb", value = Norway, row.names = FALSE)
dbWriteTable(conn = db, name = "Paraguaydb", value = Paraguay, row.names = FALSE)
dbWriteTable(conn = db, name = "Perudb", value = Peru, row.names = FALSE)
dbWriteTable(conn = db, name = "Polanddb", value = Poland, row.names = FALSE)
dbWriteTable(conn = db, name = "Romaniadb", value = Romania, row.names = FALSE)
dbWriteTable(conn = db, name = "Russiadb", value = Russia, row.names = FALSE)
dbWriteTable(conn = db, name = "SouthAfricadb", value = SouthAfrica, row.names = FALSE)
dbWriteTable(conn = db, name = "Spaindb", value = Spain, row.names = FALSE)
dbWriteTable(conn = db, name = "Swedendb", value = Sweden, row.names = FALSE)
dbWriteTable(conn = db, name = "Switzerlanddb", value = Switzerland, row.names = FALSE)
dbWriteTable(conn = db, name = "Ukrainedb", value = Ukraine, row.names = FALSE)
dbWriteTable(conn = db, name = "UnitedStatesdb", value = UnitedStates, row.names = FALSE)
dbWriteTable(conn = db, name = "Uruguaydb", value = Uruguay, row.names = FALSE)

setOldClass(c("grouped_df", "tbl_df", "data.frame"))
dbWriteTable(conn = db, name = "groupedContinentDatadb", value = groupedContinentData, row.names = FALSE)


# line graphs using data from the database
  
ggplot(data = dbGetQuery(db, "Select * from UnitedStatesdb")) + 
  geom_line(mapping = aes(x = Month, y = Test), color = "Red") + 
  geom_line(mapping = aes(x = Month, y = Train), color = "Blue", show.legend = TRUE) +
  xlim(1,13) + labs(y="Actual=Red, Prediction=Blue", title='Country')

ggplot(data = dbGetQuery(db,"Select * from groupedContinentDatadb WHERE Continent IN ('Europe','SouthAmerica') GROUP BY Month")) + 
  geom_line(mapping = aes(x = Month, y = Test), color = "Red") + 
  geom_line(mapping = aes(x = Month, y = Train), color = "Blue", show.legend = TRUE) + 
  xlim(1,13) + labs(y="Actual=Red, Prediction=Blue", title='Continent')

ggplot(data = dbGetQuery(db,"Select Month, SUM(Train) AS Train, SUM(Test) AS Test from groupedContinentDatadb GROUP BY Month")) + 
  geom_line(mapping = aes(x = Month, y = Test), color = "Red") + 
  geom_line(mapping = aes(x = Month, y = Train), color = "Blue", show.legend = TRUE) +
  xlim(1,13) + labs(y="Actual=Red, Prediction=Blue",title='World') 

ggplot(data = dbGetQuery(db,"Select * from groupedContinentDatadb")) + 
  geom_line(mapping = aes(x = Month, y = Test), color = "Red") + 
  geom_line(mapping = aes(x = Month, y = Train), color = "Blue", show.legend = TRUE) +
  xlim(1,13) + labs(y="Actual=Red, Prediction=Blue",title='Continent Summary') +
  facet_wrap(~ Continent, nrow = 3)


# calculating R-squared for the test~train.

model1<-lm(Test~Train,data=groupedContinentData)
summary(model1)


worldData1<-aggregate(groupedContinentData[,3:4], by=list(groupedContinentData$Month), FUN=sum)

model3<-lm(Test~Train, data=worldData1)
summary(model3)


#*********************************************************************************************#


#                                      APPROACH 2                                             #

# library(data.table)
# library(dplyr)
# library(RSQLite)
# library(tidyverse)
# library(lubridate)
# library(chron)
# library(plyr)
# 
# mydata<-fread('https://www.google.org/flutrends/about/data/flu/data.txt')
# 
# # Omit NAs to avoid inconsistency.
# mydata<-na.omit(mydata)
# mydata<-rename(mydata, c("New Zealand"="NewZealand", "South Africa"="SouthAfrica","United States"="UnitedStates"))
# 
# # Removing data for year 2015 because of inconsistency.
# trainData <- subset(mydata,year(mydata$Date)<2014)
# testData  <- subset(mydata,year(mydata$Date)==2014)
# 
# 
# 
# monthInsertion<-function(dataframe) {
#   
#   Month<-0
#   
#   for(i in 1:nrow(dataframe)){
#     
#     Month[i]<-month(as.Date.character(dataframe[i,1],format='%Y-%m-%d'))
#     
#   }
#   
#   tempDF<-subset(dataframe[,-1])
#   return(cbind(Month,tempDF))
# }
# 
# trainMonthData <- monthInsertion(trainData)
# 
# testMonthData <- monthInsertion(testData)
# 
# 
# trainArgentina <- aggregate(Argentina ~ Month, trainMonthData, sum)
# trainAustralia <- aggregate(Australia ~ Month, trainMonthData, sum)
# trainAustria <- aggregate(Austria ~ Month, trainMonthData, sum)
# trainBelgium <- aggregate(Belgium ~ Month, trainMonthData, sum)
# trainBolivia <- aggregate(Bolivia ~ Month, trainMonthData, sum)
# trainBrazil <- aggregate(Brazil ~ Month, trainMonthData, sum)
# trainBulgaria <- aggregate(Bulgaria ~ Month, trainMonthData, sum)
# trainCanada <- aggregate(Canada ~ Month, trainMonthData, sum)
# trainChile <- aggregate(Chile ~ Month, trainMonthData, sum)
# trainFrance <- aggregate(France ~ Month, trainMonthData, sum)
# trainHungary <- aggregate(Hungary ~ Month, trainMonthData, sum)
# trainGermany <- aggregate(Germany ~ Month, trainMonthData, sum)
# trainJapan <- aggregate(Japan ~ Month, trainMonthData, sum)
# trainMexico <- aggregate(Mexico ~ Month, trainMonthData, sum)
# trainNetherlands <- aggregate(Netherlands ~ Month, trainMonthData, sum)
# trainNewZealand <- aggregate(NewZealand ~ Month, trainMonthData, sum)
# trainNorway <- aggregate(Norway ~ Month, trainMonthData, sum)
# trainParaguay <- aggregate(Paraguay ~ Month, trainMonthData, sum)
# trainPeru <- aggregate(Peru ~ Month, trainMonthData, sum)
# trainPoland <- aggregate(Poland ~ Month, trainMonthData, sum)
# trainRomania <- aggregate(Romania ~ Month, trainMonthData, sum)
# trainRussia <- aggregate(Russia ~ Month, trainMonthData, sum)
# trainSouthAfrica <- aggregate(SouthAfrica ~ Month, trainMonthData, sum)
# trainSpain <- aggregate(Spain ~ Month, trainMonthData, sum)
# trainSweden <- aggregate(Sweden ~ Month, trainMonthData, sum)
# trainSwitzerland <- aggregate(Switzerland ~ Month, trainMonthData, sum)
# trainUkraine <- aggregate(Ukraine ~ Month, trainMonthData, sum)
# trainUnitedStates <- aggregate(UnitedStates ~ Month, trainMonthData, sum)
# trainUruguay <- aggregate(Uruguay ~ Month, trainMonthData, sum)
# 
# 
# 
# 
# 
# 
# testArgentina <- aggregate(Argentina ~ Month, testMonthData, sum)
# testAustralia <- aggregate(Australia ~ Month, testMonthData, sum)
# testAustria <- aggregate(Austria ~ Month, testMonthData, sum)
# testBelgium <- aggregate(Belgium ~ Month, testMonthData, sum)
# testBolivia <- aggregate(Bolivia ~ Month, testMonthData, sum)
# testBrazil <- aggregate(Brazil ~ Month, testMonthData, sum)
# testBulgaria <- aggregate(Bulgaria ~ Month, testMonthData, sum)
# testCanada <- aggregate(Canada ~ Month, testMonthData, sum)
# testChile <- aggregate(Chile ~ Month, testMonthData, sum)
# testFrance <- aggregate(France ~ Month, testMonthData, sum)
# testHungary <- aggregate(Hungary ~ Month, testMonthData, sum)
# testGermany <- aggregate(Germany ~ Month, testMonthData, sum)
# testJapan <- aggregate(Japan ~ Month, testMonthData, sum)
# testMexico <- aggregate(Mexico ~ Month, testMonthData, sum)
# testNetherlands <- aggregate(Netherlands ~ Month, testMonthData, sum)
# testNewZealand <- aggregate(NewZealand ~ Month, testMonthData, sum)
# testNorway <- aggregate(Norway ~ Month, testMonthData, sum)
# testParaguay <- aggregate(Paraguay ~ Month, testMonthData, sum)
# testPeru <- aggregate(Peru ~ Month, testMonthData, sum)
# testPoland <- aggregate(Poland ~ Month, testMonthData, sum)
# testRomania <- aggregate(Romania ~ Month, testMonthData, sum)
# testRussia <- aggregate(Russia ~ Month, testMonthData, sum)
# testSouthAfrica <- aggregate(SouthAfrica ~ Month, testMonthData, sum)
# testSpain <- aggregate(Spain ~ Month, testMonthData, sum)
# testSweden <- aggregate(Sweden ~ Month, testMonthData, sum)
# testSwitzerland <- aggregate(Switzerland ~ Month, testMonthData, sum)
# testUkraine <- aggregate(Ukraine ~ Month, testMonthData, sum)
# testUnitedStates <- aggregate(UnitedStates ~ Month, testMonthData, sum)
# testUruguay <- aggregate(Uruguay ~ Month, testMonthData, sum)
# 
# Argentina<-cbind(trainArgentina,testArgentina[2],'Argentina')
# colnames(Argentina) <- c("Month","Train","Test","Country")
# Argentina[,2]<-ceiling(Argentina[,2]/8)
# 
# Australia<-cbind(trainAustralia,testAustralia[2],'Australia')
# colnames(Australia) <- c("Month","Train","Test","Country")
# Australia[,2]<-ceiling(Australia[,2]/8)
# 
# Austria<-cbind(trainAustria,testAustria[2],'Austria')
# colnames(Austria) <- c("Month","Train","Test","Country")
# Austria[,2]<-ceiling(Austria[,2]/8)
# 
# Belgium<-cbind(trainBelgium,testBelgium[2],'Belgium')
# colnames(Belgium) <- c("Month","Train","Test","Country")
# Belgium[,2]<-ceiling(Belgium[,2]/8)
# 
# Bolivia<-cbind(trainBolivia,testBolivia[2],'Bolivia')
# colnames(Bolivia) <- c("Month","Train","Test","Country")
# Bolivia[,2]<-ceiling(Bolivia[,2]/8)
# 
# Brazil<-cbind(trainBrazil,testBrazil[2],'Brazil')
# colnames(Brazil) <- c("Month","Train","Test","Country")
# Brazil[,2]<-ceiling(Brazil[,2]/8)
# 
# Bulgaria<-cbind(trainBulgaria,testBulgaria[2],'Bulgaria')
# colnames(Bulgaria) <- c("Month","Train","Test","Country")
# Bulgaria[,2]<-ceiling(Bulgaria[,2]/8)
# 
# Canada<-cbind(trainCanada,testCanada[2],'Canada')
# colnames(Canada) <- c("Month","Train","Test","Country")
# Canada[,2]<-ceiling(Canada[,2]/8)
# 
# Chile<-cbind(trainChile,testChile[2],'Chile')
# colnames(Chile) <- c("Month","Train","Test","Country")
# Chile[,2]<-ceiling(Chile[,2]/8)
# 
# France<-cbind(trainFrance,testFrance[2],'France')
# colnames(France) <- c("Month","Train","Test","Country")
# France[,2]<-ceiling(France[,2]/8)
# 
# Germany<-cbind(trainGermany,testGermany[2],'Germany')
# colnames(Germany) <- c("Month","Train","Test","Country")
# Germany[,2]<-ceiling(Germany[,2]/8)
# 
# Hungary<-cbind(trainHungary,testHungary[2],'Hungary')
# colnames(Hungary) <- c("Month","Train","Test","Country")
# Hungary[,2]<-ceiling(Hungary[,2]/8)
# 
# Japan<-cbind(trainJapan,testJapan[2],'Japan')
# colnames(Japan) <- c("Month","Train","Test","Country")
# Japan[,2]<-ceiling(Japan[,2]/8)
# 
# Mexico<-cbind(trainMexico,testMexico[2],'Mexico')
# colnames(Mexico) <- c("Month","Train","Test","Country")
# Mexico[,2]<-ceiling(Mexico[,2]/8)
# 
# Netherlands<-cbind(trainNetherlands,testNetherlands[2],'Neatherlands')
# colnames(Netherlands) <- c("Month","Train","Test","Country")
# Netherlands[,2]<-ceiling(Netherlands[,2]/8)
# 
# NewZealand<-cbind(trainNewZealand,testNewZealand[2],'NewZealand')
# colnames(NewZealand) <- c("Month","Train","Test","Country")
# NewZealand[,2]<-ceiling(NewZealand[,2]/8)
# 
# Norway<-cbind(trainNorway,testNorway[2],'Norway')
# colnames(Norway) <- c("Month","Train","Test","Country")
# Norway[,2]<-ceiling(Norway[,2]/8)
# 
# Paraguay<-cbind(trainParaguay,testParaguay[2],'Paraguay')
# colnames(Paraguay) <- c("Month","Train","Test","Country")
# Paraguay[,2]<-ceiling(Paraguay[,2]/8)
# 
# Peru<-cbind(trainPeru,testPeru[2],'Peru')
# colnames(Peru) <- c("Month","Train","Test","Country")
# Peru[,2]<-ceiling(Peru[,2]/8)
# 
# Poland<-cbind(trainPoland,testPoland[2],'Poland')
# colnames(Poland) <- c("Month","Train","Test","Country")
# Poland[,2]<-ceiling(Poland[,2]/8)
# 
# Romania<-cbind(trainRomania,testRomania[2],'Romania')
# colnames(Romania) <- c("Month","Train","Test","Country")
# Romania[,2]<-ceiling(Romania[,2]/8)
# 
# Russia<-cbind(trainRussia,testRussia[2],'Russia')
# colnames(Russia) <- c("Month","Train","Test","Country")
# Russia[,2]<-ceiling(Russia[,2]/8)
# 
# SouthAfrica<-cbind(trainSouthAfrica,testSouthAfrica[2],'SouthAfrica')
# colnames(SouthAfrica) <- c("Month","Train","Test","Country")
# SouthAfrica[,2]<-ceiling(SouthAfrica[,2]/8)
# 
# Spain<-cbind(trainSpain,testSpain[2],'Spain')
# colnames(Spain) <- c("Month","Train","Test","Country")
# Spain[,2]<-ceiling(Spain[,2]/8)
# 
# Sweden<-cbind(trainSweden,testSweden[2],'Sweden')
# colnames(Sweden) <- c("Month","Train","Test","Country")
# Sweden[,2]<-ceiling(Sweden[,2]/8)
# 
# Switzerland<-cbind(trainSwitzerland,testSwitzerland[2],'Switzerland')
# colnames(Switzerland) <- c("Month","Train","Test","Country")
# Switzerland[,2]<-ceiling(Switzerland[,2]/8)
# 
# Ukraine<-cbind(trainUkraine,testUkraine[2],'Ukraine')
# colnames(Ukraine) <- c("Month","Train","Test","Country")
# Ukraine[,2]<-ceiling(Ukraine[,2]/8)
# 
# UnitedStates<-cbind(trainUnitedStates,testUnitedStates[2],'UnitedStates')
# colnames(UnitedStates) <- c("Month","Train","Test","Country")
# UnitedStates[,2]<-ceiling(UnitedStates[,2]/8)
# 
# Uruguay<-cbind(trainUruguay,testUruguay[2],'Uruguay')
# colnames(Uruguay) <- c("Month","Train","Test","Country")
# Uruguay[,2]<-ceiling(Uruguay[,2]/8)
# 
# countryNames<-colnames(mydata)[-1]
# continentNames<-c('SouthAmerica','Oceania','Europe','Europe','SouthAmerica','SouthAmerica','Europe',
#                   'NorthAmerica', 'SouthAmerica','Europe','Europe','Europe','Asia','NorthAmerica',
#                   'Europe','Oceania','Europe','SouthAmerica','SouthAmerica','Europe','Europe',
#                   'Europe','Africa','Europe','Europe','Europe','Europe','NorthAmerica',
#                   'SouthAmerica')
# countryContinent<-as.data.frame(cbind(countryNames,continentNames))
# 
# countrydata<-rbind(Argentina,Australia,Austria,Belgium,Bolivia,Brazil,Bulgaria,Canada,Chile,France,Germany,Hungary,Japan,
#                    Mexico, Netherlands, NewZealand, Norway,Paraguay,Peru,Poland,Romania,Russia,SouthAfrica,Spain,Sweden,
#                    Switzerland,Ukraine,UnitedStates,Uruguay)
# 
# 
# 
# 
# # Creating a DATABASE
# 
# db<-dbConnect(SQLite(),dbname="db")
# 
# 
# setOldClass(c("grouped_df", "tbl_df", "data.frame"))
# dbWriteTable(conn = db, name = "countryContinentdb", value = countryContinent, row.names = FALSE)
# dbWriteTable(conn = db, name = "countryDatadb", value = countrydata, row.names = FALSE)
# 
# 
# 
# ggplot(data = dbGetQuery(db, "Select * from countryDatadb where [Country] = 'UnitedStates'")) + 
#   geom_line(mapping = aes(x = Month, y = Test), color = "Red",stat = "identity", position = "identity") + 
#   geom_line(mapping = aes(x = Month, y = Train), color = "Blue", show.legend = TRUE,stat = "identity", position = "identity") +
#   xlim(1,13) + labs(y="Actual=Red, Prediction=Blue", title = "Country")
# 
# ggplot(data = dbGetQuery(db, "Select Month,SUM(Train) AS Train, SUM(Test) AS Test from countryDatadb as country 
#                               INNER JOIN countryContinentdb as continent ON country.Country=continent.countryNames 
#                               where continent.continentNames IN ('Europe','SouthAmerica') GROUP BY country.Month")) + 
#   geom_line(mapping = aes(x = Month, y = Test), color = "Red",stat = "identity", position = "identity") + 
#   geom_line(mapping = aes(x = Month, y = Train), color = "Blue", show.legend = TRUE,stat = "identity", position = "identity") +
#   xlim(1,13) + labs(y="Actual=Red, Prediction=Blue", title = "Continent")
# 
# ggplot(data = dbGetQuery(db, "Select Month,SUM(Train) AS Train, SUM(Test) AS Test from countryDatadb as country GROUP BY country.Month")) + 
#   geom_line(mapping = aes(x = Month, y = Test), color = "Red",stat = "identity", position = "identity") + 
#   geom_line(mapping = aes(x = Month, y = Train), color = "Blue", show.legend = TRUE,stat = "identity", position = "identity") +
#   xlim(1,13) + labs(y="Actual=Red, Prediction=Blue", title = "World")
# 
# 
# groupedData<-countrydata%>%group_by(Month,Country)%>%summarise_each(funs(sum))
# model2<-lm(Test~Train, data=groupedData)
# summary(model2)
# 
# worldData2<-aggregate(groupedData[,3:4], by=list(groupedData$Month), FUN=sum)
# model4<-lm(Test~Train, data=worldData2)
# summary(model4)


# *********************************************************************************************** #

# Representing countries on map

countryNames<-colnames(mydata)[-1]

# install.packages("rworldmap")
library(rworldmap)

country <-countryNames
country[23] <- 'South Africa'
country[28] <- 'United States of America'
country[16] <- 'New Zealand'

malDF <- data.frame(country,
                    flu = c(rep.int(1,29)))

malMap <- joinCountryData2Map(malDF, joinCode = "NAME",
                              nameJoinColumn = "country")

mapCountryData(malMap, nameColumnToPlot="flu", catMethod = "categorical",
               missingCountryCol = gray(.8))


