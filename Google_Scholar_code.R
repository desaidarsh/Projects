library(tidyverse)
library(stringr)
library(xlsx)
library(readxl)

Re <- read_excel("Re.xlsx")
data<-Re[,1:2]

colnames(data)<-c("Dept","Name")

data$Name<-paste0("'", data$Name, "'")

# Extracting data from input file

low<-1
high<-843

a<-NULL
query<-NULL
for(i in c(low:high)) {
  
  a<-paste0(data$Name[i], " OR ")
  
  query<-paste0(query,a)
  
}
write(query, file = "query.txt")


#------------------------------------------------------------------------------------#
# reading and compiling all files extracted from google Scholar
a<-NULL
b<-NULL
c<-as.data.frame(NULL)
string<-NULL
i<-1

for(i in c(1:46)) {
  
  string<-paste0("",i,".csv")
  a<-read.csv(string)
  b<-as.data.frame(a)
  c<-rbind(c,b)
  
}

write.xlsx(c,"researcherData.xlsx")
write.csv(c,"researcherData.csv")

d<-read_xlsx("researcherdatabase.xlsx")

write.csv(d,"researcherdatabase.csv")


#-----------------------------------------------------------------------------------------#


Re <- read_excel("Re.xlsx")
researcherData<-read.csv("researcherData.csv")

# removing '.' from Authors column
researcherData$Authors<-gsub('\\.', '', researcherData$Authors)
researcherData$Authors<-gsub("Ã¢â¬Â¦", "", researcherData$Authors)

# splitting authors

researcherData<-separate(researcherData,Authors, c("Author1","Author2","Author3","Author4","Author5","Author6","Author7","Author8"),sep=",")

# extracting last names of all researchers from Northeastern Unievrsity
Re<-Re[,2:3]

Re<-separate(Re,name_outlook,c("name","fname"), sep = ",")

i<-1
for(i in 1:843) {
  if (is.na(Re$fname[i])) {
    Re$lname[i]<- word(Re$name[i],2)
  }
  if(!is.na(Re$fname[i])){
    Re$lname[i]<-word(Re$name[i],1)
  }
}
lname<-as.list(Re[,3])

# lnames of authors

researcherData$Author1<- word(researcherData$Author1,-1)
researcherData$Author2<- word(researcherData$Author2,-1)
researcherData$Author3<- word(researcherData$Author3,-1)
researcherData$Author4<- word(researcherData$Author4,-1)
researcherData$Author5<- word(researcherData$Author5,-1)
researcherData$Author6<- word(researcherData$Author6,-1)
researcherData$Author7<- word(researcherData$Author7,-1)
researcherData$Author8<- word(researcherData$Author8,-1)

# write.csv(researcherData,"Re2.csv")
Northeastern<-as.data.frame(NULL)
for(l in 1:8) {
  for(j in 1:39049) {
    if(!is.na(researcherData[j,l+2])) {
      for(k in 1:843) {
        if (researcherData[j,l+2] %in% Re[k,3])  {
          Northeastern[j,l]<-researcherData[j,l+2]
        }
      }
    }
  }
}

# write.csv(Northeastern,"Northeastern.csv")

Re2 <- read_csv("~/Documents/R/Google Scholar/New folder/Re2.csv")

colnames(Northeastern)<-c("Number","NUresearcher1","NUresearcher2","NUresearcher3","NUresearcher4","NUresearcher5","NUresearcher6","NUresearcher7")

database<-cbind(Re2,Northeastern,researcherData[,3])
database<-database[,2:37]
colnames(database)[1:2]<-c("Number","Cites")
database<-database[,c(1,2,29:36,3:27)]
database<-database[,c(1:10,19:35)]
database2<-database[,]

database2 <- database2[!(is.na(database2$NUresearcher1)) | !(is.na(database2$NUresearcher2)) |!(is.na(database2$NUresearcher3)) |!(is.na(database2$NUresearcher4)) |!(is.na(database2$NUresearcher5)) |!(is.na(database2$NUresearcher6)) |!(is.na(database2$NUresearcher7)),]
database2$Number<-seq(from = 1, to = 28496)
row.names(database2) <- c(1:28496)

#Data wrangling

for(i in 1:28496) {
  if(is.na(database2[i,3])) {
    database2[i,3]<-database2[i,4]
  }
}

for(i in 1:28496) {
  if(is.na(database2[i,3])) {
    database2[i,3]<-database2[i,5]
  }
}

for(i in 1:28496) {
  if(is.na(database2[i,3])) {
    database2[i,3]<-database2[i,6]
  }
}

for(i in 1:28496) {
  if(is.na(database2[i,3])) {
    database2[i,3]<-database2[i,7]
  }
}
for(i in 1:28496) {
  if(is.na(database2[i,3])) {
    database2[i,3]<-database2[i,8]
  }
}
for(i in 1:28496) {
  if(is.na(database2[i,3])) {
    database2[i,3]<-database2[i,9]
  }
}


for(i in 1:28496) {
  for (j in 4:9) {
    if(!is.na(database2[i,j])) {
      if(database2[i,3] == database2[i,j]) {
        database2[i,j] <- NA
      }}
    
  }
}


for(i in 1:28496) {
  if(is.na(database2[i,4])) {
    database2[i,4]<-database2[i,5]
  }
}

for(i in 1:28496) {
  if(is.na(database2[i,4])) {
    database2[i,4]<-database2[i,6]
  }
}

for(i in 1:28496) {
  if(is.na(database2[i,4])) {
    database2[i,4]<-database2[i,7]
  }
}
for(i in 1:28496) {
  if(is.na(database2[i,4])) {
    database2[i,4]<-database2[i,8]
  }
}
for(i in 1:28496) {
  if(is.na(database2[i,4])) {
    database2[i,4]<-database2[i,9]
  }
}

for(i in 1:28496) {
  for (j in 5:9) {
    if(!is.na(database2[i,j])) {
      if(database2[i,4] == database2[i,j]) {
        database2[i,j] <- NA
      }}
    
  }
}

for(i in 1:28496) {
  if(is.na(database2[i,5])) {
    database2[i,5]<-database2[i,6]
  }
}

for(i in 1:28496) {
  if(is.na(database2[i,5])) {
    database2[i,5]<-database2[i,7]
  }
}
for(i in 1:28496) {
  if(is.na(database2[i,5])) {
    database2[i,5]<-database2[i,8]
  }
}
for(i in 1:28496) {
  if(is.na(database2[i,5])) {
    database2[i,5]<-database2[i,9]
  }
}


for(i in 1:28496) {
  for (j in 6:9) {
    if(!is.na(database2[i,j])) {
      if(database2[i,5] == database2[i,j]) {
        database2[i,j] <- NA
      }}
    
  }
}


for(i in 1:28496) {
  if(is.na(database2[i,6])) {
    database2[i,6]<-database2[i,7]
  }
}
for(i in 1:28496) {
  if(is.na(database2[i,6])) {
    database2[i,6]<-database2[i,8]
  }
}
for(i in 1:28496) {
  if(is.na(database2[i,6])) {
    database2[i,6]<-database2[i,9]
  }
}


for(i in 1:28496) {
  for (j in 8:9) {
    if(!is.na(database2[i,j])) {
      if(database2[i,7] == database2[i,j]) {
        database2[i,j] <- NA
      }}
    
  }
}


for(i in 1:28496) {
  if(is.na(database2[i,7])) {
    database2[i,7]<-database2[i,8]
  }
}
for(i in 1:28496) {
  if(is.na(database2[i,7])) {
    database2[i,7]<-database2[i,9]
  }
}


for(i in 1:28496) {
  for (j in 7:9) {
    if(!is.na(database2[i,j])) {
      if(database2[i,6] == database2[i,j]) {
        database2[i,j] <- NA
      }}
    
  }
}



for(i in 1:28496) {
  if(is.na(database2[i,8])) {
    database2[i,8]<-database2[i,9]
  }
}


for(i in 1:28496) {
  for (j in 9) {
    if(!is.na(database2[i,j])) {
      if(database2[i,8] == database2[i,j]) {
        database2[i,j] <- NA
      }}
    
  }
}

database2<-database2[,-9]

write_excel_csv(publicationsdatabase,"publicationsdatabase.csv")


#---------------------------------------------------------------------------------------------#
# Attaching Google Schoar IDs

Re<-Re[,2:3]
Re<-Re[!is.na(Re$`Google Scholar ID`),]

separate(Re[,2],name_outlook,c("name","fname"), sep = ",")

i<-1
for(i in 1:843) {
  if (is.na(Re$fname[i])) {
    Re$lname[i]<- word(Re$name[i],2)
  }
  if(!is.na(Re$fname[i])){
    Re$lname[i]<-word(Re$name[i],1)
  }
}

Re<-Re[,3:4]

Re<-Re[!is.na(Re$`Google Scholar ID`),]

a<-Re[which(duplicated(Re$lname)),]
Google_Scholar_final[,27:32] <- NA

for(k in 3:8) {
  for(i in 1:28496) {
    if(!is.na(Google_Scholar_final[i,k])) {
      for(j in 1:462) {
        if(Google_Scholar_final[i,k] == Re[j,2]) {
          Google_Scholar_final[i,k+24] <- Re[j,1]
        }}
    }
  }
}

colnames(Google_Scholar_final)[27:32]<-c("Google_ID_NU1","Google_ID_NU2","Google_ID_NU3","Google_ID_NU4","Google_ID_NU5","Google_ID_NU6")
write_excel_csv(Google_Scholar_final,"Google_Scholar.csv")
