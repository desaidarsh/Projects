##Loading the required libraries
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(rJava)
library(xlsx)
require(RSelenium)

##Reading the Researchers excel data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
researchersDataFrame <- read.xlsx("Researcher List.xlsx",1)
#researchersDataFrame <- read.xlsx("D:/Jobs/Input/Seventy_Records.xlsx",1)
researchersDataFrame

##Converting factors to strings
researchersDataFrame[] <- lapply(researchersDataFrame, as.character)

authorResultsDataFrame <- data.frame(Author=character(), NumberOfPublicationsRetrieved = numeric(), stringsAsFactors = FALSE)

##Defining the lower and upper bounds for the researchers loop
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lowerBound <- 516
upperBound <- 517

##Defining the output data frame
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outputDataFrame <- data.frame(Researcher_Name = character(), Researcher_Department = character(), Publication_Name = character(), Publication_Times_Cited = character(), Publication_Authors_List = character(), Publication_Source = character(), Misc1 = character(), Misc2 = character(), Misc3 = character())

newOutputDataFrame <- data.frame(Researcher_Name = character(), Researcher_Department = character(), appendOutputFullString = character())

##Looping through the search process for the given researchers
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (i in lowerBound:upperBound)
{
  
  ##Creating a remote driver, opening internet explorer and login to neuidmsso (single sign on)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  remDr <- remoteDriver(remoteServerAddr = "localhost" 
                        , port = 4444
                        , browserName = "internet explorer"
  )
  remDr$open()
  remDr$navigate("https://neuidmsso.neu.edu/cas-server/login")
  remDr$findElement("id", "username")$sendKeysToElement(list("username"))
  remDr$findElement("id", "password")$sendKeysToElement(list("password"))
  remDr$findElement("name", "submit")$clickElement()
  
  
  ##Defining a function to wait for a few seconds to complete page load
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  waitForAWhile <- function(x)
  {
    p1 <- proc.time()
    Sys.sleep(x)
    proc.time() - p1 # The cpu usage should be negligible
  }
  waitForAWhile(8)
  
  
  ##Navigating to Web of Services opening with Default Search by Author
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  remDr$navigate("http://apps.webofknowledge.com.ezproxy.neu.edu/WOS_GeneralSearch_input.do?product=WOS&search_mode=GeneralSearch&SID=2BUJMFb2pVvTq2YZiFD&preferencesSaved=")
  waitForAWhile(20)
  
  currentAuthorName <- researchersDataFrame[i,1]
  currentAuthorDepartment <- researchersDataFrame[i,2]
  currentAuthorSearchString <- researchersDataFrame[i,5]
  
  AdvancedSearchTabElement <- remDr$findElement("css selector", "body > div.block-search.block-search-header > div > ul > li:nth-child(3) > a")
  AdvancedSearchTabElement$clickElement()
  
  waitForAWhile(15)
  
  remDr$findElement("xpath", "//*[@id='value(input1)']")$sendKeysToElement(list(currentAuthorSearchString))
  
  waitForAWhile(2)
  
  AdvancedSearchButtonElement <- remDr$findElement("css selector", "#searchButton > input[type='image']")
  AdvancedSearchButtonElement$clickElement()
  
  waitForAWhile(15)
  
  
  if (regexpr('searchErrorMessage',remDr$getCurrentUrl()) > 0)
  {
    
    remDr$closeWindow()
    waitForAWhile(2)
    
    numberOfResultsRetrieved <- 0
    authorResultsDataFrame <- rbind(authorResultsDataFrame, data.frame(Author=currentAuthorName, NumberOfPublicationsRetrieved = numberOfResultsRetrieved))
    
  }
  else
    
    {
      NumberOfSearchResultsLink <- remDr$findElement("css selector", "#set_1_div > a")
      NumberOfSearchResultsLink$clickElement()
      
      waitForAWhile(16)
      
      #Getting the number of results
      numberOfHitsWebElement <- remDr$findElement(using = "xpath", "//*[@id='hitCount.top']")
      numberOfResultsRetrieved <- as.numeric(gsub(",", "", numberOfHitsWebElement$getElementText()))
      numberOfResultsRetrieved
      
      
      if(numberOfResultsRetrieved < 50)
      {
        
        #Clicking on the #printicon 
        printButtonElement <- remDr$findElement(using = "name", "formatForPrint")
        printButtonElement$clickElement()
        
        waitForAWhile(15)
        
        
        #Select the range records radio button
        RecordsRangeRadioButton <- remDr$findElement(using = "id", "numberOfRecordsRange")
        RecordsRangeRadioButton$clickElement()
        
        waitForAWhile(1)
        
        #Enter the range of the records
        remDr$findElement("id", "markFrom")$sendKeysToElement(list("1"))
        remDr$findElement("id", "markTo")$sendKeysToElement(list(as.character(numberOfResultsRetrieved)))
        
        #Selecting the required option from the dropdown
        webElem <- remDr$findElement(using = 'xpath', "//select[@id='bib_fields']")
        opts <- webElem$selectTag()
        opts$elements[[4]]$clickElement()
        
        waitForAWhile(1)
        
        printImageButton <- remDr$findElement(using='xpath', '//*[@id="ui-id-7"]/form/div[3]/span/input')
        printImageButton$clickElement()
        
        waitForAWhile(50)
        
        #Switching to the #printresults page
        currWin <- remDr$getCurrentWindowHandle()
        allWins <- unlist(remDr$getWindowHandles())
        otherWindow <- allWins[!allWins %in% currWin[[1]]]
        remDr$switchToWindow(otherWindow)
        remDr$getTitle()
        
        authorResultsDataFrame <- rbind(authorResultsDataFrame, data.frame(Author=currentAuthorName, NumberOfPublicationsRetrieved = numberOfResultsRetrieved))
        
        
        for(k in 1:as.numeric(numberOfResultsRetrieved)+1){
          
          xpathForEachRecordValue <- paste0("//*[@id='printForm']/table[", as.character(k), "]/tbody/tr/td")
          xpathForEachRecordValue
          
          Misc3Elements <- remDr$findElements(using = "xpath",  xpathForEachRecordValue)
          Misc3ElementsElements_CSS_Text_Headers <- unlist(lapply(Misc3Elements, function(x){x$getElementText()}))
          Misc3DataFrame <- as.data.frame(Misc3ElementsElements_CSS_Text_Headers)
          
          Misc3DataFrame[] <- lapply(Misc3DataFrame, as.character)
          
          outputFullString <- ""
          for (j in 2:nrow(Misc3DataFrame))
          {
            
            if (regexpr('Published: ',Misc3DataFrame[j,1]) > 0)
            {
              Misc3DataFrame[j,1] <- gsub('  ', '::::::',Misc3DataFrame[j,1])
              Misc3DataFrame[j,1] <- substr(Misc3DataFrame[j,1], 1, nchar(Misc3DataFrame[j,1]) - 6)
            }
            
            outputFullString <- paste0(outputFullString, "::::::", Misc3DataFrame[j,1])
            
            print(Misc3DataFrame[j,1])
            
          }
          
          outputFullString <- substr(outputFullString, 1, nchar(outputFullString) - 6)
          newOutputDataFrame <- rbind(newOutputDataFrame, data.frame(Researcher_Name = currentAuthorName, Researcher_Department = currentAuthorDepartment, appendOutputFullString = outputFullString))
        }
        
        
        
        
        
      }
      
      else
      {
        
        numberOfTimesToClickNextButton <- numberOfResultsRetrieved%/%50
        numberOfRecordsInTheLastPage <- numberOfResultsRetrieved%%50
        
        
        #Clicking on the #printicon 
        printButtonElement <- remDr$findElement(using = "name", "formatForPrint")
        printButtonElement$clickElement()
        
        waitForAWhile(15)
        
        
        #Select the range records radio button
        RecordsRangeRadioButton <- remDr$findElement(using = "id", "numberOfRecordsRange")
        RecordsRangeRadioButton$clickElement()
        
        waitForAWhile(1)
        
        #Enter the range of the records
        remDr$findElement("id", "markFrom")$sendKeysToElement(list("1"))
        remDr$findElement("id", "markTo")$sendKeysToElement(list(as.character(numberOfResultsRetrieved)))
        
        #Selecting the required option from the dropdown
        webElem <- remDr$findElement(using = 'xpath', "//select[@id='bib_fields']")
        opts <- webElem$selectTag()
        opts$elements[[4]]$clickElement()
        
        waitForAWhile(1)
        
        printImageButton <- remDr$findElement(using='xpath', '//*[@id="ui-id-7"]/form/div[3]/span/input')
        printImageButton$clickElement()
        
        waitForAWhile(50)
        
        #Switching to the #printresults page
        currWin <- remDr$getCurrentWindowHandle()
        allWins <- unlist(remDr$getWindowHandles())
        otherWindow <- allWins[!allWins %in% currWin[[1]]]
        remDr$switchToWindow(otherWindow)
        remDr$getTitle()
        
        authorResultsDataFrame <- rbind(authorResultsDataFrame, data.frame(Author=currentAuthorName, NumberOfPublicationsRetrieved = numberOfResultsRetrieved))
        
        
        for(l in 1:numberOfTimesToClickNextButton)
        {
          
          for(k in 2:51){
            
            xpathForEachRecordValue <- paste0("//*[@id='printForm']/table[", as.character(k), "]/tbody/tr/td")
            xpathForEachRecordValue
            
            Misc3Elements <- remDr$findElements(using = "xpath",  xpathForEachRecordValue)
            Misc3ElementsElements_CSS_Text_Headers <- unlist(lapply(Misc3Elements, function(x){x$getElementText()}))
            Misc3DataFrame <- as.data.frame(Misc3ElementsElements_CSS_Text_Headers)
            
            Misc3DataFrame[] <- lapply(Misc3DataFrame, as.character)
            
            outputFullString <- ""
            for (j in 2:nrow(Misc3DataFrame))
            {
              
              if (regexpr('Published: ',Misc3DataFrame[j,1]) > 0)
              {
                Misc3DataFrame[j,1] <- gsub('  ', '::::::',Misc3DataFrame[j,1])
                Misc3DataFrame[j,1] <- substr(Misc3DataFrame[j,1], 1, nchar(Misc3DataFrame[j,1]) - 6)
              }
              
              outputFullString <- paste0(outputFullString, "::::::", Misc3DataFrame[j,1])
              
              print(Misc3DataFrame[j,1])
              
            }
            
            outputFullString <- substr(outputFullString, 1, nchar(outputFullString) - 6)
            newOutputDataFrame <- rbind(newOutputDataFrame, data.frame(Researcher_Name = currentAuthorName, Researcher_Department = currentAuthorDepartment, appendOutputFullString = outputFullString))
            
            
          }
              if(as.numeric(numberOfRecordsInTheLastPage) > 0 ) 
              {
                  nextPageLinkInPrint<- remDr$findElement(using = 'css selector', '#printForm > table:nth-child(25) > tbody > tr > td:nth-child(2) > table > tbody > tr > td > input[type="image"]')
                  nextPageLinkInPrint$clickElement()
                  waitForAWhile(40)
              }
          
        }
        
        
        if(as.numeric(numberOfRecordsInTheLastPage) > 0 ) 
        {
        
        for(k in 2:as.numeric(numberOfRecordsInTheLastPage)+1){
          
          xpathForEachRecordValue <- paste0("//*[@id='printForm']/table[", as.character(k), "]/tbody/tr/td")
          xpathForEachRecordValue
          
          Misc3Elements <- remDr$findElements(using = "xpath",  xpathForEachRecordValue)
          Misc3ElementsElements_CSS_Text_Headers <- unlist(lapply(Misc3Elements, function(x){x$getElementText()}))
          Misc3DataFrame <- as.data.frame(Misc3ElementsElements_CSS_Text_Headers)
          
          Misc3DataFrame[] <- lapply(Misc3DataFrame, as.character)
          
          outputFullString <- ""
          for (j in 2:nrow(Misc3DataFrame))
          {
            
            if (regexpr('Published: ',Misc3DataFrame[j,1]) > 0)
            {
              Misc3DataFrame[j,1] <- gsub('  ', '::::::',Misc3DataFrame[j,1])
              Misc3DataFrame[j,1] <- substr(Misc3DataFrame[j,1], 1, nchar(Misc3DataFrame[j,1]) - 6)
            }
            
            outputFullString <- paste0(outputFullString, "::::::", Misc3DataFrame[j,1])
            
            print(Misc3DataFrame[j,1])
            
          }
          
          outputFullString <- substr(outputFullString, 1, nchar(outputFullString) - 6)
          newOutputDataFrame <- rbind(newOutputDataFrame, data.frame(Researcher_Name = currentAuthorName, Researcher_Department = currentAuthorDepartment, appendOutputFullString = outputFullString))
        }
        
        }
        
      }
      
      
      waitForAWhile(10)
      
      #Switching to the #printresults page
      currWin <- remDr$getCurrentWindowHandle()
      allWins <- unlist(remDr$getWindowHandles())
      otherWindow <- allWins[!allWins %in% currWin[[1]]]
      remDr$switchToWindow(otherWindow)
      remDr$getTitle()
      waitForAWhile(2)
      #remDr$quit()

      waitForAWhile(10)
      #waitForAWhile(10)
      #remDr$close
      waitForAWhile(2)
      #remDr$closeWindow()
      #waitForAWhile(2)
      #remDr$closeServer
      #waitForAWhile(5)
      #remDr$closeall()
      #remDr$errorDetails
      #remDr$closeWindow()
      rm(remDr)
      

      
    }
  
  
  
  
  
}

write.xlsx(authorResultsDataFrame, 'D:/Jobs/Advanced_Search_Output/authorSearches516.xlsx')
write.xlsx(newOutputDataFrame, 'D:/Jobs//Advanced_Search_Output/theBigData1to516.xlsx')



remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "internet explorer"
)
remDr$open()
remDr$navigate("https://www.google.com")
remDr$screenshot(display = TRUE)
remDr$navigate("https://neuidmsso.neu.edu/cas-server/login")
remDr$screenshot(display = TRUE)

#Switching to the #printresults page
currWin <- remDr$getCurrentWindowHandle()
allWins <- unlist(remDr$getWindowHandles())
otherWindow <- allWins[!allWins %in% currWin[[1]]]
remDr$switchToWindow(otherWindow)
remDr$getTitle()



remDr$closeServer
remDr$closeall()
rm(remDr)


quotient <- 63%/%50
quotient

remainder <- 63%%50
remainder

currentAuthorSearchString

researchersDataFrame[1,5]

i
j

Misc3DataFrame[j,1]


##Creating a remote driver, opening internet explorer and login to neuidmsso (single sign on)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "internet explorer"
)
remDr$open()
remDr$navigate("https://neuidmsso.neu.edu/cas-server/login")
remDr$setImplicitWaitTimeout(milliseconds = 100000)
remDr$findElement("id", "username")$sendKeysToElement(list("username"))
remDr$findElement("id", "password")$sendKeysToElement(list("password"))
remDr$findElement("name", "submit")$clickElement()
remDr$screenshot(display = TRUE)
remDr$navigate("https://www.google.com")
remDr$screenshot(display = TRUE)
remDr$navigate("http://apps.webofknowledge.com.ezproxy.neu.edu/")

remDr$navigate("http://apps.webofknowledge.com.ezproxy.neu.edu/WOS_GeneralSearch_input.do?product=WOS&search_mode=GeneralSearch&SID=2BUJMFb2pVvTq2YZiFD&preferencesSaved=")
remDr$screenshot(display = TRUE)

AdvancedSearchTabElement <- remDr$findElement("css selector", "body > div.block-search.block-search-header > div > ul > li:nth-child(3) > a")
AdvancedSearchTabElement$clickElement()
remDr$screenshot(display = TRUE)
remDr$quit()

k
i

currentAuthorName 
currentAuthorDepartment
currentAuthorSearchString 

currentAuthorName <- researchersDataFrame[516,1]
currentAuthorDepartment <- researchersDataFrame[i,2]
currentAuthorSearchString <- researchersDataFrame[i,5]