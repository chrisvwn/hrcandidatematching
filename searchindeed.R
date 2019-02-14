if(!require(RSelenium))
{
  install.packages("RSelenium")
}

if(!require(readr))
{
  install.packages("readr")
}

if(!require(dplyr))
{
  install.packages("dplyr")
}

if(!require(RMariaDB))
{
  install.packages("RMariDB")
}

# if(!require(futile.logger))
# {
#   install.packages("futile.logger")
# }

source("utils.R")
source("credentials.R")
source("rankjobs.R")

logcon <- NULL
#dbCon <- NULL
serviceMode <- TRUE

startup <- function(userId)
{
  message(Sys.time(), ": sending output to logfile")
  logfile <- paste("searchindeed", userId, ".log",sep="_")
  logcon <<- file(logfile)
  sink(logcon, append = TRUE)
  sink(logcon, append = TRUE, type = "message")
  message(Sys.time(), ": start logging to logfile")
}

cleanup <- function(userId, siteId, remDr)
{
  message(Sys.time(), ": Clean up and exit")
  
  clientId <- getClientsInSiteProcessing(userId, siteId)$Id
  
  if(!is.null(clientId))
  {
    message(Sys.time(), ": Ranking client results")
    rankClientJobs(clientId)
    
    res <- try(putUserNotification(userId, "warning", paste0("Search for client ", clientId, ": ", getClientName(clientId), " terminated. New Results may be available")), TRUE)
    
    message(Sys.time(), ": Removing processing client: ", clientId)
    
    removeClientFromProcessing(userId, siteId, clientId)
  }
  
  #RMariaDB::dbDisconnect(dbCon)
  
  message(Sys.time(), ": Closing browser window")
  if(!is.null(remDr))
  {
    res <- try(remDr$client$close(),TRUE)
    remDr <- NULL
  }
  
  message(Sys.time(), ": Sending output back to default")
  
  #reset sinks
  sink()
  sink(type = 'message')
  close(logcon)
  message(Sys.time(), ": Stopped logging to file")
}

maxRepeatCount <- 30

searchIndeed <- function(userId)
{
  siteId <- 2
  clientId <- NULL
  remDr <- NULL
  exitGraceful <- TRUE
  
  startup(userId)  
  
  repeat{
    exitCond <- tryCatch(
      {
        #check for exit command if restarting
        #probably due to an error
        cmdQueue <- popCmdQueue(userId, siteId)$CommandTxt
        
        if(length(grep("exit", cmdQueue)) > 0)
        {
          message(Sys.time(), ": Received exit command")
          cleanup(userId, siteId, remDr)
          return()
        }
        
        message(Sys.time(), ": =========== START searchINDEED UserId: ", userId, " ===========")

        maxRepeatCount <- 30
        
        if(!exists("remDr"))
        {
          remDr <- list()
          remDr$client <- NULL 
        }
        
        if(!exists("pJS"))
          pJS <- NULL
        
        options(stringsAsFactors = FALSE)
        
        message(Sys.time(), ": Launching browser")

	      eCaps <- list()#list(chromeOptions = list(  args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
 
	      if(is.null(remDr$client))
	      {
	        repeat
	        {
	          message(Sys.time(), ": remote driver not started. Starting ...")
	          remDr$client <- try(remoteDriver(remoteServerAddr='127.0.0.1', port=4444L, browserName="chrome", extraCapabilities = eCaps), TRUE)
	          
	          message(Sys.time(), ":Checking if we are connected to Selenium Hub")
	          res <- try(remDr$client$open(silent = F))

	          if(class(res) == 'try-error')
	          {
	            message(Sys.time(), ": An error occurred. Cannot connect to Selenium Hub. Trying again in 10")
	            Sys.sleep(10)
	            next
	          }
	          
	          message(Sys.time(), ": Connected to Selenium Hub")
	          break
	        }

          message(Sys.time(), ": Navigating to main page")
          remDr$client$navigate("https://www.indeed.com.br/")
          
          message(Sys.time(), ": Sleeping 3")
          Sys.sleep(3)
	      }
	      
        cancelClientSearch <- FALSE
        
        #use main search bar only the first time
        firstSearch <- TRUE
        
        #for(userIdx in 1:length(users))
        #repeat forever
        repeat
        {
          if(cancelClientSearch)
          {
            if(exists("clientId") && clientId != "")
            {
              message(Sys.time(), ": Removing cancelled client from queue")
              removeClientFromUserQueue(userId, clientId)
              
              #updating client ranking for clientId
              rankClientJobs(clientId)
              
              res <- try(putUserNotification(userId, "warning", paste0("Search for client ", clientId, ": ",  getClientName(clientId), " terminated. New Results may be available")),TRUE)
            }
            cancelClientSearch <- FALSE
          }
          
          #check if we have an exit command
          #after processing a client
          cmdQueue <- popCmdQueue(userId, siteId)$CommandTxt
          
          if(length(grep("exit", cmdQueue)) > 0)
          {
            message(Sys.time(), ": Received exit command")
            cleanup(userId, siteId, remDr)
            return()
          }
          
          clientQueue <- getClientsInSiteQueue(userId, siteId)$ClientId
          
          if(length(clientQueue) == 0)
          {
            #check for exit command even when there
            #are no clients in the queue
            cmdQueue <- popCmdQueue(userId, siteId)$CommandTxt
            
            if(length(grep("exit", cmdQueue)) > 0)
            {
              message(Sys.time(), ": Received exit command")
              cleanup(userId, siteId, remDr)
              return()
            }
            
            if(exitGraceful)
            {
              message(Sys.time(), ": No clients in queue. Exiting gracefully")
              cleanup(userId, siteId, remDr)
              return()
            }
            
            message(Sys.time(), ": No clients to process. Sleeping")
            Sys.sleep(10)
            next
          }
          
          clientId <- clientQueue[1]
          clientIdx <- 1
          
          putClientInProcessing(userId=userId, siteId=siteId, clientId=clientId)
          
          message(Sys.time(), ": Processing client: ", clientId)
          
          searchTxts <- getClientInterests(clientId)
          
          for(searchTxt in searchTxts)
          {
            if(isClientProcessingCancelled(userId, clientId))
            {
              message(Sys.time(), ": Received cancel for client: ", clientId)
              removeClientFromProcessing(userId, siteId, clientId)
              cancelClientSearch <- TRUE
              break
            }
            
            if(cancelClientSearch)
              break
            
            message(Sys.time(), ": Search criteria: ", which(searchTxts == searchTxt), ": '", searchTxt, "'")
            
            repeatCount <- 0
            repeat
            {
              if(repeatCount > maxRepeatCount)
                break
              
              repeatCount <- repeatCount + 1
              
              searchBar <- suppressMessages(try(remDr$client$findElement('id', "text-input-what"), TRUE))
              
              if(class(searchBar) == "try-error")
                searchBar <- suppressMessages(try(remDr$client$findElement('id', "what"), TRUE))
              
              if(class(searchBar) == "try-error")
              {
                message(Sys.time(), ": Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            message(Sys.time(), ": Clicking on search bar")
            #click on the search bar
            searchBar$clickElement()
            
            message(Sys.time(), ": Clearing search bar")
            #clear the search bar
            searchBar$clearElement()
            
            message(Sys.time(), ": Entering search criteria '", searchTxt, "'")
            remDr$client$sendKeysToActiveElement(list(paste0(searchTxt)))
            
            #Entering location
            message(Sys.time(), ": Entering search location")
            
            repeatCount <- 0
            
            repeat
            {
              if(repeatCount > maxRepeatCount)
                break
              
              repeatCount <- repeatCount + 1
              
              searchWhere <- suppressMessages(try(remDr$client$findElement('id', "text-input-where"), TRUE))
              
              if(class(searchWhere) == "try-error")
                searchWhere <- suppressMessages(try(remDr$client$findElement('id', "where"), TRUE))
              
              if(class(searchWhere) == "try-error")
              {
                message(Sys.time(), ": Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            message(Sys.time(), ": Clicking on search location")
            #click on the search bar
            searchWhere$clickElement()
            
            message(Sys.time(), ": Clearing search location")
            #clear the search bar
            searchWhere$clearElement()
            
            message(Sys.time(), ": Entering search location")
            remDr$client$sendKeysToActiveElement(list("Sao Paulo", key="enter"))
            
            message(Sys.time(), ": Sleeping 3")
            Sys.sleep(3)
            
            jobAds <- data.frame()
            allJobs <- 0
            newJobs <- 0
            
            pgNum <- 0
            
            #repeat until there isn't a next page
            repeat
            {
              if(isClientProcessingCancelled(userId, clientId))
              {
                message(Sys.time(), ": Received cancel for client: ", clientId)
                removeClientFromProcessing(userId, siteId, clientId)
                cancelClientSearch <- TRUE
                break
              }
              
              if(cancelClientSearch)
                break
              
              message(Sys.time(), ": Sleeping 3")
              Sys.sleep(3)
              pgNum <- pgNum + 1
              
              message(Sys.time(), ": On Page ", pgNum)
              
              #get all job titles and companies on the page
              #since later after clicking on the job
              #the page structure changes
              repeatCount <- 0
              
              repeat
              {
                if(repeatCount > maxRepeatCount)
                  break
                
                repeatCount <- repeatCount + 1
                
                message(Sys.time(), ": Getting job nodes")
                jobNodes <- suppressMessages(try(remDr$client$findElements('class',"clickcard"),TRUE))
                
                if(class(jobNodes) == "try-error")
                {
                  message(Sys.time(), ": Not yet. Sleeping 1")
                  Sys.sleep(1)
                  next
                }
                break
              }
              
              if(length(jobNodes) > 0)
              {
              message(Sys.time(), ": Getting job titles")
              jobTitles <- unlist(sapply(jobNodes, 
                                         function(x)
                                         {
                                           a <- x$findElement("class", "jobtitle")
                                           
                                           b <- a$findChildElement("class", "turnstileLink")
                                           
                                           b$getElementAttribute("title")
                                         }
              )
              )
              
              message(Sys.time(), ": Getting job urls")
              jobUrls <- unlist(sapply(jobNodes, 
                                       function(x)
                                       {
                                         a <- x$findElement("class", "jobtitle")
                                         
                                         b <- a$findChildElement("class", "turnstileLink")
                                         
                                         b$getElementAttribute("href")
                                       }
              )
              )
              
              remDr$client$setImplicitWaitTimeout(1000)
              
              message(Sys.time(), ": Getting company names")
              jobCompanies <- unlist(sapply(jobNodes, 
                                            function(x)
                                            {
                                              a <- suppressMessages(try(x$findChildElement("class", "company"), TRUE))
                                              
                                              if(class(a) == 'try-error')
                                                company <- NA
                                              else
                                                company <- a$getElementText()
                                              
                                              company
                                            }
              )
              )
              
              remDr$client$setImplicitWaitTimeout(1000)
              
              message(Sys.time(), ": Getting job summaries")
              jobSummaries <- unlist(sapply(jobNodes, 
                                            function(x)
                                            {
                                              a <- suppressMessages(try(x$findChildElement("class", "summary"), TRUE))
                                              
                                              if(class(a) == 'try-error')
                                                company <- NA
                                              else
                                                company <- a$getElementText()
                                              
                                              company
                                            }
              )
              )
              
              remDr$client$setImplicitWaitTimeout(20000)
              
              message(Sys.time(), ": Combining company details")
              #cbind jobnames with details
              jobAds <- dplyr::bind_cols("JobTitle"=jobTitles, "JobUrl"=jobUrls, "CompanyName"=jobCompanies, "DescriptionShort"=jobSummaries)
              
              message(Sys.time(), ": Adding timestamp, clientId, search txt")
              #add timestamp, clientId and searchtext
              jobAds <- dplyr::bind_cols("SearchTime"=rep(Sys.time(), nrow(jobAds)),
                                         "ClientId"=rep(clientId, nrow(jobAds)),
                                         "SiteId"=rep(siteId, nrow(jobAds)),
                                         "SearchTxt"=rep(searchTxt, nrow(jobAds)),
                                         jobAds)
                
                if(nrow(jobAds) > 0)
                {
                  matchCols <- c("jobUrl")
                  
                  #add a rank column which will be populated later
                  jobAds <- cbind.data.frame(jobAds, "Rank"=NA)
                  
                  #write to search results csv 
                  message(Sys.time(), ": Writing results to db")
                  res <- putClientJobSearchResults(jobAds)
                }
              }else
              {
                message(Sys.time(), ": No new jobs")
              }
              
              jobDetails <- data.frame()
              jobAds <- data.frame()
              
              remDr$client$setImplicitWaitTimeout(3000)
              
              #next page
              message(Sys.time(), ": Locating next page link")
              nextPageLinks <- suppressMessages(try(remDr$client$findElements('class', 'np'),TRUE))
              
              if(length(nextPageLinks) == 0 || class(nextPageLinks) == 'try-error')
              {
                message(Sys.time(), ": Last page detected. Sleeping 1")
                Sys.sleep(1)
                break
              }
              
              #get the last link which is next page
              nextPageLink <- try(nextPageLinks[[length(nextPageLinks)]],TRUE)
              
              remDr$client$setImplicitWaitTimeout(20000)
              
              message(Sys.time(), ": Sleeping 1")
              Sys.sleep(1)
              
              if(!grepl("Próxima", unlist(nextPageLink$getElementText())) || class(nextPageLink) == 'try-error')
              {
                message(Sys.time(), ": Last page detected. Sleeping 1")
                Sys.sleep(1)
                break
              }
              
              message(Sys.time(), ": Sleeping 3")
              Sys.sleep(3)
              
              message(Sys.time(), ": Clicking next page")
              #      nextPageLink$clickElement()
              
              res <- try(nextPageLink$clickElement(), TRUE)
              
              repeatCount <- 0
              while(class(res) == 'try-error')
              {
                if(repeatCount > 3)
                  break
                
                message(Sys.time(), ": *** Detecting popover ... ")
                
                remDr$client$setImplicitWaitTimeout(1000)
                
                popupx <- try(remDr$client$findElement("id", "popover-x-button"), TRUE)
                
                if(class(popupx) == "try-error")
                  message(Sys.time(), ": *** popover not detected.")
                else
                {
                  res <- try(popupx$clickElement(), TRUE)
                }
                
                if(class(res) == "try-error")
                {
                  #remove tos strip at bottom. Somehow prevents clicking on new page
                  message(Sys.time(), ": *** Detecting ToS bar ... ")
                  
                  tosButton <- try(remDr$client$findElement("class", "tos-Button"), TRUE)
  
                  if(class(tosButton) == 'try-error')
                    message(Sys.time(), ": *** ToS not detected")
                  else
                  {
                    res <- try(tosButton$clickElement())
                    message(Sys.time(), ": *** ToS detected and dismissed")
                  }
                }

                Sys.sleep(1)
                
                if(class(res) != 'try-error')
                  res <- nextPageLink$clickElement()
              }
              
              #check if we should exit i.e. user hit cancel
              #in which case client will be in processing but not in queue
              #If so delete from processing and exit loop
              
              message(Sys.time(), ": clientId: ", clientId)
              if(isClientProcessingCancelled(userId, clientId))
              {
                message(Sys.time(), ": Received cancel for client: ", clientId)
                removeClientFromProcessing(userId, siteId, clientId)
                cancelClientSearch <- TRUE
                break
              }
              
              if(cancelClientSearch)
                break
            } #repeat until there's no next page link
            
            #finished search for one criterion
            #mark client search with date/time to show complete
            #so next stage can begin ranking jobs 
          } #end for(searchTxt in searchTxts)
          
          #we've completed processing the client
          #remove client from processing and queue
          message(Sys.time(), ": Completed processing: ", clientId)
          removeClientFromProcessing(userId, siteId, clientId)
          removeClientFromUserQueue(userId, clientId)
          
          #ranking client jobs
          message(Sys.time(), ": Ranking client results")
          rankClientJobs(clientId)
          
          res <- try(putUserNotification(userId, "warning",paste0("Search for client ", clientId, ": ",  getClientName(clientId), " complete. New Results may be available")), TRUE)
        } #end repeat forever
        
        #completed so reset sinks
        message(Sys.time(), ": Run complete")
        
        if(!serviceMode || exitGraceful)
        {
          if(!serviceMode)
            message("Not in service mode. Exiting")
          
          if(exitGraceful)
            message("Exiting gracefully")
          return()
        }
        
      }, error=function(err)
      {
        print(as.character(err))
        if(!is.null(clientId))
        {
          message(Sys.time(), ": removing client ", clientId, " from  processing" )
          removeClientFromProcessing(userId, siteId, clientId)
        }
        
        message(Sys.time(), ": Closing browser window")
        
        if(!is.null(remDr))
        {
          res <- try(remDr$client$close(),TRUE)
          remDr <- NULL
        }
        
        message(Sys.time(), ": Trying again in 10")
        
        Sys.sleep(10)
      }, finally =
      {
        #logout?
        #cleanup(userId, siteId, remDr)
        if(!is.null(remDr))
        {
          res <- try(remDr$client$close(),TRUE)
          remDr <- NULL
        }
      })
    
    if(class(exitCond) == 'error')
    {
      exitCond <- NULL
      sleepTime <- 30
      message(Sys.time(), ": An error occurred. Trying again in ", sleepTime)
      Sys.sleep(sleepTime)
      next
    }
  } #repeat start
}