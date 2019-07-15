if(!require(RSelenium))
{
  install.packages("RSelenium")
}

if(!require(readr))
{
  install.packages(readr)
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
serviceMode <- TRUE

startup <- function(userId)
{
  #message(Sys.time(), ": sending output to logfile")
  #logfile <- paste("searchvagas", userId, ".log", sep="_")
  #logcon <<- file(logfile)
  #sink(logcon, append = TRUE)
  #sink(logcon, append = TRUE, type = "message")
  #message(Sys.time(), ": start logging to logfile")
}

cleanup <- function(userId, siteId, remDr)
{
  message(Sys.time(), ": Clean up and exit")
  
  clientId <- getClientsInSiteProcessing(userId, siteId)$Id
  
  if(!is.null(clientId))
  {
    message(Sys.time(), ": Ranking client results")
    rankClientJobs(clientId)
    
    res <- try(putUserNotification(userId, "warning", paste0("Search for client ", clientId, ": ", getClientName(clientId), " terminated. New Results may be available")),TRUE)
    
    message(Sys.time(), ": Removing processing client: ", clientId)
    
    removeClientFromProcessing(userId, siteId, clientId)
  }
  
  #RMariaDB::dbDisconnect(dbCon)
  
  message(Sys.time(), ": Closing browser window")
  if(!is.null(remDr))
  {
    res <- try(remDr$client$close(), FALSE)
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

searchVagas <- function(userId)
{
  siteId <- 3
  clientId <- NULL
  remDr <- NULL
  exitGraceful <- TRUE
  startup(userId)
  remSvrAddr <- '127.0.0.1'
  
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
        
        message(Sys.time(), ": =========== START searchVAGAS UserId: ", userId, " ===========")
        
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
          remDr$client$navigate("https://www.vagas.com.br/")
          
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
              
              searchBar <- suppressMessages(try(remDr$client$findElement('id', "nova-home-search"), TRUE))
              
              if(class(searchBar) == "try-error")
                searchBar <- suppressMessages(try(remDr$client$findElement('name', "q"), TRUE))
              
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
            remDr$client$setImplicitWaitTimeout(milliseconds = 20000)
            
            
            message(Sys.time(), ": Clearing search bar")
            #clear the search bar
            searchBar$clearElement()
            
            message(Sys.time(), ": Entering search criteria '", searchTxt, "'")
            #remDr$client$sendKeysToActiveElement(list(paste0(searchTxt)))
            searchBar$sendKeysToElement(sendKeys = list(paste0(searchTxt), key = "enter") )
            
            
            #submitting the search
            message("Locating search button")
            
            # repeat
            # {
            #   searchButton <- suppressMessages(try(remDr$client$findElement('class', "bx-search"),TRUE))
            #   
            #   if(class(searchButton) == "try-error")
            #   {
            #     message("Not yet. Sleeping 1")
            #     Sys.sleep(1)
            #     next
            #   }
            #   break
            # }
            # 
            message("Clicking search button")
            #searchButton$clickElement()
            
            
            message("Sleeping 3")
            Sys.sleep(3)
            
            message("Setting cidade filter criteria")
            
            #Setting the cidade filter to Sao Paulo
            
            repeat
            {
              cidadeInput <- suppressMessages(try(remDr$client$findElement('class', "js-facet-ul"),TRUE))
              
              if(class(cidadeInput) == "try-error")
              {
                message("Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            remDr$client$setImplicitWaitTimeout(20000)
            
            #taking a screenshot
            remDr$client$screenshot(display = TRUE)
            
            cidadeElements <- cidadeInput$findChildElements("xpath", "li")
            
            remDr$client$setImplicitWaitTimeout(20000)
            
            message("clicking cidade elements")
            cidadeElements[[1]]$clickElement()
            
            #taking a screenshot
            remDr$client$screenshot(display = TRUE)
            
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
              
              repeatCount <- 0
              
              repeat
              {
                if(repeatCount > maxRepeatCount)
                  break
                
                repeatCount <- repeatCount + 1
                
                message(Sys.time(), ": Getting job nodes")
                jobNodes <- suppressMessages(try(remDr$client$findElements('class',"informacoes-header"),TRUE))
                
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
                                             a <- x$findElement("class", "cargo")
                                             
                                             b <- a$findChildElement("class", "link-detalhes-vaga")
                                             
                                             b$getElementAttribute("title")
                                           }
                )
                )
                
                message(Sys.time(), ": Getting job urls")
                jobUrls <- unlist(sapply(jobNodes, 
                                         function(x)
                                         {
                                           a <- x$findElement("class", "cargo")
                                           
                                           b <- a$findChildElement("class", "link-detalhes-vaga")
                                           
                                           b$getElementAttribute("href")
                                         }
                )
                )
                
                remDr$client$setImplicitWaitTimeout(1000)
                
                message(Sys.time(), ": Getting company names")
                companyNameNodes <- suppressMessages(try(remDr$client$findElements('class',"emprVaga"),TRUE))
                
                if(class(companyNameNodes) == "try-error")
                {
                  message(Sys.time(), ": Not yet. Sleeping 1")
                  Sys.sleep(1)
                  next
                }
                break
              }
              companyNames <- unlist(sapply(companyNameNodes, function(x) x$getElementText()))
              remDr$client$setImplicitWaitTimeout(1000)
              
              message(Sys.time(), ": Getting job summaries")
              summaryNodes <- suppressMessages(try(remDr$client$findElements('class',"detalhes"),TRUE))
              
              if(class(summaryNodes) == "try-error")
              {
                message(Sys.time(), ": Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              
              jobSummaries <- unlist(sapply(summaryNodes, function(x) x$getElementText()))
              
              remDr$setImplicitWaitTimeout(20000)
              
              message(Sys.time(), ": Combining company details")
              #cbind jobnames with details
              jobAds <- dplyr::bind_cols("JobTitle"=jobTitles, "JobUrl"=jobUrls, "CompanyName"=companyNames, "ShortDescription"=jobSummaries)
              
              #Excluding the confidencial ads
              
              notConfidencial <- grep("CONFIDENCIAL", jobAds$CompanyName, invert = T, ignore.case = T)
              
              jobAds <- jobAds[notConfidencial,]
              
              #adding timestamp, clientId and searchtext
              jobAds <- dplyr::bind_cols("SearchTime"=rep(Sys.time(), nrow(jobAds)),
                                         "ClientId"=rep(clientId, nrow(jobAds)),
                                         "SiteId"=rep(siteId, nrow(jobAds)),
                                         "SearchTxt"=rep(searchTxt, nrow(jobAds)),
                                         jobAds)
              
              #write to search results csv 
              message(Sys.time(), ": Writing results to db")
              res <- putClientJobSearchResults(jobAds)
            }
            
            jobAds <- data.frame()
            
            remDr$client$setImplicitWaitTimeout(3000)
            
            #next page
            message("Locating next page link")
            nextPageLink <- suppressMessages(try(remDr$client$findElement('id', "maisVagas"),TRUE))
            
            remDr$client$setImplicitWaitTimeout(20000)
            
            message("Sleeping 1")
            Sys.sleep(1)
            
            if(class(nextPageLink) == 'try-error')
            {
              message("Last page detected. Sleeping 1")
              Sys.sleep(1)
              break
            }
            
            message(Sys.time(), ": Sleeping 3")
            Sys.sleep(3)
            
            message(Sys.time(), ": Clicking next page")
            nextPageLink$clickElement()
            
            message(Sys.time(), ": Sleeping 5")
            Sys.sleep(5)
            
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
            #repeat until there's no next page link
            
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
          res <- try(putUserNotification(userId, "warning", paste0("Search for client ", clientId, ": ", getClientName(clientId), " completed. Results may be available")), TRUE)
          
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
        
      },error=function(err)
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
    
  }#repeat start
}












