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

if(!require(prodlim))
{
  install.packages("prodlim")
}

if(!require(rapportools))
{
  install.packages("rapportools")
}

source("utils.R")
source("credentials.R")
source("rankjobs.R")

logcon <- NULL

startup <- function()
{
  message(Sys.time(), ": sending output to logfile")
  logcon <<- file(logfile)
  sink(logcon, append = TRUE)
  sink(logcon, append = TRUE, type = "message")
}

cleanup <- function()
{
  message(Sys.time(), ": Clean up and exit")
  
  userName <- getUsersInProcessing()
  
  if(!is.null(userName) && length(userName) > 0 && userName != "")
  {
    message(Sys.time(), ": Ranking user results")
    rankUserJobs(userName)
    
    message(Sys.time(), ": Removing processing user: ", userName)
    
    removeUserFromProcessing(userName)
  }
  
  message(Sys.time(), ": Sending output back to default")
  #reset sinks
  sink()
  sink(type = 'message')
  close(logcon)
}

exit <- function()
{
  cleanup()
  return()  
}

#searchCatho <- function(users, logfile=path.expand(file.path("~", paste0("hrappsearchlog.log"))))
searchIndeed <- function()
{
  repeat{
    exitCond <- tryCatch(
      {
        startup()
        
        #check for exit command if restarting
        #probably due to an error
        cmdQueue <- popCmdQueue()
        
        if(length(grep("exit", cmdQueue)) > 0)
        {
          message(Sys.time(), ": Received exit command")
          exit()
        }
        
        message(Sys.time(), ": =========== START searchCATHO ", " ===========")
        #message(Sys.time(), ": users:", users)
        
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
        
        eCap <- list(phantomjs.page.settings.userAgent 
                     = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:29.0) Gecko/20120101 Firefox/29.0")
        #"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/53 (KHTML, like Gecko) Chrome/15.0.87"
        
        eCaps <- list(chromeOptions = list(  args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
        
        if(is.null(pJS))
        {
          message(Sys.time(), ": pJS is null. Checking if phantomjs is running from another session ...")
          #phantomjs
          
          repeatCount <- 0      
          
          repeat
          {
            res <- system('tasklist /FI "IMAGENAME eq phantomjs.exe"', intern = T)
            
            if(any(grepl("phantomjs.exe", res)))
            {
              message(Sys.time(), ": a previous phantomjs is running ...")
              #system("taskkill /IM phantomjs.exe /F /T")
              #Sys.sleep(1)
              break
            }
            
            message(Sys.time(), ": starting phantomjs")
            pJS <- try(wdman::phantomjs())
            
            Sys.sleep(5)
            
            if(class(pJS) == 'try-error')
            {
              if(length(grep("in use", as.character(attr(pJS, "condition")))) >0)
              {
                message(Sys.time(), ": pJS already running ...")
                #system("taskkill /IM phantomjs.exe /F /T")
                
                #message(Sys.time(), ": restarting")
                repeatCount <- repeatCount + 1
                #next
                Sys.sleep(1)
                break
              }else
                stop("Unknown error")
            }else
            {
              message(Sys.time(), ": Seems started ok")
              Sys.sleep(1)
              break
            }
            
            if(repeatCount > 2)
              stop("Couldn't start phantomJS")
          }
          
          Sys.sleep(5) # give the binary a moment
        }
        
        message(Sys.time(), ": Trying to connect remDr to pjs")
        
        if(is.null(remDr$client))
        {
          message(Sys.time(), ": remote driver not started. Starting ...")
          remDr$client <- remoteDriver(extraCapabilities = eCaps, port=4567L)
          remDr$client$open(silent = T)
          remDr$client$setWindowSize(1920, 1080)
        }else
        {
          
        }
        
        message(Sys.time(), ": checking that we are connected")
        if(remDr$client$.objectPackage == "RSelenium" && is.environment(remDr$client$.objectParent))
        {
          message(Sys.time(), ": We are connected")
          # go to the webpage
          
          message(Sys.time(), ": Loading previous results")
          if(file.exists("jobsearchresults.csv"))
          {
            searchResults <- readr::read_csv("jobsearchresults.csv", col_types = paste0(rep_len('c',25),collapse=""))
          }else
          {
            searchResults <- data.frame()
          }
          
          message(Sys.time(), ": Opening main page")
          remDr$client$navigate("https://www.indeed.com.br/")
          
          message(Sys.time(), ": Sleeping 3")
          Sys.sleep(3)
        }else
        {
          if(is.null(remDr$client))
          {
            message(Sys.time(), ": remDr is null. Attempting to connect")
            Sys.sleep(5) # give the binary a moment
            remDr$client <- remoteDriver(extraCapabilities = eCaps, port=4567L)
            remDr$client$open(silent = T)
            remDr$client$setWindowSize(1920, 1080)
            
            # #rsDriver
            # remDr$client <- rsDriver(port=4445L, check=FALSE)
            # remDr$client$setImplicitWaitTimeout(milliseconds = 20000)
          }
          
        }
        
        cancelUserSearch <- FALSE
        
        #use main search bar only the first time
        firstSearch <- TRUE
        
        #for(userIdx in 1:length(users))
        #repeat forever
        repeat
        {
          #userName <- users[[userIdx]]$UserName
          
          #message(Sys.time(), ": Processing user ", userIdx, "[", userName, "]")
          
          #searchTxts <- users[[userIdx]]$searchTxts
          
          if(cancelUserSearch)
          {
            if(exists("userName") && userName != "")
            {
              message(Sys.time(), ": Removing cancelled user from queue")
              removeUserFromQueue(userName)
              
              #updating user ranking for userName
              rankUserJobs(userName)
            }
            cancelUserSearch <- FALSE
          }
          
          #check if we have an exit command
          #after processing a user
          cmdQueue <- popCmdQueue()
          
          if(length(grep("exit", cmdQueue)) > 0)
          {
            message(Sys.time(), ": Received exit command")
            exit()
          }
          
          userqueue <- getUsersInQueue()
          
          if(length(userqueue) == 0)
          {
            #check for exit command even when there
            #are no users in the queue
            cmdQueue <- popCmdQueue()
            
            if(length(grep("exit", cmdQueue)) > 0)
            {
              message(Sys.time(), ": Received exit command")
              exit()
            }
            
            message(Sys.time(), ": No users to process. Sleeping")
            Sys.sleep(10)
            next
          }
          
          userName <- userqueue[1]
          userIdx <- 1
          
          putUserInProcessing(userName)
          
          message(Sys.time(), ": Processing user: ", userName)
          
          clients <- read.csv("clients.csv", header = T, encoding = 'UTF-8')
          
          searchTxts <- as.character(clients[which(clients$Pessoal_NomeCompleto == userName), c("Posicao_Cargo_Interesse1", "Posicao_Cargo_Interesse2","Posicao_Cargo_Interesse3")])
          
          for(searchTxt in searchTxts)
          {
            if(isUserProcessingCancelled(userName))
            {
              message(Sys.time(), ": Received cancel for user: ", userName)
              removeUserFromProcessing(userName)
              cancelUserSearch <- TRUE
              break
            }
            
            if(cancelUserSearch)
              break
            
            message(Sys.time(), ": Search criteria: ", which(searchTxts == searchTxt), ": '", searchTxt, "'")
            
            repeat
            {
              searchBar <- suppressMessages(try(remDr$client$findElement('id', "text-input-what"), TRUE))
              
              if(class(searchBar) == "try-error")
                searchBar <- suppressMessages(try(remDr$client$findElement('id', "what"), TRUE))
              
              if(class(searchBar) == "try-error")
              {
                message("Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            message("Clicking on search bar")
            #click on the search bar
            searchBar$clickElement()
            
            message("Clearing search bar")
            #clear the search bar
            searchBar$clearElement()
            
            message("Entering search criteria '", searchTxt, "'")
            remDr$client$sendKeysToActiveElement(list(paste0(searchTxt)))
            
            #Entering location
            message("Entering search location")
            repeat
            {
              searchWhere <- suppressMessages(try(remDr$client$findElement('id', "text-input-where"), TRUE))
              
              if(class(searchBar) == "try-error")
                searchWhere <- suppressMessages(try(remDr$client$findElement('id', "where"), TRUE))
              
              if(class(searchWhere) == "try-error")
              {
                message("Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            message("Clicking on search location")
            #click on the search bar
            searchWhere$clickElement()
            
            message("Clearing search location")
            #clear the search bar
            searchWhere$clearElement()
            
            message("Entering search location")
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
              if(pgNum > 1)
                break
              
              if(isUserProcessingCancelled(userName))
              {
                message(Sys.time(), ": Received cancel for user: ", userName)
                removeUserFromProcessing(userName)
                cancelUserSearch <- TRUE
                break
              }
              
              if(cancelUserSearch)
                break
              
              message(Sys.time(), ": Sleeping 3")
              Sys.sleep(3)
              pgNum <- pgNum + 1
              
              message(Sys.time(), ": On Page ", pgNum)
              
              #get all job titles and companies on the page
              #since later after clicking on the job
              #the page structure changes
              repeat
              {
                message("Getting job nodes")
                jobNodes <- suppressMessages(try(remDr$client$findElements('class',"clickcard"),TRUE))
                
                if(class(jobNodes) == "try-error")
                {
                  message("Not yet. Sleeping 1")
                  Sys.sleep(1)
                  next
                }
                break
              }
              
              if(length(jobNodes) > 0)
              {
                message("Getting job titles")
                jobTitles <- unlist(sapply(jobNodes, 
                                           function(x)
                                           {
                                             a <- x$findElement("class", "jobtitle")
                                             
                                             b <- a$findChildElement("class", "turnstileLink")
                                             
                                             b$getElementAttribute("title")
                                           }
                )
                )
                
                message("Getting job urls")
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
                
                message("Getting company names")
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
                
                message("Getting job summaries")
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
                
                message("Combining company details")
                #cbind jobnames with details
                jobAds <- dplyr::bind_cols("jobTitle"=jobTitles, "jobUrl"=jobUrls, "companyName"=jobCompanies, "jobDescriptionShort"=jobSummaries)
                
                message("Adding timestamp, username, search txt")
                #add timestamp, username and searchtext
                jobAds <- dplyr::bind_cols("searchTime"=rep(Sys.time(), nrow(jobAds)),
                                           "userName"=rep(userName, nrow(jobAds)),
                                           "searchTxt"=rep(searchTxt, nrow(jobAds)),
                                           jobAds)
                
                if(nrow(jobAds) > 0)
                {
                  matchCols <- c("jobUrl")
                  
                  if(nrow(searchResults) > 0)
                  {
                    #check for existing rows in searchResults
                    existingJobAdsIdx <- suppressWarnings(prodlim::row.match(select(jobAds, select=matchCols), select(searchResults, select=matchCols)))
                    
                    #remove NAs returned by row.match
                    existingJobAdsIdx <- existingJobAdsIdx[!is.na(existingJobAdsIdx)]
                    
                    message(length(existingJobAdsIdx), "/", nrow(jobAds), " existing jobs found")
                    
                    # #locate the rows in searchResults that match our found jobAds
                    # #and update the search criteria to show the search criteria that yielded
                    # #this result. Will save time in future searches
                    existingPos <- suppressWarnings(prodlim::row.match(searchResults[,matchCols], jobAds[,matchCols]))
                    
                    #remove NAs created by match.row
                    existingPos <- existingPos[!is.na(existingPos)]
                    
                    #if any jobs were previously found using other searchTxts
                    #add this searchTxt
                    if(length(existingPos) > 0)
                    {
                      #check which rows were found by other searches
                      posUpdSrchTxt <- unlist(lapply(searchResults[existingPos, "searchTxt"], function(x) !grep(searchTxt, x)))
                      
                      if(length(posUpdSrchTxt) > 0)
                      {
                        message(Sys.time(), ": Updating search criteria for ", length(posUpdSrchTxt), " rows")
                        
                        searchResults[posUpdSrchTxt, "searchTxt"] <- paste0(searchResults[posUpdSrchTxt, "searchTxt"], "|", searchTxt)
                      }
                    }
                    
                    notExistingJobAdsIdx <- !(1:nrow(jobAds) %in% existingPos)
                    
                    allJobs <- allJobs + nrow(jobAds)
                    newJobs <- newJobs + length(notExistingJobAdsIdx)
                    
                    message(sum(notExistingJobAdsIdx), "/", nrow(jobAds), " new jobs found. Appending")
                    
                    jobAds <- jobAds[notExistingJobAdsIdx,]
                  }
                  
                  #add a rank column which will be populated later
                  jobAds <- cbind.data.frame(jobAds, "Rank"=NA)
                  
                  #Append jobAds to searchResults
                  message(Sys.time(), ": Appending jobAds to searchResults")
                  searchResults <- dplyr::bind_rows(searchResults, jobAds)
                  
                  #write to search results csv 
                  message(Sys.time(), ": Writing results to file")
                  readr::write_csv(searchResults, "jobsearchresults.csv")
                }
              }else
              {
                message(Sys.time(), ": No new jobs")
              }
              
              jobDetails <- data.frame()
              jobAds <- data.frame()
              
              remDr$client$setImplicitWaitTimeout(3000)
              
              #next page
              message("Locating next page link")
              nextPageLinks <- suppressMessages(try(remDr$client$findElements('class', 'np'),TRUE))
              
              nextPageLink <- nextPageLinks[[length(nextPageLinks)]]
              
              remDr$client$setImplicitWaitTimeout(20000)
              
              message("Sleeping 1")
              Sys.sleep(1)
              
              if(class(nextPageLink) == 'try-error')
              {
                message("Last page detected. Sleeping 1")
                Sys.sleep(1)
                break
              }
              
              message("Sleeping 3")
              Sys.sleep(3)
              
              message("Clicking next page")
              #      nextPageLink$clickElement()
              
              tryCatch(nextPageLink$clickElement(),
                       error=function(err)
                       {
                         message("*** ERROR: ", err)
                         
                         popupx <- remDr$client$findElement("id", "popover-x-button")
                         
                         popupx$clickElement()
                         
                         Sys.sleep(3)
                         
                         nextPageLink$clickElement()
                       })
              
              #check if we should exit i.e. user hit cancel
              #in which case user will be in processing but not in queue
              #If so delete from processing and exit loop
              
              message(Sys.time(), ": userName: ", userName)
              if(isUserProcessingCancelled(userName))
              {
                message(Sys.time(), ": Received cancel for user: ", userName)
                removeUserFromProcessing(userName)
                cancelUserSearch <- TRUE
                break
              }
              
              if(cancelUserSearch)
                break
            } #repeat until there's no next page link
            
            #finished search for one criterion
            #mark user search with date/time to show complete
            #so next stage can begin ranking jobs 
          } #end for(searchTxt in searchTxts)
          
          #we've completed processing the user
          #remove user from processing and queue
          message(Sys.time(), ": Completed processing: ", userName)
          removeUserFromProcessing(userName)
          removeUserFromQueue(userName)
          
          #ranking user jobs
          message(Sys.time(), ": Ranking user results")
          rankUserJobs(userName)
          
          
        } #end repeat forever
        
        #completed so reset sinks
        message(Sys.time(), ": Run complete. Returning")
        
        #rankUserJobs(userName)
        
        #sink()
        #sink(type = 'message')
        
        #return(c(newJobs, allJobs))
      }, error=function(err)
      {
        print(as.character(err))
        message(Sys.time(), ": Trying again in 10")
        Sys.sleep(10)
      }, finally =
        {
          #logout?
          cleanup()
          
          #return()
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

#searchCatho()
