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

source("utils.R")
source("credentials.R")
source("rankjobs.R")

logcon <- NULL
serviceMode <- TRUE

startup <- function(userId)
{
  # message(Sys.time(), ": sending output to logfile")
  # logfile <- paste("searchcatho", userId, ".log", sep="_")
  # logcon <<- file(logfile)
  # sink(logcon, append = TRUE)
  # sink(logcon, append = TRUE, type = "message")
  # message(Sys.time(), ": start logging to logfile")
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

searchCatho <- function(userId)
{
  siteId <- 1
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
      
      message(Sys.time(), ": =========== START searchCATHO UserId: ", userId, " ===========")

      maxRepeatCount <- 30
      
      if(!exists("remDr$client"))
      {
        remDr <- list()
        remDr$client <- NULL 
      }
      
      if(!exists("pJS"))
        pJS <- NULL

      options(stringsAsFactors = FALSE)
      
      message(Sys.time(), ": Launching browser")

      eCaps <- list(chromeOptions = list(  args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
      
      if(is.null(remDr$client))
      {
        repeat
        {
          message(Sys.time(), ": remote driver not started. Starting ...")
          remDr$client <- try(remoteDriver(remoteServerAddr=remSvrAddr, port=4444L, browserName="chrome", extraCapabilities = eCaps), TRUE)
          
          message("Checking if we are connected to Selenium Hub")
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
        
        #set browser window size
        remDr$client$setWindowSize(1920, 1080)

        message(Sys.time(), ": Opening main page")
        remDr$client$navigate("https://www.catho.com.br/")
        
        Sys.sleep(3)
        message(Sys.time(), ": Logging in")
        
        message(Sys.time(), ": Locating login link")
        repeatCount <- 0
        repeat
        {
          repeatCount <- repeatCount + 1
          
          if(repeatCount > maxRepeatCount)
            break
          
          loginLink <- suppressMessages(try(remDr$client$findElement(using = 'class', value = "btnLogin"), TRUE))
          if(class(loginLink) == "try-error")
          {
            message(Sys.time(), ": Not yet. Sleeping 1")
            Sys.sleep(1)
            next
          }
          
          break
        }
        
        message(Sys.time(), ": *** Detecting popover ... ")
        
        #remDr$client$setImplicitWaitTimeout(1000)
        
        popupCancelBtn <- try(remDr$client$findElement("id", "onesignal-popover-cancel-button"), TRUE)
        
        if(class(popupCancelBtn) == "try-error")
        {
          message(Sys.time(), ": *** popover not detected.")
        }
        else
        {
          message(Sys.time(), ": *** popover found. Dismissing")
          res <- try(popupCancelBtn$clickElement(), TRUE)
        }

        Sys.sleep(1)
        
        #remDr$client$setImplicitWaitTimeout(20000)
        
        message(Sys.time(), ": Clicking login link")
        loginLink$clickElement()
        
        message(Sys.time(), ": Locating login iframe")
        repeatCount <- 0
        repeat
        {
          repeatCount <- repeatCount + 1
          
          if(repeatCount > maxRepeatCount)
            break
          
          loginIframe <- suppressMessages(try(remDr$client$findElement(using = 'id', value = "loginHttps"), TRUE))
          if(class(loginIframe) == "try-error")
          {
            message(Sys.time(), ": Not yet. Sleeping 1")
            Sys.sleep(1)
            next
          }
          break
        }
        
        remDr$client$switchToFrame(loginIframe)
        
        message(Sys.time(), ": Locating username login input")
        repeatCount <- 0
        repeat
        {
          repeatCount <- repeatCount + 1
          
          if(repeatCount > maxRepeatCount)
            break
          
          userNameInput <- suppressMessages(try(remDr$client$findElement('id', 'loginUsuario'),TRUE))
          if(class(userNameInput) == "try-error")
          {
            message(Sys.time(), ": Not yet. Sleeping 1")
            Sys.sleep(1)
            next
          }
          
          break
        }
        
        
        userNameInput$clickElement()
        
        remDr$client$sendKeysToActiveElement(list(cathoLogin))
        
        message(Sys.time(), ": Locating password input")
        repeatCount <- 0
        repeat
        {
          repeatCount <- repeatCount + 1
          
          if(repeatCount > maxRepeatCount)
            break
          
          passInput <- suppressMessages(try(remDr$client$findElement('id', 'senhaUsuario'),TRUE))
          if(class(passInput) == "try-error")
          {
            message(Sys.time(), ": Not yet. Sleeping 1")
            Sys.sleep(1)
            next
          }
          break
        }
        
        passInput$clickElement()
        
        remDr$client$sendKeysToActiveElement(list(cathoPass))
        
        message(Sys.time(), ": Locating login button")
        repeatCount <- 0
        
        repeat
        {
          repeatCount <- repeatCount + 1
          
          if(repeatCount > maxRepeatCount)
            break
          
          btnOkLogin <- suppressMessages(try(remDr$client$findElement('class', 'btnOkLogin'),TRUE))
          if(class(btnOkLogin) == "try-error")
          {
            message(Sys.time(), ": Not yet. Sleeping 1")
            Sys.sleep(1)
            next
          }
          break
        }
        
        message(Sys.time(), ": Clicking login button")
        btnOkLogin$clickElement()
        
        message(Sys.time(), ": Sleeping 3")
        Sys.sleep(3)
      }

      message(Sys.time(), ": Session exists. Trying search")
      
      cancelClientSearch <- FALSE
      
      #use main search bar only the first time
      firstSearch <- TRUE
      
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
            
            res <- try(putUserNotification(userId, "warning", paste0("Search for client ", clientId, ": ", getClientName(clientId), " terminated. Results may be available")), TRUE)
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
          #are no users in the queue
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
        userIdx <- 1
        
        putClientInProcessing(userId, siteId, clientId)
        
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
          
          #only use search bar first time
          #also only set salary first time
          #if(userIdx == 1 && searchTxt == searchTxts[1])
          if(firstSearch)
          {
            #set to not use this section subsequently
            firstSearch <- FALSE
            
            message(Sys.time(), ": Getting search bar element")
            #enter interest as search criteria
            repeatCount <- 0
            repeat
            {
              repeatCount <- repeatCount + 1
              
              if(repeatCount > maxRepeatCount)
                break
              
              searchBar <- suppressMessages(try(remDr$client$findElement('class', "inputBusca"), TRUE))
              
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
            remDr$client$sendKeysToActiveElement(list(searchTxt))
            
            #submit the search
            message(Sys.time(), ": Locating search button")
            
            repeatCount <- 0
            repeat
            {
              repeatCount <- repeatCount + 1
              
              if(repeatCount > maxRepeatCount)
                break
              
              btnBuscar <- suppressMessages(try(remDr$client$findElement('class', "btnBuscar"),TRUE))
              
              if(class(btnBuscar) == "try-error")
              {
                message(Sys.time(), ": Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            message(Sys.time(), ": Clicking search button")
            btnBuscar$clickElement()
            
            message(Sys.time(), ": Sleeping 3")
            Sys.sleep(3)
            
            message(Sys.time(), ": Setting cidade filter criteria")
            message(Sys.time(), ": Locating cidade filter dropdown")
            #filter by choosing sidade Sao Paulo (estado)
            repeatCount <- 0
            repeat
            {
              repeatCount <- repeatCount + 1
              
              if(repeatCount > maxRepeatCount)
                break
              
              cidadeInput <- suppressMessages(try(remDr$client$findElement('id', "cidade"),TRUE))
              
              if(class(cidadeInput) == "try-error")
              {
                message(Sys.time(), ": Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            message(Sys.time(), ": clicking cidade input")
            cidadeInput$clickElement()
            
            remDr$client$sendKeysToActiveElement(list("São Paulo (estado)"))
            
            cidadeInput$submitElement()
            
            Sys.sleep(5)
            
            message(Sys.time(), ": Filtering by salario > 6000")  
            #filter by choosing salary 
            repeat
            {
              repeatCount <- 0
              salarioDropDown <- suppressMessages(try(remDr$client$findElement('name', "faixa_sal_id"),TRUE))
              if(class(searchBar) == "try-error")
              {
                repeatCount <- repeatCount + 1
                
                if(repeatCount > maxRepeatCount)
                  break
                
                message(Sys.time(), ": Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            #cidadeInput$clickElement()
            
            salarioOptions <- salarioDropDown$selectTag()
            
            over6k <- grep('6.000,00', salarioOptions$text)
            
            salarioOptions$elements[[over6k]]$clickElement()
          }else
          {
            #subsequent searches alter search criteria
            #but search is now on right side
            #salario already set so no need to change
            repeatCount <- 0
            repeat
            {
              repeatCount <- repeatCount + 1
              
              if(repeatCount > maxRepeatCount)
                break
              
              searchBarSide <- suppressMessages(try(remDr$client$findElement('id', "cargoDesejado"), TRUE))
              
              if(class(searchBarSide) == "try-error")
              {
                message(Sys.time(), ": Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            searchBarSide$clearElement()
            
            searchBarSide$clickElement()
            
            remDr$client$sendKeysToActiveElement(list(searchTxt))
            
            searchBarSide$submitElement()
          }
          
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
            
            message(Sys.time(), ": Getting company Details")
            
            #get all job titles and companies on the page
            #since later after clicking on the job
            #the page structure changes
            repeatCount <- 0
            repeat
            {
              repeatCount <- repeatCount + 1
              
              if(repeatCount > maxRepeatCount)
                break
              
              message(Sys.time(), ": Getting job titles")
              jobTitleNodes <- suppressMessages(try(remDr$client$findElements('class',"viewVagaAction"),TRUE))
              
              if(class(jobTitleNodes) == "try-error")
              {
                message(Sys.time(), ": Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            jobTitles <- unlist(sapply(jobTitleNodes, function(x) x$getElementText()))
            
            jobTitlesLocY <- sapply(jobTitleNodes, function(x) x$getElementLocation()$y)
            
            jobUrls <- unlist(sapply(jobTitleNodes, function(x) x$getElementAttribute("href")))
            
            repeatCount <- 0
            repeat
            {
              repeatCount <- repeatCount + 1
              
              if(repeatCount > maxRepeatCount)
                break
              
              message(Sys.time(), ": Getting company names")
              companyNameNodes <- suppressMessages(try(remDr$client$findElements('css',"div.dadosEmpresa p"),TRUE))
              
              if(class(companyNameNodes) == "try-error")
              {
                message(Sys.time(), ": Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            companyNames <- unlist(sapply(companyNameNodes, function(x) x$getElementText()))
            
            companyNamesLocY <- sapply(companyNameNodes, function(x) x$getElementLocation()$y)
            
            #using the Y locations match job titles and company names
            #essentially job titles are at the top and company names
            #below. Can be multiple not sure why?
            #Company names in between job locYs are assigned to the previous job
            jobAds <- do.call(rbind.data.frame, lapply(1:length(jobTitles), function(i)
            {
              if(i == length(jobTitles))
                compNamePositions <- which(companyNamesLocY > jobTitlesLocY[i])
              else
                compNamePositions <- which(companyNamesLocY > jobTitlesLocY[i] & companyNamesLocY < jobTitlesLocY[i+1])
              
              cbind(jobTitles[i], paste(companyNames[compNamePositions], collapse = "|"))
            }))
            
            if(nrow(jobAds) > 0 && length(jobUrls) > 0)
            {
              jobAds <- cbind(jobAds, data.frame(jobUrls))
              names(jobAds) <- c("jobTitle", "companyName", "jobUrl")
            } else
            {
              jobAds <- data.frame("jobTitle"=NULL, "companyName"=NULL, "jobUrl"=NULL)
            }
            
            #find the confidencial ads
            notEmpresaConfidencial <- grep("EMPRESA CONFIDENCIAL", jobAds$companyName, invert = T, ignore.case = T)
            
            searchResults <- NULL

            message(Sys.time(), ": Getting job boxes")
            #get the summary details
            repeatCount <- 0
            repeat
            {
              repeatCount <- repeatCount + 1
              
              if(repeatCount > maxRepeatCount)
                break
              
              boxVaga <- suppressMessages(try(remDr$client$findElements('class', "boxVaga"),TRUE))
              if(class(boxVaga) == "try-error")
              {
                message(Sys.time(), ": Not yet. Sleeping 1")
                Sys.sleep(1)
                next
              }
              break
            }
            
            jobDetails <- data.frame()
            
            #remove empresaconfidencial ads
            jobAds <- jobAds[notEmpresaConfidencial,]
            
            #for each job on the page excluding confidencial
            for(i in notEmpresaConfidencial)
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
              
              message(Sys.time(), ": Processing job ", i)
              jobDetail <- data.frame()
              
              #set implicit timeout to zero to avoid waiting when elem doesn't exist
              remDr$client$setImplicitWaitTimeout(milliseconds = 1000)
              
              message(Sys.time(), ": Getting the announcement date")  
              jobAnnounceDate <- unlist(boxVaga[[i]]$findChildElement('class', 'dataAnuncio')$getElementAttribute("datetime"))
              
              jobDetail <- setNames(data.frame(as.character(jobAnnounceDate),
                                               stringsAsFactors = F),
                                    c("dataAnuncio"))
              
              message(Sys.time(), ": Getting the short description")
              
              jobDescriptionShortNode <- suppressMessages(try(boxVaga[[i]]$findChildElement('class', 'descriptionIncloplete'), TRUE))
              
              #reset our implicit timeout
              remDr$client$setImplicitWaitTimeout(milliseconds = 1000)
              
              #if there is no short description set it to NA
              if(class(jobDescriptionShortNode) == 'try-error')
              {
                message(Sys.time(), ": No short Description")
                #set values to NA
                jobDescriptionShort <- NA
              }else
              {
                message(Sys.time(), ": Found short description")
                jobDescriptionShort <- jobDescriptionShortNode$getElementText()
              }
              
              jobDetail <- dplyr::bind_cols(jobDetail, 
                                            setNames(data.frame(jobDescriptionShort), 
                                                     "jobDescriptionShort"))
              
              #get the link to click on the box
              moreDetailsLink <- boxVaga[[i]]$findChildElement('css', 'div header h2 a')
              
              #init jobDetailMore
              jobDetailMore <- NULL
              
              #dummy to be replaced with criteria to prevent
              #clicking on box if it is not clickable
              if(TRUE)
              {
                message(Sys.time(), ": Clicking on job description")
                
                message(Sys.time(), ": Sleeping 3")
                Sys.sleep(3)
                
                moreDetailsLink$clickElement()
                
                message(Sys.time(), ": Sleeping 5")
                Sys.sleep(5)
                
                message(Sys.time(), ": Getting the long description")  
                #click on boxVaga href
                
                # our implicit timeout to 0
                remDr$client$setImplicitWaitTimeout(milliseconds = 1000)
                
                jobDescriptionLongNode <- suppressMessages(try(boxVaga[[i]]$findChildElement('class', 'descriptionIncloplete'), TRUE))
                
                #seems like there are 2 elements depending on whether
                #there was text in the short node.
                #if shortnode has text descriptionIncloplete is used for both
                #short and long
                #if not then look for descricaoVaga div
                #if descriptionIncloplete was empty try descricaoVaga else NA
                if(class(jobDescriptionLongNode) == 'try-error')
                {
                  jobDescriptionLongNode <- suppressMessages(try(boxVaga[[i]]$findChildElement('css', 'div.descricaoVaga div'), TRUE))
                }
                
                #if still not found there's no long description
                if(class(jobDescriptionLongNode) == 'try-error')
                {
                  message(Sys.time(), ": No long Description")
                  #set missing fields to NAs
                  jobDescriptionLong <- NA
                }else
                {
                  message(Sys.time(), ": Found long description")
                  jobDescriptionLong <- jobDescriptionLongNode$getElementText()
                }
                
                #reset our implicit timeout
                remDr$client$setImplicitWaitTimeout(milliseconds = 20000)
                
                message(Sys.time(), ": Locating the box sections with job details")
                #find the box sections with job details
                groupDadosVaga <- boxVaga[[i]]$findChildElements('class', 'groupDadosVaga')
                
                message(Sys.time(), ": Found ", length(groupDadosVaga), " sections")
                #for each subsection in the job details
                if(length(groupDadosVaga) > 0)
                {
                  for(j in 1:length(groupDadosVaga))
                  {
                    message(Sys.time(), ": Processing section ", j)
                    Sys.sleep(1)
                    
                    message(Sys.time(), ": Getting child elements")
                    section1 <- groupDadosVaga[[j]]$findChildElements('css', 'div.groupDadosVaga section')
                    
                    section1Txt <- unlist(sapply(section1, function(x) x$getElementText()))
                    #then split header from content on \n
                    
                    section1Txt <- section1Txt[section1Txt != ""]
                    
                    if(is.null(section1Txt))
                      next
                    
                    message(Sys.time(), ": Splitting child elements")
                    section1Split <- lapply(strsplit(section1Txt, "\n"), unlist)
                    
                    #due to the css we are using we get duplicates since
                    #the first container has most of the section
                    #If we detect DADOS EMPRESA in section1 remove any
                    #other fields. They will be found in the other section parts
                    if(grepl(section1Split[[1]][1], "DADOS DA EMPRESA"))
                    {
                      message(Sys.time(), ": Removing duplicate elements")
                      section1Split[[1]] <- section1Split[[1]][1:2]
                    }
                    
                    message(Sys.time(), ": Collapsing complex elements")
                    #lists and any other section parts with other \n will result
                    #in multiple splits. Take only the first for header and collapse
                    #the rest
                    #creates a character matrix
                    section1Split <- sapply(section1Split, function(x) if(length(x) > 2) c(x[1], paste(x[2:length(x)], collapse="| ")) else x)
                    
                    message(Sys.time(), ": Cbinding elements")
                    #column bind the details in the sections
                    if(is.null(jobDetailMore))
                    {
                      jobDetailMore <- section1Split
                    }else
                    {
                      jobDetailMore <- cbind(jobDetailMore, section1Split)
                    }
                  }
                }else
                {
                  jobDetailMore <- data.frame("Final"=NA)
                }
                
                #if we did not find job details skip
                if(is.na(jobDetailMore))
                  next()
                
                message(Sys.time(), ": Putting elems in a DF")
                #convert the matrix into data.frame
                jobDetailMore <- data.frame(jobDetailMore, stringsAsFactors = F)
                
                #make the first row the header
                jobDetailMore <- setNames(jobDetailMore[2,],
                                          make.names(
                                            iconv(as.character(jobDetailMore[1,]), 
                                                  to="ASCII//TRANSLIT")
                                          )
                )
              }
              
              #first add the long description
              jobDetail <- dplyr::bind_cols(jobDetail, 
                                            setNames(data.frame(jobDescriptionLong), "jobDescriptionLong"))
              
              #if job details were found cbind them
              if(!is.null(jobDetailMore))
              {
                message(Sys.time(), ": Cbinding Job title, company name and job details")
                
                #message("jobDetail: ", jobDetail)
                #message("jobDetailMore: ", jobDetailMore)
                
                jobDetail <- try(dplyr::bind_cols(jobDetail, jobDetailMore), TRUE)
                if(class(jobDetail) == 'try-error')
                  next
              }
              
              #message("jobDetails: ", jobDetails)
              jobDetails <- try(dplyr::bind_rows(jobDetails, jobDetail), TRUE)
              
              if(class(jobDetails) == 'try-error')
                next
            }#end for(i in )
            
            message(Sys.time(), ": Combining company")
            #cbind jobnames with details
            jobAds <- try(dplyr::bind_cols(jobAds, jobDetails), TRUE)
            
            if(class(jobAds) == "try-error")
              next()
            
            if(nrow(jobAds) > 0)
            {
              #add a rank column which will be populated later
              jobAds <- cbind.data.frame(jobAds, "Rank"=NA)
              
              #write to search results csv 
              message(Sys.time(), ": Writing results to db")
              #readr::write_csv(searchResults, "jobsearchresults.csv")
              
              jobAds <- jobAds[,c("jobTitle", "jobUrl", "companyName", "jobDescriptionShort", "jobDescriptionLong")]
              
              names(jobAds) <- c("JobTitle", "JobUrl", "CompanyName", "DescriptionShort", "DescriptionLong")
              
              message(Sys.time(), ": Adding timestamp, clientId, search txt")
              
              jobAds <- dplyr::bind_cols("SearchTime"=rep(Sys.time(), nrow(jobAds)),
                                         "ClientId"=rep(clientId, nrow(jobAds)),
                                         "SiteId"=rep(siteId, nrow(jobAds)),
                                         "SearchTxt"=rep(searchTxt, nrow(jobAds)),
                                         jobAds)
              
              message("jobAds: ")
              print(jobAds)
              
              #write to search results csv 
              message(Sys.time(), ": Writing results to db")
              res <- putClientJobSearchResults(jobAds)
            }

            jobDetails <- data.frame()
            jobAds <- data.frame()
            
            remDr$client$setImplicitWaitTimeout(3000)
            
            #next page
            message(Sys.time(), ": Locating next page link")
            nextPageLink <- suppressMessages(try(remDr$client$findElement('css', 'nav.paginacao a.arrow.next'),TRUE))
            
            remDr$client$setImplicitWaitTimeout(20000)
            
            message(Sys.time(), ": Sleeping 1")
            Sys.sleep(1)
            
            if(class(nextPageLink) == 'try-error')
            {
              message(Sys.time(), ": Last page detected. Sleeping 1")
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
          } #repeat until there's no next page link
          
          #finished search for one criterion
          #mark user search with date/time to show complete
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
        res <- try(remDr$client$close(),TRUE)
      
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