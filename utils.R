library(RMariaDB)

#userProcessingFile <- "userprocessing.txt"
#userQueueFile <- "userqueue.csv"
#commandFile <- "commandfile.txt"
commandLog <- "commandlog.txt"
logfile <- "hrappsearchlog.log"

tblSearchProcessing <- "SearchProcessing"
tblSearchQueue <- "SearchQueue"
tblSearchCommands <- "SearchCommands"
tblJobSearchResults <- "JobSearchResults"
tblUsers <- "Users"
tblClients <- "Clients"
tblClientDetails <- "ClientDetails"
tblSites <- "Sites"
tblServerStatus <- "ServerStatus"
tblUserNotifications <- "UserNotifications"
tblCurrLogins <- "CurrLogins"
tblLogs <- "AppLogs"

dbName <- "HRAppDB"
dbUser <- "root"

getUsers <- function(userId)
{
  sqlCmd <- paste0("SELECT * FROM ", tblUsers)
  
  if(!missing(userId))
  {
    if(!is.numeric(userId) || length(userId) != 1 || is.na(userId))
      userId <- -1
    
    sqlCmd <- paste0(sqlCmd, " WHERE Id = ", userId)
  }
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, sqlCmd)
  dbDisconnect(dbCon)
  return(res)
}

getAllClients <- function()
{
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblClients))
  dbDisconnect(dbCon)
  return(res)
}

putClientInQueue <- function(userId, siteId, clientId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(siteId) || !is.numeric(siteId) || length(siteId) != 1 || is.na(siteId) ||
     missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    return(FALSE)
  
  df <- cbind.data.frame(userId, clientId, siteId)
 
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName) 
  res <- dbWriteTable(dbCon, tblSearchQueue, df, append=T)
  dbDisconnect(dbCon)
  
  return(TRUE)
}

putClientInAllQueues <- function(userId, clientId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    return(FALSE)
  
  siteIds <- getEnabledSites()$Id
  
  df <- cbind.data.frame("UserId"=userId, "SiteId"=siteIds, "ClientId"=clientId)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName) 
  res <- dbWriteTable(dbCon, tblSearchQueue, df, append=T)
  dbDisconnect(dbCon)
  
  return(TRUE)
}

putClientInProcessing <- function(userId, siteId, clientId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(siteId) || !is.numeric(siteId) || length(siteId) != 1 || is.na(siteId) ||
     missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    return(FALSE)
  
  df <- cbind.data.frame(userId, siteId, clientId)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbWriteTable(dbCon, tblSearchProcessing, df, append=T)
  dbDisconnect(dbCon)
  
  return(TRUE)
}

getClientsInSiteQueue <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(siteId) || !is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
  {
    userId <- -1
    siteId <- -1
  }
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblSearchQueue, " WHERE UserId = ", userId, " AND SiteId = ", siteId))
  dbDisconnect(dbCon)
  return(res)
}

getClientsInUserQueue <- function(userId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    userId <- -1
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblSearchQueue, " WHERE UserId = ", userId))
  dbDisconnect(dbCon)
  return(res)
}

getClientsInQueue <- function(clientId)
{
  if(missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    clientId <- -1
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblSearchQueue, " WHERE ClientId = ", clientId))
  dbDisconnect(dbCon)
  return(res)
}

isClientInUserQueue <- function(userId, clientId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    return(FALSE)
  
  res <- getClientsInUserQueue(userId)
  
  return(clientId %in% res$ClientId)
}

isClientInQueue <- function(clientId)
{
  if(missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    return(FALSE)
  
  res <- getClientsInQueue(clientId)
  
  return(clientId %in% res$ClientId)
}

isClientInUserProcessing <- function(userId, clientId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    return(FALSE)
  
  res <- getClientsInUserProcessing(userId)
  
  return(clientId %in% res$ClientId)
}

isClientInProcessing <- function(clientId)
{
  if(missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    return(FALSE)
  
  res <- getClientsInProcessing()
  
  return(clientId %in% res$ClientId)
}

isClientProcessingCancelled <- function(userId, clientId)
{
  #user is processing if they are both in 
  #the queue and processing files. UI cancels processing by
  #removing user from queue.
  #detect user is cancelled if not in queue

  return(!isClientInUserQueue(userId, clientId) && isClientInUserProcessing(userId, clientId))
}

getClientsInProcessing <- function()
{
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblSearchProcessing))
  dbDisconnect(dbCon)

  return(res)
}

getClientsInUserProcessing <- function(userId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    userId <- -1
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, paste0("SELECT DISTINCT ClientId FROM ", tblSearchProcessing, " WHERE UserId = ", userId))
  dbDisconnect(dbCon)
  return(res)
}

getClientsInSiteProcessing <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(siteId) || !is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
  {
    userId <- -1
    siteId <- -1
  }
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, paste0("SELECT DISTINCT ClientId FROM ", tblSearchProcessing, " WHERE UserId = ", userId, " AND SiteId = ", siteId))
  dbDisconnect(dbCon)
  
  return(res)
}

putUserNotification <- function(userId, notificationType, content)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    return(FALSE)
  
  df <- cbind.data.frame(userId, notificationType, content)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbWriteTable(dbCon, tblUserNotifications, df, append=T)
  dbDisconnect(dbCon)
  
  return(res)
}

getUserNotifications <- function(userId, notificationId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    userId <- -1

  sqlCmd <- paste0("SELECT * FROM ", tblUserNotifications, " WHERE UserId = ", userId)
  
  if(!missing(notificationId))
  {
    if(!is.numeric(notificationId) || length(notificationId) != 1 || is.na(notificationId))
      notificationId <- -1

    sqlCmd <- paste0(sqlCmd, " AND Id = ", notificationId)
  }
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, sqlCmd)
  
  dbDisconnect(dbCon)
  
  return(res)
}

delUserNotification <- function(notificationId)
{
  if(missing(notificationId) || !is.numeric(notificationId) || length(notificationId) != 1 || is.na(notificationId))
    notificationId <- -1
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  dbExecute(dbCon, paste0("DELETE FROM ", tblUserNotifications, " WHERE Id = ", notificationId, ";"))
  
  dbDisconnect(dbCon)
  
  return(TRUE)
}

getAllSites <- function()
{
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblSites))
  dbDisconnect(dbCon)
  
  return(res)
}

getEnabledSites <- function()
{
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblSites, " WHERE Enabled = TRUE"))
  dbDisconnect(dbCon)
  
  return(res)
}

removeClientFromUserQueue <- function(userId, clientId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    return(FALSE)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  dbExecute(dbCon, paste0("DELETE FROM ", tblSearchQueue, " WHERE Userid = ", userId, " AND ClientId = ", clientId))
  dbDisconnect(dbCon)
  
  return(TRUE)
}

removeClientFromProcessing <- function(userId, siteId, clientId)
{
  if(missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 ||is.na(clientId))
    return(FALSE)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  dbExecute(dbCon, paste0("DELETE FROM ", tblSearchProcessing, " WHERE UserId = ", userId, " AND SiteId = ", siteId, " AND ClientId = ", clientId))
  
  dbDisconnect(dbCon)
  
  return(TRUE)
}

clearUserQueue <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    return(FALSE)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  sqlCmd <- paste0("DELETE FROM ", tblSearchQueue, " WHERE UserId = ", userId)
  
  if(!missing(siteId))
  {
    if(!is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
      siteId <- -1
    
    sqlCmd <- paste0(sqlCmd, " AND SiteId = ", siteId)
  }
  
  dbExecute(dbCon, sqlCmd)
  
  dbDisconnect(dbCon) 
  
  return(TRUE)
}

clearUserProcessing <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    return(FALSE)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  sqlCmd <- paste0("DELETE FROM ", tblSearchProcessing, " WHERE UserId = ", userId)
  
  if(!missing(siteId))
  {
    if(!is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
      siteId <- -1
    
    sqlCmd <- paste0(sqlCmd, " AND SiteId = ", siteId)
  }
  
  dbExecute(dbCon, sqlCmd)
  
  dbDisconnect(dbCon)
  
  return(TRUE)
}

clearAllUserProcessing <- function(confirm)
{
  if(missing(confirm) || !is.logical(confirm) || length(confirm) != 1)
    return(FALSE)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  sqlCmd <- paste0("DELETE FROM ", tblSearchProcessing)
  
  if(!missing(siteId))
  {
    if(!is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
      siteId <- -1
    
    sqlCmd <- paste0(sqlCmd, " AND SiteId = ", siteId)
  }
  
  dbExecute(dbCon, sqlCmd)
  
  dbDisconnect(dbCon)
  
  return(TRUE)
}

pushCmdAllUserQueues <- function(userId, cmd)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(cmd) || !is.character(cmd) || length(cmd) != 1 || is.na(cmd))
    return(FALSE)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)

  siteIds <- getEnabledSites()$Id
  
  df <- cbind.data.frame("UserId"=userId, "SiteId"=siteIds, "CommandTxt"=cmd)
  
  res <- dbWriteTable(dbCon, tblSearchCommands, df, append=T)
  
  dbDisconnect(dbCon)
  
  return(TRUE)
}

pushCmdUserQueue <- function(userId, siteId, cmd)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 ||
     missing(userId) || !is.numeric(userId) || length(userId) != 1 ||
     missing(cmd) || !is.character(cmd) || length(cmd) != 1 || is.na(cmd))
    return(FALSE)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)

  df <- cbind.data.frame("UserId"=userId, "SiteId"=siteId, "CommandTxt"=cmd)
  
  res <- dbWriteTable(dbCon, tblSearchCommands, df, append=T)
  
  dbDisconnect(dbCon)
  
  return(TRUE)
}

popCmdQueue <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 ||
     missing(siteId) || !is.numeric(siteId) || length(siteId) != 1)
  {
    userId <- -1
    siteId <- -1
  }
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblSearchCommands, " WHERE UserId = ", userId, " AND SiteId = ", siteId, " LIMIT 1"))
  
  if(nrow(res) > 0)
    dbExecute(dbCon, paste0("DELETE FROM ", tblSearchCommands, " WHERE Id = ", res$Id))
  
  dbDisconnect(dbCon)
  
  return(res)
}

logCmd <- function()
{
  
}

clearUserCommands <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(siteId) || !is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
    return(FALSE)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)

  sqlCmd <- paste0("DELETE FROM ", tblSearchCommands, " WHERE UserId = ", userId)
  
  if(!missing(siteId))
  {
    if(!is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
      siteId <- -1
    
    sqlCmd <- paste0(sqlCmd, " AND SiteId = ", siteId)
  }
  
  dbExecute(dbCon, sqlCmd)
  
  dbDisconnect(dbCon)
  
  return(TRUE)
}

putClientJobSearchResults <- function(jobSearchResults)
{
  if(!is.data.frame(jobSearchResults) || nrow(jobSearchResults) == 0)
    return()
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  existing <- dbGetQuery(dbCon, paste0("SELECT * FROM ",
                                       tblJobSearchResults,
                                       " WHERE ClientId = ",
                                       unique(jobSearchResults$ClientId),
                                       " AND JobUrl IN ('",
                                       paste(jobSearchResults$JobUrl, collapse="','"),
                                       "')"))
  
  jobSearchResults <- jobSearchResults[which(!jobSearchResults$JobUrl %in% existing$JobUrl),]
  
  res <- dbWriteTable(dbCon, tblJobSearchResults, jobSearchResults, append=T)
  
  dbDisconnect(dbCon)
  
  return(res)
}

getClientJobSearchResults <- function(clientId)
{
  if(missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    clientId <- -1
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblJobSearchResults, " WHERE ClientId = ", clientId))
  
  dbDisconnect(dbCon)
  
  return(res)
}

getClientDetails <- function(clientId)
{
  sqlCmd <- paste0("SELECT * FROM ", tblClientDetails)
  
  if(!missing(clientId))
  {
    if(!is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
      clientId <- -1
    
      sqlCmd <- paste0(sqlCmd, " WHERE ClientId = ", clientId)
  }
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)

  clientDetails <- dbGetQuery(dbCon, sqlCmd)
  
  dbDisconnect(dbCon)
    
  clientDetails
}

getClientName <- function(clientId)
{
  if(missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    clientId <- -1
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  clientDetails <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblClients, " WHERE Id = ", clientId))
  
  dbDisconnect(dbCon)
  
  clientDetails$Name
}

getClientInterests <- function(clientId)
{
  if(missing(clientId) || !is.numeric(clientId) || length(clientId) != 1 || is.na(clientId))
    clientId <- -1
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  clientDetails <- dbGetQuery(dbCon, paste0("SELECT Value FROM ", tblClientDetails, " WHERE ClientId = ", clientId, " AND Property LIKE '%interesse%'"))
  
  dbDisconnect(dbCon)
  
  clientDetails$Value
}

getServerStatus <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(siteId) || !is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
  {
    userId <- -1
    siteId <- -1
  }
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName) 
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblServerStatus, " WHERE UserId = ", userId, " AND SiteId = ", siteId))
  dbDisconnect(dbCon)
  
  res
}

setServerStatus <- function(userId, siteId, status)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId) ||
     missing(siteId) || !is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
    return(FALSE)
  
  statusId <- getServerStatus(userId = userId, siteId = siteId)$Id
  
  if(is.null(statusId))
  {
    dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName) 
    res <- dbExecute(dbCon, "UPDATE ", tblServerStatus, " SET Status = ", status, " WHERE Id = ", statusId)
    dbDisconnect(dbCon)
  }
  
  df <- cbind.data.frame(userId, clientId, siteId)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName) 
  res <- dbWriteTable(dbCon, tblServerStatus, df, append=T)
  dbDisconnect(dbCon)
  
  return(TRUE)
}

deleteServerStatus <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 ||
     missing(siteId) || !is.numeric(siteId) || length(siteId) != 1)
    return()
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName) 
  res <- dbGetQuery(dbCon, paste0("DELETE FROM ", tblServerStatus, " WHERE UserId = ", userId, " AND SiteId = ", siteId))
  dbDisconnect(dbCon)
  
  return(res)
}

startUserServices <- function(userId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
     return(FALSE)
     
  sites <- getEnabledSites()
  
  for(i in 1:nrow(sites))
  {
    if(areUserServicesRunning(userId, sites$Id[i]))
      next
    
    if((sites$Name[i] == "catho") && sites$Enabled[i])
    {
      message("Starting catho for userid: ", userId)
      #clear the site command queue
      clearUserCommands(userId, sites$Id[i])
      clearUserProcessing(userId, sites$Id[i])
      
      #start catho
      system(paste0("/bin/R --slave --no-restore -e \"source('searchcatho.R'); searchCatho(", userId, ")\""),
             wait = F)
    }else if((sites$Name[i] == "indeed") && sites$Enabled[i])
    {
      message("Starting indeed for userid: ", userId)
      #clear the site command queue
      clearUserCommands(userId, sites$Id[i])
      clearUserProcessing(userId, sites$Id[i])
      
      #start indeed
      system(paste0("/bin/R --slave --no-restore -e \"source('searchindeed.R'); searchIndeed(", userId, ")\""),
             wait = F)
    }
  }
  
  return(TRUE)
}

getRunningUserServices <- function(userId)
{
  searchUserId <- "\\d+"
    
  if(!missing(userId))
  {
    if(!is.numeric(userId) || length(userId) != 1 || is.na(userId))
      userId <- -1
    
    searchUserId <- userId
  }
     
  procs <- system("ps -ax", intern = T)
  
  siteNames <- getEnabledSites()$Name
  
  procs <- grep(paste0("R --slave --no-restore -e source.*search[",paste0(siteNames, collapse="|"),"].*\\(", searchUserId, "\\)"), procs, value=T)
  
  if(length(procs) == 0)
  {
    nullProcs <- data.frame("userId"=NA, "siteName"=NA, "pId"=NA)
    
    return(nullProcs[FALSE,])
  }
  
  pids <- lapply(procs, function(x) c(gsub("\\(|\\)", "", stringr::str_extract(x, "\\(\\d+\\)")), stringr::str_extract(x, paste0(siteNames, collapse="|")), unlist(strsplit(trimws(x), " "))[1]))
  
  piddata <- setNames(do.call(rbind.data.frame, pids), c("userId", "siteName", "pId"))
  
  return(piddata)
}

areUserServicesRunning <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    return(FALSE)
  
  procs <- getRunningUserServices(userId)
  
  if(!missing(siteId))
  {
    if(!is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
      siteId <- -1
    
    sites <- getEnabledSites()
    siteName <- sites[which(sites$Id == siteId), "Name"]
    procs <- procs[which(procs$siteName %in% siteName),]
  }
  
  return(nrow(procs) > 0)
}

killUserServices <- function(userId, siteName)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    return(FALSE)
  
  procs <- getRunningUserServices(userId)
  
  if(nrow(procs) == 0)
  {
    message("No running services for userId: ", userId)
   
    return(FALSE)
  }
  
  if(!missing(siteName))
    procs <- procs[which(procs$siteName == siteName),]
  
  for(i in 1:nrow(procs))
  {
    userId <- procs[i, "userId"]
    siteName <- procs[i, "siteName"]
    pId <- procs[i, "pId"]
    
    message("Killing userId: ", userId, "| site: ", siteName, " | pid: ", pId)
    system(paste0("kill -9 ", pId))
  }
  
  return(TRUE)
}

killAllUserServices <- function(confirm)
{
  if(missing(confirm) || !is.logical(confirm) || length(confirm) != 1)
    return(FALSE)
  
  procs <- getRunningUserServices()
  
  if(nrow(procs) == 0)
  {
    message("No running services")
    
    return(FALSE)
  }
  
  for(i in 1:nrow(procs))
  {
    userId <- procs[i, "userId"]
    siteName <- procs[i, "siteName"]
    pId <- procs[i, "pId"]

    message("Killing userId: ", userId, "| site: ", siteName, " | pid: ", pId)
    system(paste0("kill -9 ", pId))
  }
  
  return(TRUE)
}

exitUserServices <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    return(FALSE)
  
  sites <- getEnabledSites()
  
  procs <- getRunningUserServices(userId)
  
  if(nrow(procs)==0)
  {
    message("No running services for userId: ", userId)
    return(FALSE)
  }
  
  procs$siteId <- sapply(procs$siteName, function(x) grep(x, sites$Name))
  
  if(!missing(siteId) && is.numeric(siteId) && length(siteId) == 1 && !is.na(siteId))
    procs <- procs[which(procs$siteId %in% siteId),]
  
  for(i in 1:nrow(procs))
  {
    userId <- procs[i, "userId"]
    siteId <- procs[i, "siteId"]
    
    message("Sending exit cmd to service userId: ", userId, " siteId: ", siteId)
    pushCmdUserQueue(userId, siteId, 'exit')
  }
  
  return(TRUE)
}

exitAllUserServices <- function()
{
  sites <- getEnabledSites()
  
  procs <- getRunningUserServices()
  
  if(nrow(procs)==0)
  {
    message("No running services")
    return(FALSE)
  }
  
  procs$siteId <- sapply(procs$siteName, function(x) grep(x, sites$Name))
  
  for(i in 1:nrow(procs))
  {
    userId <- procs[i, "userId"]
    siteId <- procs[i, "siteId"]
    
    message("Sending exit cmd to service userId: ", userId, " siteId: ", siteId)
    pushCmdUserQueue(userId, siteId, 'exit')
  }
  
  return(TRUE)
}

cancelExitUserServices <- function(userId, siteId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    return(FALSE)
  
  sites <- getEnabledSites()
  
  procs <- getRunningUserServices(userId)
  
  siteIds <- sapply(procs$siteName, function(x) grep(x, sites$Name))
  
  if(!missing(siteId) && is.numeric(siteId) && length(siteId) == 1 && !is.na(siteId))
    siteIds <- siteIds[which(siteIds %in% siteId)]
  
  for(siteId in siteIds)
    cancelCmdUserQueue(userId, siteId, 'exit')
  
  return(TRUE)
}

cancelCmdUserQueue <- function(userId, siteId, cmd)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1)
    return(FALSE)
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  sqlCmd <- paste0("DELETE FROM ", tblSearchCommands, " WHERE UserId = ", userId)
  
  if(!missing(siteId))
  {
    if(!is.numeric(siteId) || length(siteId) != 1 || is.na(siteId))
      siteId <- -1
    
    sqlCmd <- paste0(sqlCmd, " AND SiteId = ", siteId)
  }
  
  if(!missing(cmd))
  {
    if(!is.character(cmd) || length(cmd) != 1 || is.na(cmd))
      cmd <- -1
    
    sqlCmd <- paste0(sqlCmd, " AND CommandTxt = ", cmd)
  }
  
  dbExecute(dbCon, sqlCmd)
  
  dbDisconnect(dbCon)
  
  return(TRUE)
}

addCurrLoginRecord <- function(session, sessionId, userId)
{
  if(missing(session) || !all(class(session) == c("ShinySession", "R6")) ||
     #missing(sessionId) || !is.character(sessionId) || length(sessionId) != 1 || is.na(sessionId) ||
     missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    return(FALSE)
  
  cdata <- session$request
  cnames <- names(cdata)
    
  allValues <- paste(lapply(cnames, function(name) {
    if(class(cdata[[name]])=='character' && length(cdata[[name]]==1)) paste(name, cdata[[name]], sep = "=")
  }), collapse=", ")
  
  df <- cbind.data.frame("SessionId"=sessionId, "UserId"=userId)
  
  df1 <- data.frame("LogText"=paste0("User Login, userId=", userId, ", SessionId=", sessionId, ", HTTP Headers, ", allValues))
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName) 
  res <- dbWriteTable(dbCon, tblCurrLogins, df, append=T)
  res <- dbWriteTable(dbCon, tblLogs, df1, append=T)
  dbDisconnect(dbCon)
  
  return(TRUE)
}

delCurrLoginRecord <- function(session, sessionId, userId)
{
  if(missing(userId) || !is.numeric(userId) || length(userId) != 1 || is.na(userId))
    return(FALSE)
  
  sqlDelCurrLogin <- paste0("DELETE FROM ", tblCurrLogins, " WHERE UserId = ", userId, " AND SessionId = '", sessionId, "'")
  
  df1 <- data.frame("LogText"=paste0("User Logout, userId=", userId))
  
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName) 
  res <- dbExecute(dbCon, sqlDelCurrLogin)
  res <- dbWriteTable(dbCon, tblLogs, df1, append=T)
  dbDisconnect(dbCon)
  
  return(TRUE)
}

getLoggedInUsers <- function()
{
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName) 
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblCurrLogins))
  dbDisconnect(dbCon)
  
  res
}