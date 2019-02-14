library(RMariaDB)
source("utils.R")

forceOverwriteDB <- FALSE

tryCatch({
  #Connect to mysql
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser)
  
  #we were able to connect to mysql
  #create database using RMariaDB in R if it
  #does not exist
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE DATABASE IF NOT EXISTS ", dbName, " CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  #disconnect and connect to the created db
  RMariaDB::dbDisconnect(dbCon)
  
  # reconnecting to database we just created using following command in R :
  dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=dbUser, db = dbName)
  
  if(forceOverwriteDB)
  {
    RMariaDB::dbExecute(dbCon, paste0("DROP DATABASE ", dbName))
    RMariaDB::dbExecute(dbCon, paste0("CREATE DATABASE IF NOT EXISTS ", dbName, " CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
    RMariaDB::dbExecute(dbCon, paste0("USE ", dbName))
  }
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ", tblUsers, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           FirstName VARCHAR (20),
                                           LastName VARCHAR (20),
                                           Username VARCHAR (20) NOT NULL,
                                           Password TEXT NOT NULL,
                                           Email VARCHAR (50),
                                           Role VARCHAR (20))
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  #create clients table with 2 cols property & value
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ",  tblClients, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           Name VARCHAR (100) NOT NULL UNIQUE)
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ", tblClientDetails, " (
                                           ClientId INT NOT NULL,
                                           Property VARCHAR (255),
                                           Value TEXT,
                                           FOREIGN KEY (ClientId)
                                           REFERENCES Clients(Id)
                                           ON DELETE CASCADE)
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ", tblSites, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           Name VARCHAR (255) NOT NULL,
                                           Enabled BOOLEAN NOT NULL DEFAULT TRUE)
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  #create searchresults table
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ", tblJobSearchResults, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           SearchTime TIMESTAMP,
                                           SiteId INT NOT NULL,
                                           ClientId INT NOT NULL,
                                           SearchTxt VARCHAR (255) NOT NULL,
                                           JobTitle VARCHAR (255),
                                           JobUrl VARCHAR (255),
                                           CompanyName VARCHAR (255),
                                           DescriptionShort VARCHAR (255),
                                           DescriptionLong VARCHAR(10000),
                                           Score INT,
                                           Selected boolean,
                                           Rank INT, 
                                           FOREIGN KEY (SiteId) REFERENCES Sites(Id),
                                           FOREIGN KEY (ClientId) REFERENCES Clients(Id))
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))

#DISABLED: was needed to get a unique row by hashing all columns but not in use. JobUrl should be used
#  as the primary key
#   res <- RMariaDB::dbExecute(dbCon, paste0("ALTER TABLE ", tblJobSearchResults, " ADD COLUMN IF NOT EXISTS
# ClientJobUrlHash binary(32)
# GENERATED ALWAYS AS (
# unhex(sha2(concat(ClientId,'-',JobUrl), 256))) 
# STORED NOT NULL,
# ADD UNIQUE INDEX
# unique_constraint (ClientJobUrlHash);"))
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ", tblSearchQueue, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           UserId INT NOT NULL, 
                                           SiteId INT NOT NULL, 
                                           ClientId INT NOT NULL,
                                           UNIQUE (UserId, SiteId, ClientId),
                                           FOREIGN KEY (UserId) REFERENCES Users(Id) ON DELETE CASCADE,
                                           FOREIGN KEY (SiteId) REFERENCES Sites(Id) ON DELETE CASCADE,
                                           FOREIGN KEY (ClientId) REFERENCES Clients(Id) ON DELETE CASCADE)
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ", tblSearchProcessing, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           UserId INT NOT NULL, 
                                           SiteId INT NOT NULL, 
                                           ClientId INT NOT NULL,
                                           UNIQUE (UserId, SiteId, ClientId),
                                           FOREIGN KEY (UserId) REFERENCES Users(Id) ON DELETE CASCADE,
                                           FOREIGN KEY (SiteId) REFERENCES Sites(Id) ON DELETE CASCADE,
                                           FOREIGN KEY (ClientId) REFERENCES Clients(Id) ON DELETE CASCADE)
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ", tblSearchCommands, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           UserId INT NOT NULL, 
                                           SiteId INT NOT NULL,
                                           CommandTxt VARCHAR (255) NOT NULL,
                                           FOREIGN KEY (UserId) REFERENCES Users(Id) ON DELETE CASCADE,
                                           FOREIGN KEY (SiteId) REFERENCES Sites(Id) ON DELETE CASCADE)
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ",  tblServerStatus, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           UserId INT NOT NULL,
                                           SiteId INT NOT NULL,
                                           StatusTime TIMESTAMP,
                                           Status VARCHAR (20),
                                           UNIQUE (UserId, SiteId),
                                           FOREIGN KEY (UserId) REFERENCES Users(Id) ON DELETE CASCADE,
                                           FOREIGN KEY (SiteId) REFERENCES Sites(Id) ON DELETE CASCADE)
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ",  tblUserNotifications, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           UserId INT NOT NULL,
                                           NotificationType ENUM ('default', 'message', 'warning', 'error'),
                                           Content VARCHAR (100),
                                           FOREIGN KEY (UserId) REFERENCES Users(Id) ON DELETE CASCADE)
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ", tblCurrLogins, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           Time TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
                                           UserId INT NOT NULL,
                                           SessionId TEXT NOT NULL)
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  res <- RMariaDB::dbExecute(dbCon, paste0("CREATE TABLE ", tblLogs, " (
                                           Id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                                           Time TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
                                           LogText TEXT)
                                           CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"))
  
  res <- RMariaDB::dbExecute(dbCon, paste0("INSERT INTO ", tblSites, " VALUES (0, 'catho', TRUE), (0,'indeed', TRUE);"))
  
  RMariaDB::dbDisconnect(dbCon)
  
  return(TRUE)
}, error=function(err)
{
  message(err)
  
  RMariaDB::dbDisconnect(dbCon)
  
  return(FALSE)
})

source("populate_database.R")