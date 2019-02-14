library(RMariaDB)
library(magrittr)

composeUserAddQuery <- function(id, fname, lname, uname, password, email, role) 
{
  # convenience function to salt and hash a password before composing
  # a query to add it to the database
  sodium::password_store(password) %>%
    sprintf("INSERT INTO Users VALUES('%i', '%s', '%s', '%s', '%s', '%s', '%s')", id, fname, lname, uname, ., email, role)
}

sendUserAddQuery <- function(id, fname, lname, uname, password, email, role) 
{
  # convenience function to call the composer and send the query
  # returning named booleans indicating the success of each row
  composeUserAddQuery(id, fname, lname, uname, password, email, role) %>%
    dbSendQuery(dbCon, .) %>%
    dbClearResult()
}

tryCatch({
  # initialize a connection
  dbCon <- dbConnect(MariaDB(), user=dbUser, db = dbName)
  
  res <- dbGetQuery(dbCon, paste0("SELECT * FROM ", tblUsers))
  
  if(nrow(res) > 0)
  {
    dbDisconnect(dbCon)
    return()
  }
  
  # create the table for the logins
  #dbClearResult(dbSendQuery(dbCon, 'DROP TABLE IF EXISTS Users'))
  #dbClearResult(dbSendQuery(dbCon, 'CREATE TABLE Users (Id TEXT, FirstName TEXT, LastName TEXT, UserName TEXT, Password TEXT, Email TEXT, Role TEXT)'))
  
  # initialize a DT of some dummy logins
  db_logins <- data.table::data.table(
    id = c(0, 0, 0),
    fname = c('fname1', 'fname2', 'fname3'),
    lname = c('lname1', 'lname2', 'lname3'),
    uname = c('user1', 'user2', 'admin'),
    role = c('User', 'User', 'Admin'),
    password = rep("Welcome1", 3)
  )
  db_logins[, email := paste0(uname, "@me.com")]
  
  # perform additions
  success <- db_logins[, mapply(sendUserAddQuery, id, fname, lname, uname, password, email, role)]
  
  # check that all are TRUE
  stopifnot(all(success))
  
  dbDisconnect(dbCon)
  
  return(TRUE)
}, error=function(err)
{
  message(err)
  dbDisconnect(dbCon)
  return(FALSE)
})