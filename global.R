
# Libraries ---------------------------------------------------------------

library("shiny")
library("RMariaDB")
library("sodium")
library("dtplyr")
library("dplyr")

# Global Variables --------------------------------------------------------

#options(shiny.trace=TRUE)

app_name <- "User Auth"
# dbName <- "HRAppDB"
# dbUser <- "root"

source("init.R")
source("utils.R")

support_contact <- "Mac@me.com"

forgot_password_message <- paste0(
  paste0("mailto:", support_contact, "?")
  , paste0("subject=", app_name, " - Forgot Password&")
  , "body=Hello,%0D%0A%0D%0AI have forgotten my password for this app, would you mind resetting the password for the user associated with this email address?%0D%0A%0D%0AThank you!"
) %>%
  URLencode

request_login_message <- paste0(
  paste0("mailto:", support_contact, "?"),
  paste0("subject=", app_name, " - Access Request&"),
  "body=Hello,%0D%0A%0D%0AI would like access to this app, would you mind initializing a username and password for this email address?%0D%0A%0D%0AThank you!"
) %>%
  URLencode

# Global Functions --------------------------------------------------------

composeUserGetQuery <- function(userName) 
  # convenience function to cmopose the query given the table and user
{
  sprintf('SELECT Id, FirstName, LastName, UserName, Password, Email, Role FROM Users WHERE UserName = "%s"', userName)
}

sendUserGetQuery <- function(userName) 
  # convenience function to compose a query, retreive the results and if 
  # necessary, convert from null atomic vector to a NA value
{
  dbCon <- dbConnect(MariaDB(), user = dbUser, dbname = dbName)
  
  query <- composeUserGetQuery(userName) %>%
    dbSendQuery(dbCon, .)
  
  response <- dbFetch(query) %>% 
    as.list %>%
    lapply(., function(x) ifelse(length(x) == 0L, NA_character_, x))
  
  dbClearResult(query)
  dbDisconnect(dbCon)
  
  return(response)
}

validateLogin <- function(stored, input) 
  # check the input against the stored value, returning a boolean and handling
  # the pesky NA values indicating a corrupted/non-existant login
{
  if (is.na(stored)) {
    return(FALSE)
  } else {
    return(sodium::password_verify(stored, input))
  }
}

composeLoginModal <- function(...)
  # generate a modal with the inputs for a login a well as initialization
  # and password recovery links
{
  showModal(
    modalDialog(
      id        = "loginmodal",
      size      = 's',
      easyClose = FALSE,
      div(
        id = "modal-contents",
        textInput('login_user', 'Login'),
        passwordInput('login_passwd', 'Password'),
        div(...),
        actionButton(
          inputId = 'login_button',
          label   = 'Login',
          class   = 'btn action-button btn-success',
          icon    = icon('sign-in')
        ) #/ login-button
      ), #/ modal-contents
      footer = div(id = "modal-footer",
                   a(id = "forgot-login-link",
                     href = forgot_password_message,
                     p("Forgot Password", style = "display: inline;")
                     ),
                   HTML("&bull;"),
                   a(id = "request-login-link",
                     href = request_login_message,
                     p("Request Login", style = "display: inline;")
                     )
      ) #/ modal-footer
    ) #/ modalDialog
  ) #/ showModal
}

showConfirmModal <- function(id, ...) {
  showModal(
    modalDialog(
      id        = sprintf("%s-confirm-modal", id),
      size      = 's',
      easyClose = TRUE,
      div(...),
      div(style = "text-align: right",
          actionButton(sprintf("%s_ok", id), "OK", icon = icon("check"), style = "display: inline;"),
          actionButton(sprintf("%s_cancel", id), "Cancel", icon = icon("times"), style = "display: inline;")
      ),
      footer = NULL
    ) #/ modalDialog
  ) #/ showModal
}

showOKModal <- function(id, ...) {
  showModal(
    modalDialog(
      id        = sprintf("%s-confirm-modal", id),
      size      = 's',
      easyClose = TRUE,
      div(...),
      div(style = "text-align: right",
          actionButton(sprintf("%s_ok", id), "OK", icon = icon("check"), style = "display: inline;")
      ),
      footer = NULL
    ) #/ modalDialog
  ) #/ showModal
}

# initialize a connection for the session -- a server observer will 
# automatically close the connection for us upon exit
dbCon <- dbConnect(MariaDB(), user=dbUser, dbname = dbName)