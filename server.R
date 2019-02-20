if(!require(shiny))
{
  install.packages("shiny")
}

if(!require(shinydashboard))
{
  install.packages("shinydashboard")
}

if(!require(DT))
{
  install.packages("DT")
}

if(!require(RMariaDB))
{
  library(RMariaDB)
}

if(!require(shinyjs))
{
  library(shinyjs)
}

source("processpdf.R")
source("utils.R")
source("rankjobs.R")

#plan(multiprocess, workers=2)
options(stringsAsFactors = F)
shinyjs::useShinyjs()

shinyServer(function(input, output, session)
{
  numJobs <- NULL

  values <- reactiveValues(newUserUploaded=0, notificationsChanged=0, servicePids=NULL)
  
  #AUTHENTICATION
  login <- reactiveValues(Login = FALSE,
                          Id = NULL,
                          FirstName = NULL,
                          LastName = NULL,
                          UserName = NULL,
                          Role = NULL,
                          Email = NULL)
 
  getNotificationMenu <- reactive({
    
    req(login$Login)
    
    invalidateLater(10000)
    
    values$notificationsChanged
    
    dbNotifications <- getUserNotifications(login$Id)
    
    if(nrow(dbNotifications) == 0)
      return(dropdownMenu(type = "notifications", .list = list()))
    
    notifications <- apply(dbNotifications, 1, function(row) {
      notification <- notificationItem(
        text = row[["Content"]],
        icon = shiny::icon(row[["NotificationType"]]),
        status = "warning",
        href ="#shiny-tab-SearchResults" 
      )
      
      notification$children[[1]] <- shiny::a(href="#shiny-tab-SearchResults","onclick"=paste0("clickFunction('",row[["Id"]],"'); return false;"), list(notification$children[[1]]$children))
      notification
    })
    
    dropdownMenu(type = "notifications", badgeStatus = 'warning', .list = notifications)
  })
  
  output$notificationMenu <- renderMenu({

    getNotificationMenu()
  })
  
  observeEvent(input$linkClicked,{
    print(input$linkClicked)
    
    notification <- getUserNotifications(login$Id, as.numeric(input$linkClicked))
    
    delUserNotification(notification$Id)
    
    values$notificationsChanged <- values$notificationsChanged + 1
    
    clientId <- gsub(":","",stringr::str_extract(notification$Content, "\\d+:"))
    
    updateTabItems(session, inputId = "topTabs", selected = "Search Results")
    
    updateSelectInput(session, inputId = "client", selected = clientId)
    
    output$notificationMenu <- renderMenu({getNotificationMenu()})
  })
  
  output$uiBtnSearch <- shiny::renderUI({
    req(login$Login)
    
    if(is.null(input$client) || input$client == "")
      clientIsInQueue <- FALSE
    else
      clientIsInQueue <- isClientInQueue(as.numeric(input$client))
    
    shiny::actionButton(inputId = "btnSearch", label = ifelse(clientIsInQueue, "Cancel", "Search"))
  })
  
  clients <- reactive({
    #dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), host=dbHost, user=dbUser, db=dbName)
    
    values$newUserUploaded
    
    clientList <- getAllClients()
    
    #dbDisconnect(dbCon)
    
    clientList
  })
  
  clientNamesReactive <- reactive({
    clientList <- clients()

    clientNames <- setNames(clientList$Id, clientList$Name)
  })
  
  clientDetails <- reactive({
    getClientDetails(as.numeric(input$client))
  })
  
  clientInterests <- reactive({
    getClientInterests(as.numeric(input$client))
  })
  
  searchResults <- shiny::reactive({
      res <- getClientJobSearchResults(as.numeric(input$client))
      
      res
  })
  
  output$clientSelect <- shiny::renderUI({
    req(login$Login)
    
    clientNames <- clientNamesReactive()
    
    shiny::selectInput(inputId = "client", label = "Client", choices = clientNames)
  })
  
  output$interestsResults <- shiny::renderUI({
    req(login$Login)
    
    clntInterests <- clientInterests()
    
    lngthInterest <- max(sapply(clntInterests, nchar))
      
    #assume 10px width per char
    shiny::selectInput(inputId = "interestsResults",
                       label = "Interest",
                       choices = clntInterests,
                       width = lngthInterest*7)
  })
  
  ################################  adminUsersTab ################################
  
  output$clientDetailsDT <-  DT::renderDataTable({
    req(login$Login)
    
    values$newUserUploaded
    
    clntDetails <- clientDetails()
    
    clntDetails
  })
  
  ################################  reactive Values ################################
  
  reactVals <- reactiveValues(clntResultsDF = NULL, checkBoxes = NULL)
  
  ################################  observe results dataframe ################################
  
  observe({
    req(login$Login)
    
    maxChars <- 250
    
    clntResults <- searchResults()
    
    clntResults <- mutate(clntResults,
                          JobUrl = paste0("<a href='", JobUrl,"' target='_blank'>", JobTitle,"</a>"), 
                          DescriptionShort = ifelse(nchar(DescriptionShort) > maxChars,
                                                    paste0(substr(DescriptionShort, 1, maxChars), " ..."),
                                                    DescriptionShort),
                          DescriptionLong = ifelse(nchar(DescriptionLong) > maxChars,
                                                   paste0(substr(DescriptionLong, 1, maxChars), " ..."),
                                                   DescriptionLong))
    
    if(!is.null(input$filterOn) && input$filterOn)
     if(!is.null(input$interestsResults))
       clntResults <- clntResults[which(clntResults$SearchTxt == input$interestsResults), ]
    
    #remove first 5 cols after id leaving only id, short & long description, score, selected and rank
    clntResults <- clntResults[order(clntResults$Rank), ]
    
    clntResults <- dplyr::select(clntResults, Id, SearchTime, JobUrl, CompanyName, DescriptionShort, DescriptionLong, Rank, Score, Selected)

    if(nrow(clntResults) == 0)
    {
      #update the underlying dataframe of the datatable with the empty df
      reactVals$clntResultsDF <- clntResults
      
      #stop processing
      return()
    }
    
    #embed row number and jobid to identify the row in page(in js)/record in db later
    #zero-index for access in javascript
    jobSelected <- paste0('<input type="checkbox" id="jobid_', 0:(nrow(clntResults)-1), "_", clntResults$Id, '"', ifelse(clntResults$Selected==1, ' checked', ''), '>')
    
    #clntResults[["Actions"]]<-
    #  paste0('
    #         <div class="btn-group" role="group" aria-label="Basic example">
    #         <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(clntResults),'>Delete</button>
    #         <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(clntResults),'>Modify</button></div>
    #         ')
    
    #remove the Selected col from the DT and replace with the jobSelected checkboxes
    clntResults <- clntResults[,-which(names(clntResults)=="Selected")]
    
    clntResults[["Selected"]] <- jobSelected
    
    reactVals$clntResultsDF <- clntResults
  })
  
  ################################  render results datatable ################################
  
  output$resultsDT <- DT::renderDataTable({
    req(login$Login)
    
    df <- reactVals$clntResultsDF 

    datatable(df,
              rownames=F,
              escape=F,
              editable=T,
              selection='single',
              extensions=list(c('Scroller','Responsive')),
              options=list(dom='Bfrtip',
                           scrollY=400,
                           autoWidth=T,
                           searchHighlight = TRUE,
                           columnDefs = list(list(visible=FALSE, targets=c(1)),
                                             list(className='dt-center', targets=c(8)))),
              callback=JS(
                'table.on("click.dt","tr td input:checkbox", function () {
                var dataScore = $(this).parent().siblings().eq(6).text();
                var isChecked = $(this).is(":checked");
                
                //alert("score=<" + dataScore + "> checked=<" + isChecked + ">");
                
                //only trigger if we are checking the checkbox though it should not happen that it is 
                //selected and no score exists
                if(dataScore == "" && $(this).is(":checked")){
                alert("Please set a score before selecting the job");
                
                //return the checkbox to the initial value
                this.checked = !$(this).is(":checked");
                return;
                }
                
                Shiny.onInputChange("chkboxId", this.id);
                Shiny.onInputChange("chkboxClick", Math.random());
                Shiny.onInputChange("chkboxChecked", $(this).is(":checked"));
  })')) %>% formatStyle('Score',  backgroundColor = '#ffda6b')

  })
  
  # Here is the code from Part 4 of https://github.com/rstudio/DT/pull/480:
  proxy <- dataTableProxy('resultsDT')
  
  ################################  observe edit scores ################################
  
  observeEvent(input$resultsDT_cell_edit, {
    scoreColIdx <- 8
    
    info <- input$resultsDT_cell_edit
    
    i <- info$row
    j <- info$col + 1  # column index offset by 1
    v <- as.integer(info$value)
    
    if(j != scoreColIdx)
    {
      helpTextMsg <- "Can only change scores"
      
      helpText(helpTextMsg) %>%
        div(style = "margin-bottom: 15px", .) %>%
        showOKModal("searchCancel", .)
      
      #return the datatable value to prev value
      replaceData(proxy, reactVals$clntResultsDF, resetPaging=FALSE, rownames=FALSE)
      
      return()
    }
    
    if(is.na(v) || !is.integer(v) || v < 0 || v > 10)
    {
      helpTextMsg <- "Only integer scores (0 - 10) allowed"
      
      helpText(helpTextMsg) %>%
        div(style = "margin-bottom: 15px", .) %>%
        showOKModal("searchCancel", .)
      
      #return the datatable value to prev value
      #replaceData(proxy, reactVals$clntResultsDF, resetPaging=FALSE, rownames=FALSE)
      
      return()
    }
    
    #clntResults[i, j] <<- DT::coerceValue(v, clntResults[i, j])
    
    tempDF <- reactVals$clntResultsDF
    
    tempDF[i, j] <- DT::coerceValue(v, tempDF[i, j])
    
    replaceData(proxy, tempDF, resetPaging=FALSE, rownames=FALSE)
    
    jobId <- reactVals$clntResultsDF$Id[i]
    
    #write to DB
    if(j == scoreColIdx)
      updateClientJobScore(jobId = jobId, newScore = as.numeric(v))
  })
  
  ################################  update job selected ################################
  
  observeEvent(input$chkboxClick,
   {
     paste0("here")
     
     jobId <- as.numeric(gsub("jobid_.*_", "", input$chkboxId))
     
     #write to DB
     updateClientJobSelection(jobId = jobId, selected = input$chkboxChecked)
   }
  )
  
  getClientsInQueueReactive <- reactive({
    if(is.null(input$client) || input$client == "")
      return()
      
    getClientsInUserQueue(as.numeric(login$Id))
  })
  
  getClientsInProcessingReactive <- reactive({
    if(is.null(input$client))
      return()
    
    getClientsInProcessing()
  })

  ################################  observe click search ################################
  
  observeEvent(input$btnSearch, {
    
    if(isClientInQueue(as.numeric(input$client)))
    {
      if(isClientInUserQueue(as.numeric(login$Id), as.numeric(input$client)))
      {
        helpTextMsg <- "Are you sure you want to cancel user search?"
        
        helpText(helpTextMsg) %>%
          div(style = "margin-bottom: 15px", .) %>%
          showConfirmModal("searchCancel", .)
      }
      else
      {
        helpTextMsg <- "This search was started by another user. You cannot cancel it"
        
        helpText(helpTextMsg) %>%
          div(style = "margin-bottom: 15px", .) %>%
          showOKModal("searchCancel", .)
      }
      
      return()
    }

    #if not in queue put in queue and change button text
    putClientInAllQueues(as.numeric(login$Id), as.numeric(input$client))
    
    shiny::updateActionButton(session = session, inputId = "btnSearch", label = "Cancel")
    
    startUserServices(login$Id)
  })
  
  observeEvent(input$searchCancel_ok, {
    shiny::removeModal()
    
    #when ok is clicked only do anything if the user is in user queue
    if(isClientInUserQueue(as.numeric(login$Id), as.numeric(input$client)))
    {
      removeClientFromUserQueue(as.numeric(login$Id), as.numeric(input$client))
      
      shiny::updateActionButton(session = session, inputId = "btnSearch", label = "Search")
      
      return()
    }
  })
  
  observeEvent(input$searchCancel_cancel, {
    #any cancel just remove modal
    shiny::removeModal()
  })
  
  ################################  observe topTabs ################################
  
  observeEvent(eventExpr = input$topTabs,
               handlerExpr = {

                 if(is.null(input$client))
                   return()
                 
                 clientsInQueue <- getClientsInQueueReactive()
                 
                 if(is.null(clientsInQueue) || nrow(clientsInQueue) == 0)
                   return()
                 
                 if(isClientInUserQueue(as.numeric(login$Id), as.numeric(input$client)))
                   shiny::updateActionButton(session = session, inputId = "btnSearch", label = "Cancel")
               })
  
  #plan(sequential)
  
  ################################  display server status ################################
  
  output$displayServerStatus <- shiny::renderText({
    req(login$Login)

  })

  ################################  client search Status ################################
  
  output$displayStatus <- shiny::renderText({
    req(login$Login)
    
    invalidateLater(1000)
    
    if(is.null(input$client) || input$client == "")
      return()

    clientsInQueue <- getClientsInUserQueue(as.numeric(login$Id))$Id
    clientsInProcessing <- getClientsInProcessing()$ClientId
    
    if(is.null(input$client) || input$client == "")
    {
      clientIsInQueue <- FALSE
      clientIsInProcessing <- FALSE
    }
    else
    {
      clientIsInQueue <- isClientInQueue(as.numeric(input$client))
      clientIsInProcessing <- isClientInProcessing(as.numeric(input$client))
    }
    
    #side effect update button text
    if(clientIsInQueue)
      shiny::updateActionButton(session = session, inputId = "btnSearch", label = "Cancel")
    else
      shiny::updateActionButton(session = session, inputId = "btnSearch", label = "Search")
    
    statusMsg <- "STATUS: "
    
    if(clientIsInQueue && !clientIsInProcessing)
      statusMsg <- paste0(statusMsg, "Queued")
    else if(clientIsInQueue && clientIsInProcessing)
      statusMsg <- paste0(statusMsg, "Searching")
    else if(!clientIsInQueue && clientIsInProcessing)
      statusMsg <- paste0(statusMsg, "Cancelling")
    else
      statusMsg <- paste0(statusMsg, "Idle")
  
    statusMsg
  })
  
  ################################  uploadPDF ################################
  
  output$fileUpload <- shiny::renderUI({
    req(login$Login)
    
    shiny::fileInput(inputId = "uploadPDF",
                     label = "Upload New Client",
                     multiple = T,placeholder = "clientform.pdf",
                     accept = c("application/pdf",".pdf"))
  })
  
  observeEvent(input$uploadPDF,
               {
                 inFiles <- input$uploadPDF
                 
                 if(is.null(inFiles))
                   return()
                 
                 for(inFile in inFiles$datapath)
                 {
                   extractPDFData(inFile)
                 }
                 
                 #updateSelectInput(session, inputId = "clientSelect", choices = clientNamesReactive())
                 values$newUserUploaded <- values$newUserUploaded + 1
               })
  
  ################################  serverLogs ################################
  
  output$serverLogs <- renderText({
    req(login$Login)
    
      # Read the text, and make it a consistent number of lines so
      # that the output box doesn't grow in height.
      txt <- serverLogContents()
      
      if(length(txt) > 0)
        length(txt) <- 14
      else
        txt <- "Empty Log"
      
      txt[is.na(text)] <- ""
      
      paste(txt, collapse = '\n')
    })
  
  getCathoLogFilename <- reactive({
    req(login$Login)
    paste0("searchcatho_", login$Id, "_.log")
    })
  
  serverLogContents <- reactiveFileReader(500,
                                session = session,
                                filePath = "searchcatho_3_.log",
                                readFunc = readLines)
                                # function(logFilename)
                                # {
                                #   req(login$Login)
                                #   #clientsFilename <- "clients.csv"
                                #   
                                #   if(!file.exists(logFilename))
                                #     return(NULL)
                                #   
                                #   logContents <- read.csv(logFilename, header = F, encoding = 'UTF-8')
                                # 
                                #   paste0("nrows",nrow(logContents))
                                #   
                                #   names(logContents) <- "Server Log"
                                #   
                                #   return(logContents)
                                # })
  
  ################################  login ################################
  
  # initially display the login modal
  observe({
    composeLoginModal()
  })
  
  output$btnLogout <- shiny::renderUI({
    req(login$Login)
    
    shiny::actionButton("logout", "Logout")
  })
  
  observeEvent(input$logout_ok, {
    shiny::removeModal()
    
    delCurrLoginRecord(isolate(session), isolate(session$token), isolate(login$Id))
    
    # clear the values when logout is confirmed
    login$Login <- FALSE
    login$Id  <- NULL
    login$FirstName  <- NULL
    login$LastName  <- NULL
    login$UserName  <- NULL
    login$Email <- NULL
    
    composeLoginModal(
      div(
        id    = "modal-logout-message"
        , style = "margin-bottom: 10px"
        , span(class = "text-muted", "Successfully Logged Out")
      ) #/ modal-logout-message
    ) #/ composeLoginModal
  })
  
  # once a login is attempted, do some checks
  observeEvent(input$login_button, {
    
    # remove the modal while we check
    shiny::removeModal()
    
    # query the database for that user will return NAs if not populated
    stored <- sendUserGetQuery(input$login_user)
    
    # if any are NA then the record doesn't exist or the record is corrupted
    user_invalid <- stored %>% sapply(is.na) %>% any
    
    # try to login, will automatically handle NULL-y objects
    login$Login <- validateLogin(stored$Password, input$login_passwd)
    
    # if the login is not successful, toss up another login modal, 
    # this time with a message
    if (isTRUE(user_invalid) | login$Login == FALSE) {
      composeLoginModal(
        div(
          id    = "modal-login-message"
          , style = "margin-bottom: 10px"
          , span(style = "color: red; font-weight:bold", "Incorrect Login/Password")
        ) #/ modal-login-message
      ) #/ composeLoginModal
    } else {
      # if the login is successful, populate the known values
      login$Id  <- stored$Id
      login$FirstName  <- stored$FirstName
      login$LastName  <- stored$LastName
      login$UserName  <- stored$UserName
      login$Role  <- stored$Role
      login$Email <- stored$Email
      
      rm(stored)
      
      addCurrLoginRecord(session, session$token, login$Id)
      #startUserServices(login$Id)
      
    } #/ fi
  }) #/ login_button Observer
  
  # close database conncention on exit
  session$onSessionEnded(function() {
    delCurrLoginRecord(isolate(session), isolate(session$token), isolate(login$Id))
    dbDisconnect(dbCon)
  })
  
  observeEvent(input$logout, {
    helpText("Are you sure you want to Logout? Any unsaved work will be lost!") %>%
      div(style = "margin-bottom: 15px", .) %>%
      showConfirmModal("logout", .)
  })
  
  observeEvent(input$logout_cancel, {
    shiny::removeModal()
  })
  
  # output$adminButton <- shiny::renderUI({
  #   req(login$Login & login$Role == "Admin")
  # 
  #   shiny::actionButton(inputId = "adminButton", label = "ADMIN")
  # })
  # 
  # observeEvent(input$adminButton,
  #              {
  #                appendTab("topTabs", adminUsersTab)
  #                appendTab("topTabs", adminLogsTab)
  #                appendTab("topTabs", adminServicesTab)
  #                appendTab("topTabs", adminServerTab)
  #              }
  #         )
  
  ################################  adminUsersTab ################################
  
  output$user <- renderUI({
    req(login$Login & login$Role=="Admin")
    
    users <- getUsers()
    userNames <- paste(users$FirstName, users$LastName, sep=" ")
    shiny::selectInput("user", label = "User", choices = c("All", userNames), selected = NULL)
    })
  
  output$adminUsers <- DT::renderDataTable({
    req(login$Login & login$Role=="Admin")
    
    users <- getUsers()
    
    if(!is.null(input$user) && input$user != "All")
    {
      splits <- unlist(strsplit(input$user, " "))
      fName <- splits[1]
      lName <- splits[2]
      users <- users[which(users$FirstName == fName & users$LastName == lName),]
    }
    
    users
  })
  
  ################################  adminClientsTab ################################
  
  output$admClient <- renderUI({
    req(login$Login & login$Role=="Admin")
    
    shiny::selectInput("admClient", label = "Client", choices = c("All", getAllClients()$Name, selected = NULL))
    })
  
  output$adminClients <- renderDataTable({
    req(login$Login & login$Role=="Admin")
    
    clients <- getAllClients()
    
    if(!is.null(input$admClient) && input$admClient != "All")
    {
      clients <- clients[which(clients$Name == input$admClient),]
    }
    
    clients
  })
  
  output$adminClientDetails <- renderDataTable({
    req(login$Login & login$Role=="Admin")
    
    clientDets <- getClientDetails()
    
    if(!is.null(input$admClient) && input$admClient != "All")
    {
      clientDets <- clientDets[which(clientDets$Name == input$admClient),]
    }
    
    clientDets
  })
  
  ################################  adminServicesTab ################################

  runningServices <- reactivePoll(intervalMillis = 1000,
                                  session = session,
                                  checkFunc = function(){
                                    services <- getRunningUserServices()
                                    servicesChanged <- setequal(services$pId, values$servicePids)
                                    return(servicesChanged)
                                  },
                                  valueFunc = function(){
                                    services <- getRunningUserServices()
                                    
                                    values$servicePids <- services$pId
                                    
                                    if(!is.null(input$admServiceUser) && input$admServiceUser != "All")
                                    {
                                      services <- services[which(services$userId == input$admServiceUser),]
                                    }
                                    
                                    return(services)
                                    })
  
  output$admServiceUser <- renderUI({
    req(login$Login & login$Role=="Admin")
    
    users <- getUsers()
    userNames <- paste(users$FirstName, users$LastName, sep=" ")
    shiny::selectInput("admServiceUser", label = "User", choices = c("All", userNames), selected = NULL)
  })
  
  output$btnAdmExitAllServices <- shiny::renderUI({
    req(login$Login)
    
    shiny::actionButton(inputId = "btnAdmExitAllServices", "Exit all Services!")
  })
  
  observeEvent(input$btnAdmExitAllServices,
               {
                 exitAllUserServices()
               })
  
  output$btnAdmKillAllServices <- shiny::renderUI({
    req(login$Login)
    
    shiny::actionButton(inputId = "btnAdmKillAllServices", "!!Kill all Services!!")
  })
  
  observeEvent(input$btnAdmKillAllServices,{
                 killAllUserServices(TRUE)
                 clearAllUserProcessing(TRUE)
               })
  
  output$adminServices <- renderDataTable({
    req(login$Login & login$Role=="Admin")
    
    services <- runningServices()
    
    return(services)
  })
  
  ################################  topTabs ################################
  
  output$topTabs <- renderUI({
    
    req(login$Login)
    
    shiny::tabsetPanel(id = "topTabs",
                       shiny::tabPanel("Details",
                                       DT::dataTableOutput("clientDetailsDT")
                       ),
                       
                       shiny::tabPanel("Search Results",
                                       mainPanel(fluidRow(column(8, shiny::uiOutput(outputId = "interestsResults")),
                                       column(4, shiny::tags$br(),shiny::checkboxInput(inputId = "filterOn", label = "Filter by interest", value = FALSE)))),
                                       DT::dataTableOutput("resultsDT")
                        ),

                       shiny::tabPanel("Users",
                                       uiOutput("user"),
                                       DT::dataTableOutput("adminUsers")
                       ), #list user services,check state, send exit, force kill,

                       shiny::tabPanel("Client Administration",
                                       uiOutput("admClient"),
                                       dataTableOutput("adminClients"),
                                       dataTableOutput("adminClientDetails")

                       ), #uname,pass,role,logged in,work logs?

                       shiny::tabPanel("Server Logs",
                                       dataTableOutput(outputId = "serverLogs")
                       ),

                       shiny::tabPanel("Service Administration",
                                       uiOutput("admServiceUser"),
                                       uiOutput("btnAdmExitAllServices"),
                                       uiOutput("btnAdmKillAllServices"),
                                       dataTableOutput("adminServices")
                       ), #uname,pass,role,logged in,work logs?

                       shiny::tabPanel("Server Control", #shiny, selenium, docker, controls
                                       shiny::uiOutput(outputId = "serverControl")
                       )
     )
  })
})
