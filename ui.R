library(shiny)
library(shinydashboard)
library(DT)

options(stringsAsFactors = F)
#options(shiny.trace=TRUE)
#options(shiny.sanitize.errors = FALSE)

shiny::uiOutput(outputId = "mainPage")

# Define UI for application that draws a histogram
shinydashboard::dashboardPage(
  # Application title
  header <- shinydashboard::dashboardHeader(title="HR Matching",
                                            dropdownMenuOutput('notificationMenu')),
  
  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      
      shiny::uiOutput(outputId = "clientSelect"),

      shiny::uiOutput(outputId = "uiBtnSearch"),
      
      shiny::uiOutput(outputId = "displayStatus"),
      
      shiny::uiOutput(outputId = "fileUpload"),
      
      shiny::uiOutput(outputId = "btnLogout"),
      
      br(),
      
      shiny::uiOutput(outputId = "adminButton")
    )),
  
  body <-   shinydashboard::dashboardBody(

    tags$script(HTML("function clickFunction(link){ 
                       Shiny.onInputChange('linkClicked',link);
    }")),
    
    shiny::uiOutput("topTabs"),
    shiny::uiOutput("adminTabs")
  )
)
