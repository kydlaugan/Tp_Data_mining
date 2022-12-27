#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
dat <- read.table("C:/Users/Robinson/Documents/3115_Projet/SouthGermanCredit.asc", header=TRUE) 
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = " INF 3115 PROJET "),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse descriptive", tabName = "Analyse descriptive", icon = icon("dashboard"),
               menuSubItem("Histogrammes des attributs",tabName = "hist"),
               menuSubItem("Summary",tabName = "sum")
               ),
      menuItem("Widgets", tabName = "widgets" , icon = icon("car"))
    )
  ),
  dashboardBody( 
    tabItems(
      tabItem("Analyse descriptive",h1("Analyse statistique")),
      tabItem("hist",h1("Histogrammes des attributs"),
              box(plotOutput("hist") ),
              box(selectInput("features","Histogrammes des variables:",c("laufkont","laufzeit","moral","verw","hoehe")))
      ),
      tabItem("sum",h1(" Vue sommaire des onnées "),
        verbatimTextOutput("donne")
      ),
      tabItem("widgets",
              fluidPage(
                h1("Jeu de données"),
                dataTableOutput("jeu")
              )
      )
  )  
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$hist <- renderPlot({
    plot(dat[[input$features]],xlab=dat[[input$features]])
    })
    output$donne <- renderPrint({
      summary(dat)
      
    })
    output$jeu <- renderDataTable(dat)
      
}

# Run the application 
shinyApp(ui = ui, server = server)
