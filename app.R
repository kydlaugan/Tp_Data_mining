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
nom_colonne <- c("status", "duration", "credit_history", "purpose", "amount", 
                 "savings", "employment_duration", "installment_rate",
                 "personal_status_sex", "other_debtors",
                 "present_residence", "property",
                 "age", "other_installment_plans",
                 "housing", "number_credits",
                 "job", "people_liable", "telephone", "foreign_worker",
                 "credit_risk")
names(dat) <- nom_colonne
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = " INF 3115 PROJET "),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse descriptive", tabName = "Analyse descriptive", icon = icon("dashboard"),
               menuSubItem("Histogrammes des attributs",tabName = "hist"),
               menuSubItem("Summary",tabName = "sum"),
               menuSubItem("Distribution",tabName = "dis")
               )
    )
  ),
  dashboardBody( 
    tabItems(
      tabItem("Analyse descriptive",h1("Analyse statistique")),
      tabItem("hist",h1("Histogrammes des attributs"),
              box(selectInput("features","Histogrammes des variables:",nom_colonne,width = 300),width = 4),
              box(plotOutput("hist") )
      ),
      tabItem("sum",h1(" Vue sommaire des onnées "),
        p(strong("Chaque Attribut est representé respectivement par son minimum , son premier quartile, la médiane la moyenne ,le troisieme quartile ,et le maximum  "), .noWS = NULL, .renderHook = NULL),
        verbatimTextOutput("donne")
      ),
      tabItem("dis",h1("Boite a moustache de chaque attribut"),
              box(selectInput("feature","Boite a moustache:",nom_colonne,width = 300),width = 4),
              box(plotOutput("dis") ),
              strong("Les deux traits horizontaux de la boite
sont le 1er et le 3e quartiles de l'attribut choisi"),
              strong("Le trait fort est la médiane de l'attribut choisi ")
      )
  )  
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$hist <- renderPlot({
    plot(hist(dat[[input$features]]),xlab=input$features,ylab=input$features)
    })
    output$dis <- renderPlot({
      boxplot(dat[[input$feature]])
    })
    output$donne <- renderPrint({
      summary(dat)
      
    })
    output$jeu <- renderDataTable(dat)
      
}

# Run the application 
shinyApp(ui = ui, server = server)
