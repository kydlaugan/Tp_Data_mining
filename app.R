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
set.seed(9850)
str(dat)
#on divise les données en donnees d'entrainement et de test
nt = sample(1:nrow(dat),nrow(dat))
data_train = dat[nt,1:20]
data_test = dat[-nt,1:20]
data_train_target = dat[nt,21] 
data_train_test = dat[-nt,21]
library(class)
proche = knn(train = data_train , 
             test = data_test ,
             cl=data_train_target , 
             k=round(sqrt(nrow(dat))))
plot_predictions = data.frame( data_test$status, 
                               data_test$duration, data_test$credit_history, data_test$purpose, data_test$amount, 
                               data_test$savings, data_test$employment_duration, data_test$installment_rate,
                               data_test$personal_status_sex, data_test$other_debtors,
                               data_test$present_residence, data_test$property,
                               data_test$age, data_test$other_installment_plans,
                               data_test$housing, data_test$number_credits,
                               data_test$job, data_test$people_liable, data_test$telephone, data_test$foreign_worker,
                              predicted=proche )
library(ggplot2)
library(gridExtra)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = " INF 3115 PROJET "),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse descriptive", tabName = "Analyse descriptive", icon = icon("dashboard"),
               menuSubItem("Histogrammes des attributs",tabName = "hist"),
               menuSubItem("Summary",tabName = "sum"),
               menuSubItem("Distribution",tabName = "dis"),
               menuSubItem("knn",tabName = "kppv")
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
      ),
        tabItem("kppv",h1("Methode de K plus proches voisins"),
                h2("Relations etre les prédictions de"+dat[[input$feature]]+"et"+dat[[input$feature]]),
                box(selectInput("features"," "+dat[[input$feature]]+" ",nom_colonne,width = 300),width = 4),
                box(selectInput("features"," "+dat[[input$feature]]+" ",nom_colonne,width = 300),width = 4),
                box(plotOutput("kppv") )
        )
  )  
))


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
    output$kppv <- renderPrint({
      ggplot2(plot_predictions, aes(dat[[input$features]],
                                        dat[[input$features]],
                                        color = predicted,
                                        fill = predicted) +
                    geom_point(size=3)+
                    geom_text(aes(label = test_labels),hjust=1,vjust=2) +
                    ggtitle("Relations etre les prédictions de"+data_test$status+"et"+data_test$status)+
                    theme(plot.title=element_text(hjust=0.5))+
                    theme(legend.position="none"))
    })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
