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
                 "job", "people_liable", "telephone", "foreign_worker")
colnames(dat) <- nom_colonne
set.seed(9850)
#on divise les données en donnees d'entrainement et de test
nt = sample(1:nrow(dat),0.7*nrow(dat))
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
colnames(plot_predictions) <- c("status", "duration", "credit_history", "purpose", "amount", 
                               "savings", "employment_duration", "installment_rate",
                               "personal_status_sex", "other_debtors",
                               "present_residence", "property",
                               "age", "other_installment_plans",
                               "housing", "number_credits",
                               "job", "people_liable", "telephone", "foreign_worker",
                               "predicted")
library(ggplot2)
library(gridExtra)
library(dplyr)
library(factoextra)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = " INF 3115 PROJET "),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse descriptive", tabName = "Analyse descriptive", icon = icon("dashboard"),
               menuSubItem("Histogrammes des attributs",tabName = "hist"),
               menuSubItem("Summary",tabName = "sum"),
               menuSubItem("Distribution",tabName = "dis"),
               menuSubItem("k proches voisins",tabName = "kppv"),
               menuSubItem("Nuage de Points",tabName = "ndp"),
               menuSubItem("Clustering",tabName = "cl")
      )
    )
  ),
  dashboardBody( 
    tabItems(
      tabItem("Analyse descriptive",h1("Analyse statistique")),
      tabItem("hist",h1("Histogrammes des attributs"),
              p("Un Histogramme est un outil utilisé pour résumer des données représentées par intervalles de valeurs "),
              box(selectInput("features","Histogrammes des variables:",nom_colonne,width = 300),width = 4),
              plotOutput("hist")
      ),
      tabItem("sum",h1(" Vue sommaire des onnées "),
              p(strong("Chaque attribut est representé respectivement par les paramètres de tendance centrale qui sont la médiane et la moyenne , et ensuite les troisieme et premier quartile , le minimun et le maximum  "), .noWS = NULL, .renderHook = NULL),
              verbatimTextOutput("donne")
      ),
      tabItem("dis",h1("Boite a moustacheS de chaque attribut"),
              box(selectInput("feature","Boite a moustache:",nom_colonne,width = 300),width = 6),
              box(strong("Les deux traits horizontaux de la boite
sont le 1er et le 3e quartile de l'attribut choisi et le trait fort du milieu représente la médiane qui est de deuxième quartile de l'att
ribut
               ")),
              box(strong("Les extrémités des moustaches sont calculeés en utilisant 1.5 fois l'espace inter-quartile,qui est la distance entre le premier et le troisième quartile
              ")),
              plotOutput("dis") 
              
      ),
      tabItem("kppv",h1("Methode de K plus proches voisins"),
              box(textInput("txt1"," Entree 1 ","",width = 300),width = 6),
              box(textInput("txt2"," Entree 2 ","",width = 300),width = 6),
              actionButton(inputId = "id_action", label = "Ok !",
                           icon = icon("refresh")),
              
              plotOutput("kppv" )
      
      ),
      tabItem("ndp",h1("Nuage de Points"),
              box(selectInput("feature1"," Entree 1 ",nom_colonne,width = 300),width = 6),
              box(selectInput("fit1"," Entree 2 ",nom_colonne,width = 300),width = 6),
              plotOutput("ndp" ),
              
      ),
      tabItem(  "cl",
                box(selectInput("n_cluster","Nombre de clusters",c(2:10),width = 300),width = 6),
                box(strong("La figure ci-dessous représente le classement non supervisé de 1000 donneés en le nombre de clusters choisi.Chaque cluster représente un groupe.
                           La classification non supervisée permet de regrouper des données non etiquetées en groupes en se servant des distances qui existent entre eux pour les classer. ")),
                plotOutput("cl")
                )
    )  
  ))


server <- function(input, output) {
  
  output$hist <- renderPlot({
    plot(hist(dat[[input$features]]),xlab=paste(input$features), col="red",main="Histogram ")
  })
  output$dis <- renderPlot({
    boxplot(dat[[input$feature]])
  })
 
  output$donne <- renderPrint({
    summary(dat)
    
  })
  output$jeu <- renderDataTable(dat)
  
  output$kppv <- renderPlot({
    input$id_action
    isolate(plot(ggplot(plot_predictions, aes(x=get(input$txt1),y=get(input$txt2),color=predicted,fill=predicted)) +
           geom_point(size=3)+
           ggtitle("Relations des predictions entre les deux variables choisies ")+
           theme(plot.title=element_text(hjust=0.5))+
           theme(legend.position="none"),xlab=paste(input$txt1)))

  })
  output$ndp <- renderPlot({
    plot(dat[[input$feature1]],dat[[input$fit1]],xlab="Entree 1",ylab="Entree 2")
  })
  output$cl <- renderPlot({
    data_labels=dat$credit_risk
    datas=dat[1:20]
    data_scale=scale(datas)
    datas=dist(data_scale)
    km.out = kmeans(data_scale,centers =input$n_cluster)
    km.clusters = km.out$cluster
    rownames(data_scale) = paste(dat$credit_risk,1:dim(dat)[1],sep="_")
    fviz_cluster(list(data=data_scale ,cluster=km.clusters))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
