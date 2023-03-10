#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2) 
library(shinydashboard)
library(bslib)
library(arules)
library(dplyr)
library(rpart)
library(rpart.plot)
library(gmodels)
library(nnet)
library(neuralnet)
library(e1071)
library(gridExtra)
library(class)
#library(shiny.tailwind)
#chargement de l'interface
source("ui/final_ui.R")  
#chargement du server
source("server/final_server.R")

# Run the application 
shinyApp(ui = ui, server = server)
