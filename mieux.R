library(shiny)
library(dplyr)
library(arules)
library(shinydashboard)
dat <- read.table("/home/junior/Bureau/WD/mon_app/SouthGermanCredit.asc", header=TRUE) 
nom_colonne <- c("status", "duration", "credit_history", "purpose", "amount", 
                 "savings", "employment_duration", "installment_rate",
                 "personal_status_sex", "other_debtors",
                 "present_residence", "property",
                 "age", "other_installment_plans",
                 "housing", "number_credits",
                 "job", "people_liable", "telephone", "foreign_worker",
                 "credit_risk")
names(dat) <- nom_colonne
dat <- select(dat,-c(people_liable,other_installment_plans))
dim(dat)
ui = shinyUI(pageWithSidebar(
  headerPanel("Regle d'association"),
  
  sidebarPanel(
    
    conditionalPanel(
      condition = "input.samp=='quelque'",
      numericInput("nrule", 'Number of Rules', 5), br()
    ),
    
    conditionalPanel(
      condition = "input.mytab %in%' c(, 'table', 'datatable', 'itemFreq')", 
      radioButtons('samp', label='Sample', choices=c('Toutes les regles', 'Simple'), inline=T), br(),
      uiOutput("choose_columns"), br(),
      sliderInput("supp", "Support:", min = 0, max = 1, value = 0.6, step = 1/10000), br(),
      sliderInput("conf", "Confidence:", min = 0, max = 1, value = 0.8 , step = 1/10000), br(),
      selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support')), br(), br(),
      numericInput("minL", "Min. items per set:", 2), br(), 
      numericInput("maxL", "Max. items per set::", 3), br(),
    )
    
  ),
  
  mainPanel(
    tabsetPanel(id='mytab',
                tabPanel('Table', value='table', verbatimTextOutput("rulesTable")),
                tabPanel('ItemFreq', value='itemFreq', plotOutput("itemFreqPlot", width='100%', height='100%'))
    )
  )
  
)
)
server <- function(input, output) {
  output$choose_columns <- renderUI({
    checkboxGroupInput("cols", "Choose variables:", 
                       choices  = colnames(dat),
                       selected = colnames(dat)[1:19])
  })
  
  rules <- reactive({
    tr <- as(dat, 'transactions')
    arAll <- apriori(tr, parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))
  }
  )
  nR <- reactive({
    nRule <- ifelse(input$samp == 'All Rules', length(rules()), input$nrule)
  })
  output$itemFreqPlot <- renderPlot({
    trans <- as(dat, 'transactions')
    itemFrequencyPlot(trans)
  }, height=800, width=800)
  output$rulesDataTable <- renderDataTable({
    ar <- rules()
    rulesdt <- rules2df(ar)
    rulesdt
  })
  
  output$rulesTable <- renderPrint({
    ar <- rules()
    inspect(sort(ar, by=input$sort))
  })
  
 
}
# Run the application 
shinyApp(ui = ui, server = server)