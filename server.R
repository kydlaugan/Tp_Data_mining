
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      data <- read.table("Data_set/SouthGermanCredit/SouthGermanCredit.asc", header=TRUE)
      
      #renommage des colonnes
      nom_colonne <- c("status", "duration", "credit_history", "purpose", "amount", 
                      "savings", "employment_duration", "installment_rate",
                      "personal_status_sex", "other_debtors",
                      "present_residence", "property",
                      "age", "other_installment_plans",
                      "housing", "number_credits",
                      "job", "people_liable", "telephone", "foreign_worker",
                      "credit_risk")
      names(data) <- nom_colonne
      
      if (input$dur != "All") {
        data <- data[data$duration == input$dur,]
      }
      if (input$amo != "All") {
        data <- data[data$amount == input$amo,]
      }
      if (input$ag != "All") {
        data <- data[data$age == input$ag,]
      }
      
      data
    })) 
    
    #Presentation de la banque
    output$test1 <- renderImage({
      
      list(src = "image/bank2.png",
           width = "100%",
           height = 500)
      
    }, deleteFile = F) 
    output$test2 <- renderImage({
      
      list(src = "image/bank3.png",
           width = "100%",
           height = 500)
      
    }, deleteFile = F) 
    
}

