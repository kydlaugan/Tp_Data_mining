
#chargement des données
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
ui <- fluidPage(
  
  # Application title
  titlePanel("Affichage des données "),
  
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("dur",
                       "Duration:",
                       c("All",
                         unique(as.character(data$duration))))
    ),
    column(4,
           selectInput("amo",
                       "Amount:",
                       c("All",
                         unique(as.character(data$amount))))
    ),
    column(4,
           selectInput("ag",
                       "Age:",
                       c("All",
                         unique(as.character(data$age))))
    )
  ),
  
  # Create a new row for the table.
  DT::dataTableOutput("table")
  
)