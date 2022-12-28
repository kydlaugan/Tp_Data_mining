tags$script(src = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/css/bootstrap.min.css")
tags$script(src = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/js/bootstrap.bundle.min.js")

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

test <- fluidPage (
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
  DT::dataTableOutput("table") ,
 tags$script(src = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/css/bootstrap.min.css"),
 #tags$script(src = "https://cdn.tailwindcss.com"),
 tags$script(src = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/js/bootstrap.bundle.min.js"),
 
  
)

ui <- dashboardPage(
  dashboardHeader(title = "CreditRisk"),
  dashboardSidebar(
    
    sidebarMenu(
      #premier menu
      menuItem("SouthGermanCredit", tabName = "SouthBank", icon = icon("building-columns" , "fa-2x")) ,
      menuItem("Data_set", tabName = "widgets", icon = icon("th"))
    )
  ) ,
  dashboardBody(
    tabItems(
      
      #contenu_premier menu
      tabItem(tabName = "SouthBank",
              fluidRow(
                column(6,
                       tags$h1( class="text-center  text-primary" ,"Données des Crédits d'Allemagne du sud") ,
                       h3("Lorsqu'une banque reçoit une demande de prêt, en fonction du profil du demandeur, la banque doit prendre une décision quant à l'approbation ou non du prêt. Deux types de risques sont associés à la décision de la banque :"),
                       tags$ul(
                         tags$li("Si le demandeur présente un bon risque de crédit, c'est-à-dire qu'il est susceptible de rembourser le prêt, le fait de ne pas approuver le prêt à la personne entraîne une perte d'activité pour la banque" , style=("font-size:20px")),
                         tags$li("Si le demandeur est un mauvais risque de crédit, c'est-à-dire qu'il est peu probable qu'il rembourse le prêt, l'approbation du prêt à la personne entraîne une perte financière pour la banque" , style=("font-size:20px"))
                       ),
                ),
                column(6 ,
                       imageOutput("test1")
                )
              ),
              fluidRow(
                column(6,
                       imageOutput("test2")
                  
                ),
                column(6 ,
                       p("L'octroi de prêts personnels est subordonné à la solvabilité du client, c'est-à-dire à la volonté et à la capacité du client de payer correctement les intérêts et les échéances de remboursement. Un emprunteur potentiel doit être affecté soit à la catégorie des emprunteurs sans problème, soit à des cas problématiques où un examen plus approfondi doit être effectué ou l'activité de prêt doit être supprimée. Chaque client de crédit se caractérise par un certain nombre de caractéristiques qui caractérisent sa situation personnelle, économique et juridique. Sur la base de ces caractéristiques, une tentative est faite pour prendre une décision statistiquement sûre d'accorder ou de refuser le prêt.",style=("margin-top:25% ;font-size:20px"))
                  
                )
              )
    ),
    
    
    
    #contenu second_menu
    tabItem(tabName = "widgets",

         card(
  height = 150, full_screen = TRUE,
    fluidPage (
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
            ),
            DT::dataTableOutput("table"),
)
            
          

            
      
     )
    ) 
  )
  ) 