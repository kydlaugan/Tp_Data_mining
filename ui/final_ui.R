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
      
#chargement des données

#menu du tableau de bord
menu <- dashboardSidebar(
    sidebarMenu(
        menuItem("SouthGermanCredit", tabName = "SouthBank", icon = icon("building-columns" , "fa-2x")),
        menuItem("Data_set", tabName = "widgets", icon = icon("th")),
        menuItem("Valeurs_manquantes", tabName = "NA", icon = icon("th")) , 
        menuItem("Analyse descriptive", tabName = "Analyse descriptive", icon = icon("dashboard"),
               menuSubItem("Histogrammes des attributs",tabName = "hist"),
               menuSubItem("Summary",tabName = "sum"),
               menuSubItem("Distribution",tabName = "dis")
               ),
        menuItem("Classification supervisée" , tabName  = "class-super" , icon = icon("th") ,
            menuSubItem("Arbre de Décision" ,tabName = "decision"),
            menuSubItem("Réseau de neuronnes" ,tabName = "neuronne")
        )

    )
)

# corps du tableau de bord
 corps <- dashboardBody(
    tabItems(
        #debut premier item
        tabItem(
            tabName="SouthBank" , 
            fluidRow(
                column(
                    6,
                    tags$h1( class="text-center  text-primary" ,"Données des Crédits d'Allemagne du sud") ,
                    h3("Lorsqu'une banque reçoit une demande de prêt, en fonction du profil du demandeur, la banque doit prendre une décision quant à l'approbation ou non du prêt. Deux types de risques sont associés à la décision de la banque :"),
                    tags$ul(
                         tags$li("Si le demandeur présente un bon risque de crédit, c'est-à-dire qu'il est susceptible de rembourser le prêt, le fait de ne pas approuver le prêt à la personne entraîne une perte d'activité pour la banque" , style=("font-size:20px")),
                         tags$li("Si le demandeur est un mauvais risque de crédit, c'est-à-dire qu'il est peu probable qu'il rembourse le prêt, l'approbation du prêt à la personne entraîne une perte financière pour la banque" , style=("font-size:20px"))
                    ),

                ),
                column(
                    6,
                    imageOutput("test1")
                )
            ),

            fluidRow(
                column(
                    6,
                    imageOutput("test2")
                ),
                column(
                    6,
                    p("L'octroi de prêts personnels est subordonné à la solvabilité du client, c'est-à-dire à la volonté et à la capacité du client de payer correctement les intérêts et les échéances de remboursement. Un emprunteur potentiel doit être affecté soit à la catégorie des emprunteurs sans problème, soit à des cas problématiques où un examen plus approfondi doit être effectué ou l'activité de prêt doit être supprimée. Chaque client de crédit se caractérise par un certain nombre de caractéristiques qui caractérisent sa situation personnelle, économique et juridique. Sur la base de ces caractéristiques, une tentative est faite pour prendre une décision statistiquement sûre d'accorder ou de refuser le prêt.",style=("margin-top:25% ;font-size:20px"))


                )
            )
        ) ,
        #fin premier item

        #debut du second item
        tabItem(tabName = "widgets",
            navs_tab_card(
            title = "Affichage des données",
            nav(
                "Données_brutes", 
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
            DT::dataTableOutput("table"),
            ),
            nav(
                "Données_nommées", 
            DT::dataTableOutput("tables"),
            ),
            nav(
                "Frequences_des_attributs", 
                fluidRow(
                    column(
                        6,
                        box(DT::dataTableOutput("table1" , height="300") ,width="500"),

                    ),
                    column(
                        6,
                        box(DT::dataTableOutput("table2" , height="300") , width="500"),

                    )
                ),
                fluidRow(
                    column(
                        6,
                        box(DT::dataTableOutput("table4" , height="300") ,width="500"),

                    ),
                    column(
                        6,
                        box(DT::dataTableOutput("table5" , height="300"), width="500"),

                    )
                ),
                fluidRow(
                    column(
                        6,
                        box(DT::dataTableOutput("table6" , height="300"),width="500"),

                    ),
                    column(
                        6,
                        box(DT::dataTableOutput("table7" , height="300"),width="500"),

                    )
                ),
                fluidRow(
                    column(
                        6,
                        box(DT::dataTableOutput("table8" , height="300"),width="500"),

                    ),
                    column(
                        6,
                        box(DT::dataTableOutput("table3" , height="300"), width="500"),

                    )
                ),
                fluidRow(
                    column(
                        6,
                        box(DT::dataTableOutput("table9" , height="300"),width="500"),

                    ),
                    column(
                        6,
                        box(DT::dataTableOutput("table10" , height="300"),width="500"),

                    )
                ),
                fluidRow(
                    column(
                        6,
                        box(DT::dataTableOutput("table11" , height="300"),width="500"),

                    ),
                    column(
                        6,
                        box(DT::dataTableOutput("table12" , height="300"), width="500"),

                    )
                ),
                fluidRow(
                    column(
                        6,
                        box(DT::dataTableOutput("table13" , height="300"),width="500"),

                    ),
                    column(
                        6,
                        box(DT::dataTableOutput("table14" , height="300"),width="500"),

                    )
                ),
                fluidRow(
                    column(
                        6,
                        box(DT::dataTableOutput("table15" , height="300"),width="500"),

                    ),
                    column(
                        6,
                        box(DT::dataTableOutput("table16" , height="300"), width="500"),

                    )
                ),
                fluidRow(
                    column(
                        12,
                        box(DT::dataTableOutput("table17" , height="300") , width= 10000),

                    )
                ) ,
            tags$p( class="fs-3" ,"Au vu de toutes ces relations , nous pouvons conclure que nous pouvons nous passer de  l'attribut",
              tags$strong("people_liable et other_installement_plans") ,"parceque peu importe le nombre d'individu qui depandent du débiteur , les chances qu'il soit crédible ou pas sont  quasi-égales !"
            )
            )
            )
        ),
        #debut du troièmes items
        tabItem(tabName="NA" ,
            card(
                 DT::dataTableOutput("val_na") 
            ),
                tags$p("Nous relevons de ce tableau qu'il y a" , 
                 tags$strong("aucune valeurs maquantes donc 0 N/A")
                
                )

        ) ,

        #Analyse descriptive
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
              strong("Les deux traits horizontaux de la boite sont le 1er et le 3e quartiles de l'attribut choisi"),
              strong("Le trait fort est la médiane de l'attribut choisi ")
      ) ,

        #arbre de decision 
        tabItem(tabName ="decision" ,
            fluidRow(
                column(12 , 
                h1(class="text-center" , "Arbre de décision") ,
                box( plotOutput("decision" , height = "800") , width =1000 ) 
                )
            ) ,

            fluidRow(
                column(12 ,
                box( h2("Matrice de confusion ") ,
                verbatimTextOutput("confusion1"),
                p("Avec un taux de réussite de " , textOutput("taux_reussite")))

                )
            )   ,
            fluidRow(
                column(6 ,
                       p("Nous avons une Precision de   " , textOutput("precision1"), "pour les mauvais Credit_risk ce qui signifie que  ", textOutput("precision1_1") ,"des cas prédits par le modèle ont effectivement été jugé comme mauvais Credit_risk")
                ),
                 column(6 ,
                       p("Nous avons une Precision de   " , textOutput("precision2"), "pour les bons Credit_risk ce qui signifie que  ", textOutput("precision2_2") ,"des cas prédits par le modèle ont effectivement été jugé comme bons Credit_risk")
                )
            ) ,
            fluidRow(
                column(6 ,
                       p("C'est donc dire que  " , textOutput("rappel1"), "des cas de mauvais crédit ont été prédit avec pecision")
                ),
                 column(6 ,
                            p("C'est donc dire que  " , textOutput("rappel2"), "des cas de bons crédit ont été prédit avec precision")

                )
            )   

        ),
        #fin arbre de decision
        tabItem(tabName ="neuronne" ,
            fluidRow(
                column(12 , 
                h1(class="text-center" , "Réseau de neuronnes") ,
                box(imageOutput("neuronne"), width=600 , height=900 )
                )
            ) ,
        )

        #fin du reseau de neuronne

    )
 )


ui <- dashboardPage(
    dashboardHeader(title="CreditRisk"),
    menu ,
    corps
)
