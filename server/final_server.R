server <- function(input , output){
    #presentation de la banque
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

    #affichage des données
    donnee <- read.table("Data_set/SouthGermanCredit/SouthGermanCredit.asc", header=TRUE)
      
      #renommage des colonnes
      nom_colonne <- c("status", "duration", "credit_history", "purpose", "amount", 
                      "savings", "employment_duration", "installment_rate",
                      "personal_status_sex", "other_debtors",
                      "present_residence", "property",
                      "age", "other_installment_plans",
                      "housing", "number_credits",
                      "job", "people_liable", "telephone", "foreign_worker",
                      "credit_risk")
      names(donnee) <- nom_colonne
      data <- donnee
      dat <- donnee

    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
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
    #renommage des données
    for (i in setdiff(1:21, c(2,4,5,13)))
         dat[[i]] <- factor(dat[[i]])
    
    dat[[4]] <- factor(dat[[4]], levels=as.character(0:10))

    
    levels(dat$credit_risk) <- c("bad", "good")
    levels(dat$status) = c("no checking account",
                         "... < 0 DM",
                         "0<= ... < 200 DM",
                         "... >= 200 DM / salary for at least 1 year")   
    
    levels(dat$credit_history) <- c(
  "delay in paying off in the past",
  "critical account/other credits elsewhere",
  "no credits taken/all credits paid back duly",
  "existing credits paid back duly till now",
  "all credits at this bank paid back duly")

   levels(dat$purpose) <- c(
  "others",
  "car (new)",
  "car (used)",
  "furniture/equipment",
  "radio/television",
  "domestic appliances",
  "repairs",
  "education", 
  "vacation",
  "retraining",
  "business")

  levels(dat$savings) <- c("unknown/no savings account",
                         "... <  100 DM", 
                         "100 <= ... <  500 DM",
                         "500 <= ... < 1000 DM", 
                         "... >= 1000 DM")
  levels(dat$employment_duration) <- 
                  c(  "unemployed", 
                      "< 1 yr", 
                      "1 <= ... < 4 yrs",
                      "4 <= ... < 7 yrs", 
                      ">= 7 yrs")

  dat$installment_rate <- ordered(dat$installment_rate)
  levels(dat$installment_rate) <- c(">= 35", 
                                  "25 <= ... < 35",
                                  "20 <= ... < 25", 
                                  "< 20")
  levels(dat$other_debtors) <- c(
  "none",
  "co-applicant",
  "guarantor")

  levels(dat$personal_status_sex) <- c(
  "male : divorced/separated",
  "female : non-single or male : single",
  "male : married/widowed",
  "female : single")
  dat$present_residence <- ordered(dat$present_residence)
  levels(dat$present_residence) <- c("< 1 yr", 
                                   "1 <= ... < 4 yrs", 
                                   "4 <= ... < 7 yrs", 
                                   ">= 7 yrs")

  levels(dat$property) <- c(
  "unknown / no property",
  "car or other",
  "building soc. savings agr./life insurance", 
  "real estate"
)
levels(dat$other_installment_plans) <- c(
  "bank",
  "stores",
  "none"
)


levels(dat$housing) <- c("for free", "rent", "own")
dat$number_credits <- ordered(dat$number_credits)
levels(dat$number_credits) <- c("1", "2-3", "4-5", ">= 6")
## manager/self-empl./highly qualif. employee  was
##   management/self-employed/highly qualified employee/officer
levels(dat$job) <- c(
  "unemployed/unskilled - non-resident",
  "unskilled - resident",
  "skilled employee/official",
  "manager/self-empl./highly qualif. employee"
)
 levels(dat$people_liable) <- c("3 or more", "0 to 2")
 levels(dat$telephone) <- c("no", "yes (under customer name)")
 levels(dat$foreign_worker) <- c("yes", "no")

 ## checks against fahrmeir table
tabs <- 
list(status = round(100*prop.table(xtabs(~status+credit_risk, dat),2),2),
credit_history = round(100*prop.table(xtabs(~credit_history+credit_risk, dat),2),2),
purpose = round(100*prop.table(xtabs(~purpose+credit_risk, dat),2),2),
savings = round(100*prop.table(xtabs(~savings+credit_risk, dat),2),2),
employment_duration = round(100*prop.table(xtabs(~employment_duration+credit_risk, dat),2),2),
installment_rate = round(100*prop.table(xtabs(~installment_rate+credit_risk, dat),2),2),
personal_status_sex = round(100*prop.table(xtabs(~personal_status_sex+credit_risk, dat),2),2),
other_debtors = round(100*prop.table(xtabs(~other_debtors+credit_risk, dat),2),2),
present_residence = round(100*prop.table(xtabs(~present_residence+credit_risk, dat),2),2),
property = round(100*prop.table(xtabs(~property+credit_risk, dat),2),2),
other_installment_plans = round(100*prop.table(xtabs(~other_installment_plans+credit_risk, dat),2),2),
housing = round(100*prop.table(xtabs(~housing+credit_risk, dat),2),2),
number_credits = round(100*prop.table(xtabs(~number_credits+credit_risk, dat),2),2),
job = round(100*prop.table(xtabs(~job+credit_risk, dat),2),2),
people_liable = round(100*prop.table(xtabs(~people_liable+credit_risk, dat),2),2),
telephone = round(100*prop.table(xtabs(~telephone+credit_risk, dat),2),2),
foreign_worker = round(100*prop.table(xtabs(~foreign_worker+credit_risk, dat),2),2))

## variables for which a tab entry is available
## (all except 2, 5 and 13)
tabwhich <- setdiff(1:20, c(2,5,13))


 output$tables<- DT::renderDataTable(DT::datatable({
        dat
    })) 

output$table1<- DT::renderDataTable(DT::datatable({
        tabs$status
    })) 

output$table2<- DT::renderDataTable(DT::datatable({
        tabs$credit_history
    })) 

output$table3<- DT::renderDataTable(DT::datatable({
        tabs$purpose
    })) 

output$table4<- DT::renderDataTable(DT::datatable({
        tabs$savings
    })) 

output$table5<- DT::renderDataTable(DT::datatable({
        tabs$employment_duration
    })) 
output$table6<- DT::renderDataTable(DT::datatable({
        tabs$installment_rate
    })) 
output$table7<- DT::renderDataTable(DT::datatable({
        tabs$personal_status_sex
    })) 
output$table8<- DT::renderDataTable(DT::datatable({
        tabs$other_debtors
    })) 
output$table9<- DT::renderDataTable(DT::datatable({
        tabs$present_residence
    })) 
output$table10<- DT::renderDataTable(DT::datatable({
        tabs$property
    })) 
output$table11<- DT::renderDataTable(DT::datatable({
        tabs$other_installment_plans
    })) 
output$table12<- DT::renderDataTable(DT::datatable({
        tabs$housing
    })) 
output$table13<- DT::renderDataTable(DT::datatable({
        tabs$number_credits
    })) 
output$table14<- DT::renderDataTable(DT::datatable({
        tabs$job
    })) 
output$table15<- DT::renderDataTable(DT::datatable({
        tabs$people_liable
    })) 
output$table16<- DT::renderDataTable(DT::datatable({
        tabs$telephone
    })) 
output$table17<- DT::renderDataTable(DT::datatable({
        tabs$foreign_worker
    })) 
#suppression de other-installment_plan et people_liable
dat[ ,-14]
dat[ ,-18]
data[ ,-14]
data[ ,-18]
data <- select(data,-c(people_liable,other_installment_plans))

# affichage des valeurs manquantes

output$manq <- renderText({
  #compter le nombres de  valeurs manquantes
   nrow(data[!complete.cases(data),])
})
Valeurs_manquantes <- is.na(data)
output$val_na<- DT::renderDataTable(DT::datatable({
        Valeurs_manquantes
    })) 

#Analyse descriptive
output$hist <- renderPlot({
    plot(hist(data[[input$features]]),xlab=input$features,ylab=input$features)
    })
    output$dis <- renderPlot({
      boxplot(data[[input$feature]])
    })
    output$donne <- renderPrint({
      summary(data)
      
    })
    output$jeu <- renderDataTable(dat)

#Extraction des regles
rulesdata <- donnee
rulesdata <- select(donnee,-c(people_liable,other_installment_plans))
rules = apriori(data= rulesdata[, -19], parameter = list(support = 0.6,
                                                        confidence = 0.8))

trans <- as(rulesdata[,-19], "transactions")
dim(trans)
itemLabels(trans)
summary(trans)
output$regles <- renderPlot({
     itemFrequencyPlot(trans, topN=5,  cex.names=1)
})
#construction de l'arbre de décision 
jeu_decision <- dat
nbre_lignes <- floor((nrow(jeu_decision)*0.7))    
jeu_decision <- jeu_decision[sample(nrow(jeu_decision)) ,]
donnee_apprentissage <- jeu_decision[1:nbre_lignes , ]
donnee_test <- jeu_decision[(nbre_lignes+1):nrow(jeu_decision) , ] 
 
arbre <- rpart(credit_risk ~. , data = donnee_apprentissage)

#validation du modèle
prediction <- predict(arbre , donnee_test[,-21] , type='class')
data.frame(donnee_test$credit_risk ,prediction)
mc  <- table(donnee_test$credit_risk ,prediction )
mce <- CrossTable(donnee_test$credit_risk , prediction)

output$decision <- renderPlot({
  rpart.plot(arbre)

})

output$confusion1 <- renderPrint({
    mce$t
})
#taux de réussite 
    somme<- ((mc[1,1] + mc[2,2])/sum(mc))*100
output$taux_reussite <-renderText({
    somme
})

#precision avec l'arbre de décision
output$precision1 <- renderText({
    mc[1,1]/(mc[1,1]+mc[2,1])
})
output$precision1_1 <- renderText({
    (mc[1,1]/(mc[1,1]+mc[2,1]))*100
})
output$precision2 <- renderText({
   mc[2,2]/(mc[2,2]+mc[1,2])
})
output$precision2_2 <- renderText({
   (mc[2,2]/(mc[2,2]+mc[1,2]))*100
})
#rappel avec l'arbre de decision
output$rappel1 <- renderText({
    (mc[1,1]/(mc[1,1]+mc[1,2]))*100
})
output$rappel2 <- renderText({
    (mc[2,2]/(mc[2,2]+mc[2,1]))*100
})

#réseau de neuronnes  
#levels(data$credit_risk) <- c("bad", "good")
#jeu_decision1 <- data
#nbre_lignes1 <- floor((nrow(jeu_decision1)*0.7))    
#jeu_decision1 <- jeu_decision1[sample(nrow(jeu_decision1)) ,]
#donnee_apprentissage1 <- jeu_decision1[1:nbre_lignes1 , ]
#donnee_test1 <- jeu_decision1[(nbre_lignes1+1):nrow(jeu_decision1) , ] 
#n1 <- neuralnet(credit_risk ~. , donnee_apprentissage1 , hidden =3)
#plot(n1)


#mypredict <- compute(n1, donnee_test1[,-21])$net.result
#maxidx <- function(arr) { return(which(arr == max(arr))) } 
#idx <- apply(mypredict, c(2), maxidx)
#prediction1 <- c('good','bad')[idx] 
#table(prediction1, donnee_test1$credit_risk)
#mce1 <- CrossTable(donnee_test1$credit_risk , prediction1)


# Random sampling
samplesize = 0.70 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

## Scale data for neural network

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))
# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]
levels(trainNN$credit_risk) <- c("bad", "good")
# fit neural network
set.seed(2)
NN = neuralnet(credit_risk ~ status + duration + credit_history + purpose + amount +
                       savings + employment_duration + installment_rate +
                       personal_status_sex + other_debtors +
                       present_residence + property +
                       age +
                       housing + number_credits +
                       job + telephone + foreign_worker , trainNN ,hidden = 3 , linear.output = T )
plot(NN)

file.remove('image/export.png')
dev.print(device = png, file = "image/export.png", width = 600,height = 900)
#png(file = "out.png", width = 600, height = 900)
#plot(NN)
#dev.off()
output$neuronne <- renderImage({
  list(src = "image/export.png",
       width = 600,
       height= 900)
})

# debut knn
datakppv <- donnee
set.seed(9850)
#on divise les données en donnees d'entrainement et de test
nt = sample(1:nrow(datakppv),0.7*nrow(datakppv))
data_train = datakppv[nt,1:20]
data_test = datakppv[-nt,1:20]
data_train_target = datakppv[nt,21] 
data_train_test = datakppv[-nt,21]
proche = knn(train = data_train , 
             test = data_test ,
             cl=data_train_target , 
             k=round(sqrt(nrow(datakppv))))
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
output$kppv <- renderPlot({
  plot(ggplot(plot_predictions, aes(duration,status,color=predicted,fill=predicted)) +
         geom_point(size=3)+
         ggtitle("Relations des predictions entre les deux variables choisies ")+
         theme(plot.title=element_text(hjust=0.5))+
         theme(legend.position="none"))
})

#methode svm
donnee_svm <- donnee
for (j in setdiff(1:21, c(2,4,5,13)))
         donnee_svm[[j]] <- factor(donnee_svm[[j]])
    
donnee_svm[[4]] <- factor(donnee_svm[[4]], levels=as.character(0:10))    
levels(donnee_svm$credit_risk) <- c("bad", "good")
mymodel <- svm(credit_risk~. , donnee_svm)
summary(mymodel)
pred <- predict(mymodel , donnee_svm)
tab <- table(Predicted=pred , Actual = donnee_svm$credit_risk)
tabcros <- CrossTable(pred , donnee_svm$credit_risk)


output$confusion2 <- renderPrint({
    tabcros$t
})
#taux de réussite 
    somme2 <- ((tab[1,1] + tab[2,2])/sum(tab))*100
output$taux_reussite2 <-renderText({
    somme2
})

#precision avec l'arbre de décision
output$precision3 <- renderText({
    tab[1,1]/(tab[1,1]+tab[2,1])
})
output$precision3_3 <- renderText({
    (tab[1,1]/(tab[1,1]+tab[2,1]))*100
})
output$precision4 <- renderText({
   tab[2,2]/(tab[2,2]+tab[1,2])
})
output$precision4_4 <- renderText({
   (tab[2,2]/(tab[2,2]+tab[1,2]))*100
})
#rappel avec l'arbre de decision
output$rappel3 <- renderText({
    (tab[1,1]/(tab[1,1]+tab[1,2]))*100
})
output$rappel4 <- renderText({
    (tab[2,2]/(tab[2,2]+tab[2,1]))*100
})


}