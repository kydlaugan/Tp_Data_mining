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
    dat <- read.table("Data_set/SouthGermanCredit/SouthGermanCredit.asc", header=TRUE)
      
      #renommage des colonnes
      nom_colonne <- c("status", "duration", "credit_history", "purpose", "amount", 
                      "savings", "employment_duration", "installment_rate",
                      "personal_status_sex", "other_debtors",
                      "present_residence", "property",
                      "age", "other_installment_plans",
                      "housing", "number_credits",
                      "job", "people_liable", "telephone", "foreign_worker",
                      "credit_risk")
      names(dat) <- nom_colonne
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


}