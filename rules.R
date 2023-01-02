library(dplyr)
library(arules)
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
head(dat)
dat <- select(dat,-c(people_liable,other_installment_plans))
head(dat)
dim(dat)
dat_itemlist <- dply(dat,c("amont"),function(df1)paste(df1$titre,collapse = ","))
rules = apriori(data= dat, parameter = list(support = 0.6,
                                                        confidence = 0.8))

trans <- as(dat, "transactions")
dim(trans)
itemLabels(trans)
summary(trans)
itemFrequencyPlot(trans, topN=5,  cex.names=1)
rules <- apriori(trans, 
                 parameter = list(supp=0.6, conf=0.8, 
                                  maxlen=10, 
                                  minlen=2,
                                  target= "rules"))
inspect(rules)
subrules <- head(rules, n = 5, by = "confidence")
plot(subrules, method = "graph",  engine = "htmlwidget")

