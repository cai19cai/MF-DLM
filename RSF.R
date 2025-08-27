rm(list=ls())
library(tidyverse)
library(mxjqkit)
library(survival)
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
source("tidyfuncs4sa.R")
library(dplyr)

library(doParallel)
registerDoParallel(
  makePSOCKcluster(
    max(1, (parallel::detectCores(logical = F))-1)
  )
)

setwd("")   
dt1 <- read_csv("") %>%
  column_to_rownames(var = names(.)[1])  
dt2 <- read_csv("Borutadata.csv")%>%
   column_to_rownames(var = names(.)[1]) %>%
   select(-time) %>%
   select(-status) 

dt <- merge(dt1, dt2, by = "row.names", all = TRUE) %>%  
  column_to_rownames(var = "Row.names") 

traindata <- dt %>% filter(group == "train") %>% select(-group)
testdata  <- dt %>% filter(group == "val")   %>% select(-group)
newdata   <- dt %>% filter(group == "multi") %>% select(-group)

sort(unique(traindata$time))
itps <- c(12 * c(1,2,3,4,5))
itps
table(cut(traindata$time, c(0, itps, Inf)))

measure_sa <- msrs("surv.cindex")
measure_sa

cols4model <- c("#8DD3C7", "#FFD92F", "#8DA0CB", "#FB8072", "#80B1D3"
                ,"#FDB462" ,"#B3DE69" ,"#E78AC3", "#2ECC71")

task_train <- as_task_surv(
  traindata, 
  time = "time",
  event = "status", 
  type = "right"
)
task_train

task_test <- as_task_surv(
  testdata, 
  time = "time",
  event = "status", 
  type = "right"
)
task_test

###################################################

# 随机森林
# https://mlr3extralearners.mlr-org.com/reference/mlr_learners_surv.rfsrc.html
# install.packages("randomForestSRC")

learner_rsf <- lrn(
  "surv.rfsrc",
  ntree = to_tune(100, 450),
  mtry = to_tune(1, 5),
  nodesize = to_tune(2, 30)
)

learner_rsf$id <- "rsf"
learner_rsf

set.seed(42)
tune_rsf <- tune(
  tuner = tnr(
    "grid_search", 
    resolution = 3
  ),
  task = task_train,
  learner = learner_rsf,
  resampling = rsmp("cv", folds = 5),
  measure = msr("surv.cindex"),
  terminator = trm("none")
)
tune_rsf
as.data.table(tune_rsf$archive) %>%
  as.data.frame() %>%
  select(1:4) %>%
  plotly::plot_ly(
    type = 'parcoords',
    line = list(color = ~surv.cindex, 
                colorscale = 'Jet', 
                showscale = T),
    dimensions = list(
      list(label = 'ntree', values = ~ntree),
      list(label = 'mtry', values = ~mtry),
      list(label = 'nodesize', values = ~nodesize)
    )
  ) %>%
  plotly::layout(title = "RSF HPO Guided by C-Index",
                 font = list(family = "serif"))


learner_rsf$param_set$values <-
  tune_rsf$result_learner_param_vals
set.seed(42)
learner_rsf$train(task_train)
learner_rsf

learner_rsf
learner_rsf$model

###################################################

predtrain_rsf <- learner_rsf$predict(task_train)
predprobtrain_rsf <- predprob(
  pred = predtrain_rsf, 
  data4pred = traindata, 
  Time = "time",
  Statu = "status",
  model = "RSF", 
  dataset = "train", 
  timepoints = itps
)

predtrain_rsf$score(measure_sa)
# bootci_cindex(learner_rsf, traindata)
Traincindex<-bootci_cindex(learner_rsf, traindata)
Traincindex_low <- Traincindex["2.5%"]
Traincindex_high <- Traincindex["97.5%"]


evaltrain_rsf <- eval4sa(
  predprob = predprobtrain_rsf,
  data4pred = traindata,
  Time = "time",
  Statu = "status",
  model = "RSF",
  dataset = "train",
  timepoints = itps,
  calimethod = "quantile",  # nne
  q4quantile = 3,
  groupno = 3, 
  groupcut = NULL, 
  groupname = c("L", "M", "H"), 
  color4times = cols4model
)
evaltrain_rsf$plot_risk
evaltrain_rsf$auroc
evaltrain_rsf$plotroc
# evaltrain_rsf$aupr
# evaltrain_rsf$plotpr
# evaltrain_rsf$brierscore
# evaltrain_rsf$plotcalibration
# evaltrain_rsf$plotdca


predtest_rsf <- learner_rsf$predict(task_test)
predprobtest_rsf <- predprob(
  pred = predtest_rsf, 
  data4pred = testdata, 
  Time = "time",
  Statu = "status",
  model = "RSF", 
  dataset = "test", 
  timepoints =itps
)
predtest_rsf$score(measure_sa)
# bootci_cindex(learner_rsf, testdata)
Testcindex<-bootci_cindex(learner_rsf, testdata)
Testcindex_low <- Testcindex["2.5%"]
Testcindex_high <- Testcindex["97.5%"]

evaltest_rsf <- eval4sa(
  predprob = predprobtest_rsf,
  data4pred = testdata,
  Time = "time",
  Statu = "status",
  model = "RSF",
  dataset = "test",
  timepoints = itps,
  calimethod = "quantile",  # nne
  q4quantile = 3,
  groupno = NULL, 
  groupcut = evaltrain_rsf$groupcut, 
  groupname = c("L", "M", "H"), 
  color4times = cols4model
)
evaltest_rsf$plot_risk
evaltest_rsf$auroc
evaltest_rsf$plotroc
# evaltest_rsf$aupr
# evaltest_rsf$plotpr
# evaltest_rsf$brierscore
# evaltest_rsf$plotcalibration
# evaltest_rsf$plotdca
prednew_rsf <- learner_rsf$predict_newdata(newdata)
predprobnew_rsf <- predprob(
  pred = prednew_rsf, 
  data4pred = newdata, 
  Time = "time",
  Statu = "status",
  model = "RSF", 
  dataset = "External test", 
  timepoints =itps
)
prednew_rsf$score(measure_sa)
# bootci_cindex(learner_rsf, newdata)
newcindex<-bootci_cindex(learner_rsf, newdata)
newcindex_low <- newcindex["2.5%"]
newcindex_high <- newcindex["97.5%"]

evalnew_rsf <- eval4sa(
  predprob = predprobnew_rsf,
  data4pred = newdata,
  Time = "time",
  Statu = "status",
  model = "RSF",
  dataset = "External test",
  timepoints = itps,
  calimethod = "quantile",  # nne
  q4quantile = 5,
  groupno = NULL, 
  groupcut = evaltrain_rsf$groupcut, 
  groupname = c("L", "M", "H"), 
  color4times = cols4model
)
evalnew_rsf$plot_risk
evalnew_rsf$auroc
evalnew_rsf$plotroc
# evalnew_rsf$aupr
# evalnew_rsf$plotpr
# evalnew_rsf$brierscore
# evalnew_rsf$plotcalibration
# evalnew_rsf$plotdca

################################################################################

auc1<-evaltrain_rsf$auroc
auc1$Cindex<-predtrain_rsf$score(msrs(c("surv.cindex")))
auc1$Cindex_low<-Traincindex_low
auc1$Cindex_high<-Traincindex_high

auc2<-evaltest_rsf$auroc
auc2$Cindex<-predtest_rsf$score(msrs(c("surv.cindex")))
auc2$Cindex_low<-Testcindex_low
auc2$Cindex_high<-Testcindex_high

auc3<-evalnew_rsf$auroc
auc3$Cindex<-prednew_rsf$score(msrs(c("surv.cindex")))
auc3$Cindex_low<-newcindex_low
auc3$Cindex_high<-newcindex_high

auc<-cbind(auc1,auc2,auc3)
write.table(auc, file=".\\AUC\\RSF_auc.csv", sep=",", quote=T, row.names=F, col.names=T)

risk<-rbind(predprobtrain_rsf,predprobtest_rsf,predprobnew_rsf)
riskID<-rbind(traindata,testdata,newdata)
risk$ID<-row.names(riskID)
write.table(risk, file=".\\RISK\\rsfrisk.csv", sep=",", quote=T, row.names=F, col.names=T)

a<-evaltrain_rsf$plotroc
b<-evaltest_rsf$plotroc
c<-evalnew_rsf$plotroc
ggsave(".//Figure//train_rsf$roc.pdf", plot = a, width = 5, height = 5)  
ggsave(".//Figure//test_rsf$roc.pdf", plot = b, width = 5, height = 5)  
ggsave(".//Figure//multi_rsf$roc.pdf", plot = c, width = 5, height = 5) 

save(predtrain_rsf,
     predprobtrain_rsf,
     evaltrain_rsf,
     predtest_rsf,
     predprobtest_rsf,
     evaltest_rsf,
     prednew_rsf,
     predprobnew_rsf,
     evalnew_rsf,  
     file = ".\\model\\rsf.RData")