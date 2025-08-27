 
rm(list=ls())   
 
library(survival)  
library(dplyr)  
library(survminer)  
library(tidyverse)

setwd("")   
dt <- read_csv("risk_predictions.csv") %>%
  column_to_rownames(var = names(.)[1]) 

predprobtrain <- dt %>% filter(group == "train") %>% select(-group)
predprobtest  <- dt %>% filter(group == "val")   %>% select(-group)
predprobmulti   <- dt %>% filter(group == "multi") %>% select(-group)
predprobnac    <- dt %>% filter(group == "nac") %>% select(-group)


opti_cut <- surv_cutpoint(predprobtrain,   
                          time = "time",   
                          event = "status",   
                          variables = c("risk")) 
 
summary(opti_cut)  
cutpoint=round(opti_cut[["cutpoint"]]$cutpoint,6)


group_cut_train <- surv_categorize(opti_cut)  
colnames(group_cut_train)[3] <- "riskgroup"  
risk_train <-cbind(predprobtrain,group_cut_train["riskgroup"])
write.csv(risk_train,"risk_train.csv",row.names = F)
predprobtest$riskgroup <- ifelse(predprobtest$risk > cutpoint , "high", "low")
risk_test<-predprobtest
write.csv(predprobtest,"risk_validation.csv",row.names = F)

predprobmulti$riskgroup <- ifelse(predprobmulti$risk > cutpoint, "high", "low")
risk_multi<-predprobmulti
write.csv(predprobmulti,"risk_mutli.csv",row.names = F)

predprobnac$riskgroup <- ifelse(predprobnac$risk > cutpoint    , "high", "low")
risk_nac<-predprobnac
write.csv(predprobnac,"risk_nac.csv",row.names = F)

dt <- rbind(risk_train, risk_test, risk_multi)  
dt <- dt %>%
  mutate(ID = rownames(dt)) %>%  
  select(ID, everything())  

write.csv(dt,"FB_score.csv",row.names = F)
risk_train$riskgroup <- factor(risk_train$riskgroup, levels = c('low', 'high'))  
risk_test$riskgroup <- factor(risk_test$riskgroup, levels = c('low', 'high'))  
risk_multi$riskgroup <- factor(risk_multi$riskgroup, levels = c('low', 'high'))  
risk_nac$riskgroup <- factor(risk_nac$riskgroup, levels = c('low', 'high'))

fit_train <- survfit(Surv(time, status) ~ riskgroup, data = risk_train)  
pdf('.//Figure//KM_MBDLM_train.pdf', width=4.5, height=4) 
train<-ggsurvplot(fit_train,  
                  xlim = c(0, 140),   
                  xlab = 'Time (Months)',   
                  ylab = 'Overall Survival',    
                  pval = TRUE,  
                  pval.size = 3.5,    
                  pval.coord = c(24, 0.1),    
                  pval.method = TRUE, 
                  pval.method.coord = c(0.05, 0.05),  
                  break.time.by = 24,   
                  surv.median.line = "hv",    
                  ggtheme = theme_classic() +   
                    theme(legend.background = element_blank()),  
                  legend.labs = c('Low', 'High'),  
                  legend.title = 'Risk group',  
                  legend = c(0.15, 0.25),  
                  fontsize = 3.5,   
                  risk.table = TRUE,  
                  risk.table.col = 'strata',    
                  tables.height = 0.3,  
                  palette = c("#82B0D2", "#C85D4D"),  
                  fun = 'pct'
                  )  
print(train, newpage = FALSE)  

dev.off() 


fit_validation <- survfit(Surv(time, status) ~ riskgroup, data = risk_test)  
pdf('.//Figure//KM_MBDLM_validation.pdf', width=4.5, height=4) 
validation<-ggsurvplot(fit_validation,  
                       xlim = c(0, 140),  
                       xlab = 'Time (Months)',  
                       ylab = 'Overall Survival', 
                       pval = TRUE,  
                       pval.size = 3.5,  
                       pval.coord = c(24, 0.1), 
                       pval.method = TRUE,  
                       pval.method.coord = c(0.05, 0.05),   
                       break.time.by = 24,  
                       surv.median.line = "hv",  
                       ggtheme = theme_classic() +   
                       theme(legend.background = element_blank()), 
                       legend.labs = c('Low', 'High'),  
                       legend.title = 'Risk group',  
                       legend = c(0.15, 0.25), 
                       fontsize = 3.5, 
                       risk.table = TRUE,  
                       risk.table.col = 'strata',  
                       tables.height = 0.3,
                       palette = c("#82B0D2", "#C85D4D"), 
                       fun = 'pct' 
)  
print(validation, newpage = FALSE)  
dev.off() 


fit_multi <- survfit(Surv(time, status) ~ riskgroup, data = risk_multi)
pdf('.//Figure//KM_MBDLM_mutli.pdf', width=4.5, height=4) 
test<-ggsurvplot(fit_multi,
                 # xlim = c(0, 140),   
                 xlab = 'Time (Months)',  
                 ylab = 'Overall Survival', 
                 pval = TRUE,    
                 pval.size = 3.5,  
                 pval.coord = c(24, 0.1),  
                 pval.method = TRUE, 
                 pval.method.coord = c(0.05, 0.05),  
                 break.time.by = 24,  
                 surv.median.line = "hv",  
                 ggtheme = theme_classic() +   
                   theme(legend.background = element_blank()),  
                 legend.labs = c('Low', 'High'), 
                 legend.title = 'Risk group',  
                 legend = c(0.15, 0.25),    
                 fontsize = 3.5,  
                 risk.table = TRUE,  
                 risk.table.col = 'strata',   
                 tables.height = 0.3, 
                 palette = c("#82B0D2", "#C85D4D"), 
                 fun = 'pct'  # 生成绘图 
)   

print(test, newpage = FALSE)  
dev.off() 
train
validation
test

fit_nac <- survfit(Surv(time, status) ~ riskgroup, data = risk_nac)
pdf('.//Figure//KM_MBDLM_nac.pdf', width=4.5, height=4) 
nac<-ggsurvplot(fit_nac,
                xlim = c(0, 140),  
                xlab = 'Time (Months)',  
                ylab = 'Overall Survival',  
                pval = TRUE,  
                pval.size = 3.5, 
                pval.coord = c(24, 0.1),  
                pval.method = TRUE,  
                pval.method.coord = c(0.05, 0.05),  
                break.time.by = 24, 
                surv.median.line = "hv", 
                ggtheme = theme_classic() +
                  theme(legend.background = element_blank()),  
                legend.labs = c('Low', 'High'), 
                legend.title = 'Risk group',  
                legend = c(0.15, 0.25),    
                fontsize = 3.5,    
                risk.table = TRUE,  
                risk.table.col = 'strata',  
                tables.height = 0.3,  
                palette = c("#82B0D2", "#C85D4D"),  
                fun = 'pct'  
) 
print(nac, newpage = FALSE)
dev.off() 
nac


library(ggplot2)
library(ggpubr)
library(dplyr)  

cutpoint=round(opti_cut[["cutpoint"]]$cutpoint,3)
risk_train <- risk_train %>%  
  arrange(risk) %>%  
  mutate(Samplerisk = row_number())  


p1=ggplot(risk_train,aes(Samplerisk,risk))+
  geom_area(aes(fill=riskgroup))+
  scale_fill_manual(values = c("#82B0D2","#C85D4D"))+
  labs(x="Patient ID(increasing Risk Score)",y="Risk Score")+
  annotate("text",x = sum(risk_train$riskgroup=="low"), y =cutpoint+1,size=5,
           label = paste("cutpoint=",cutpoint))+
  geom_hline(yintercept=cutpoint,
             colour="black", linetype="dashed",size=0.8)+#linetype="dotted"
  geom_vline(xintercept=sum(risk_train$riskgroup=="low"),
             colour="black", linetype="dashed",size=0.8)+
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     limits = c(-10,max(risk_train$risk)+5))+
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)),
                     limits = c(0,max(risk_train$Samplerisk)+1))+
  coord_cartesian(ylim = c(-10, max(risk_train$risk)+0.1)) +
  theme_bw()+
  theme(#panel.grid = element_blank(),
    axis.title = element_text(size=10,color='black'),
    axis.text = element_text(size=10,color='black'),
    legend.title = element_blank(),
    legend.text = element_text(size=12,color='black'),
    legend.background = element_blank(),
    legend.position = c(0.1,0.9))+
  guides(fill=guide_legend(ncol = 1,override.aes = list(size = 5),reverse = T))
p1


risk_train$status=ifelse(risk_train$status=="1","death","alive")

p2=ggplot(risk_train,aes(x=Samplerisk,y=time))+
  geom_point(aes(fill=status,shape=status),color = "white", size = 3, stroke =0)+
  scale_fill_manual(values = c("#00b5b5","#ff7469"))+
  scale_shape_manual(values = c(22,21))+
  labs(x="Patient ID(increasing risk score)",y="Survival time(year)")+
  geom_vline(xintercept=sum(risk_train$riskgroup=="low"),
             colour="black", linetype="dashed",size=0.8)+
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)),
                     limits = c(0,max(risk_train$Samplerisk)+1))+
  theme_bw()+
  theme(#panel.grid = element_blank(),
    axis.title = element_text(size=10,color='black'),
    axis.text = element_text(size=10,color='black'),
    legend.title = element_blank(),
    legend.text = element_text(size=12,color='black'),
    legend.background = element_blank(),
    legend.position = c(0.92,0.9))+
  guides(fill=guide_legend(ncol = 1,override.aes = list(size = 5)))
p2


risk_test <- risk_test %>%  
  arrange(risk) %>%  
  mutate(Samplerisk = row_number())  

p3=ggplot(risk_test,aes(Samplerisk,risk))+
  geom_area(aes(fill=riskgroup))+
  scale_fill_manual(values = c("#82B0D2","#C85D4D"))+
  labs(x="Patient ID(increasing Risk Score)",y="Risk Score")+
  annotate("text",x = sum(risk_test$riskgroup=="low"), y =cutpoint+0.5,size=5,
           label = paste("cutpoint=",cutpoint))+
  geom_hline(yintercept=cutpoint,
             colour="black", linetype="dashed",size=0.8)+#linetype="dotted"
  geom_vline(xintercept=sum(risk_test$riskgroup=="low"),
             colour="black", linetype="dashed",size=0.8)+
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     limits = c(-10,max(risk_test$risk)+0.1))+
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)),
                     limits = c(0,max(risk_test$Samplerisk)+1))+
  coord_cartesian(ylim = c(-10, max(risk_test$risk)+0.1)) + 
  theme_bw()+
  theme(#panel.grid = element_blank(),
    axis.title = element_text(size=10,color='black'),
    axis.text = element_text(size=10,color='black'),
    legend.title = element_blank(),
    legend.text = element_text(size=12,color='black'),
    legend.background = element_blank(),
    legend.position = c(0.1,0.9))+
  guides(fill=guide_legend(ncol = 1,override.aes = list(size = 5),reverse = T))
p3


risk_test$status=ifelse(risk_test$status=="1","death","alive")

p4=ggplot(risk_test,aes(x=Samplerisk,y=time))+
  geom_point(aes(fill=status,shape=status),color = "white", size = 3, stroke =0)+
  scale_fill_manual(values = c("#00b5b5","#ff7469"))+
  scale_shape_manual(values = c(22,21))+
  labs(x="Patient ID(increasing risk score)",y="Survival time(year)")+
  geom_vline(xintercept=sum(risk_test$riskgroup=="low"),
             colour="black", linetype="dashed",size=0.8)+
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)),
                     limits = c(0,max(risk_test$Samplerisk)+1))+
  theme_bw()+
  theme(#panel.grid = element_blank(),
    axis.title = element_text(size=10,color='black'),
    axis.text = element_text(size=10,color='black'),
    legend.title = element_blank(),
    legend.text = element_text(size=12,color='black'),
    legend.background = element_blank(),
    legend.position = c(0.92,0.9))+
  guides(fill=guide_legend(ncol = 1,override.aes = list(size = 5)))
p4


risk_multi <- risk_multi %>%
  arrange(risk) %>%
  mutate(Samplerisk = row_number())

p5=ggplot(risk_multi,aes(Samplerisk,risk))+
  geom_area(aes(fill=riskgroup))+
  scale_fill_manual(values = c("#82B0D2","#C85D4D"))+
  labs(x="Patient ID(increasing Risk Score)",y="Risk Score")+
  annotate("text",x = sum(risk_multi$riskgroup=="low"), y =cutpoint+0.5,size=5,
           label = paste("cutpoint=",cutpoint))+
  geom_hline(yintercept=cutpoint,
             colour="black", linetype="dashed",size=0.8)+#linetype="dotted"
  geom_vline(xintercept=sum(risk_multi$riskgroup=="low"),
             colour="black", linetype="dashed",size=0.8)+
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     limits = c(-10,max(risk_multi$risk)+0.1))+
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)),
                     limits = c(0,max(risk_multi$Samplerisk)+1))+
  coord_cartesian(ylim = c(-10, max(risk_multi$risk)+0.1)) + 
  theme_bw()+
  theme(#panel.grid = element_blank(),
    axis.title = element_text(size=10,color='black'),
    axis.text = element_text(size=10,color='black'),
    legend.title = element_blank(),
    legend.text = element_text(size=12,color='black'),
    legend.background = element_blank(),
    legend.position = c(0.05,0.9))+
  guides(fill=guide_legend(ncol = 1,override.aes = list(size = 5),reverse = T))
p5


risk_multi$status=ifelse(risk_multi$status=="1","death","alive")

p6=ggplot(risk_multi,aes(x=Samplerisk,y=time))+
  geom_point(aes(fill=status,shape=status),color = "white", size = 3, stroke =0)+
  scale_fill_manual(values = c("#00b5b5","#ff7469"))+
  scale_shape_manual(values = c(22,21))+
  labs(x="Patient ID(increasing risk score)",y="Survival time(year)")+
  geom_vline(xintercept=sum(risk_multi$riskgroup=="low"),
             colour="black", linetype="dashed",size=0.8)+
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)),
                     limits = c(0,max(risk_multi$Samplerisk)+1))+
  theme_bw()+
  theme(#panel.grid = element_blank(),
    axis.title = element_text(size=10,color='black'),
    axis.text = element_text(size=10,color='black'),
    legend.title = element_blank(),
    legend.text = element_text(size=12,color='black'),
    legend.background = element_blank(),
    legend.position = c(0.92,0.9))+
  guides(fill=guide_legend(ncol = 1,override.aes = list(size = 5)))
p6

library(patchwork)
trainp<-p1 / p2
ggsave(".//Figure//RISK_train.pdf", plot = trainp, width = 8.5, height = 6)  
testp<-p3 / p4
ggsave(".//Figure//RISK_validaitiont.pdf", plot = testp, width = 8.5, height = 6)  
multip<-p5 / p6
ggsave(".//Figure//RISK_multi.pdf", plot = multip, width = 8.5, height = 6) 
