
rm(list=ls())   
setwd("")   
 
predprobtrain <- read.csv("train_pred_deephit.csv")  
predprobtest <- read.csv("test_pred_deephit.csv")  
predprobmulti <- read.csv("multi_pred_deephit.csv")
predprobnac <- read.csv("nac_pred_deephit.csv")
 
library(survival)  
library(dplyr)  
library(survminer)  

 
opti_cut <- surv_cutpoint(predprobtrain,   
                          time = "time",   
                          event = "status",   
                          variables = c("risk")) 
# pdf('opti_cut.pdf',width =10,height = 10)
# plot(opti_cut, "risk", palette = "npg")
# dev.off()  
summary(opti_cut)  

group_cut_train <- surv_categorize(opti_cut)  
colnames(group_cut_train)[3] <- "riskgroup"  
risk_train <-cbind(predprobtrain,group_cut_train["riskgroup"])
write.csv(risk_train,"risk_train.csv",row.names = F)

predprobtest$riskgroup <- ifelse(predprobtest$risk > 8.00552    , "high", "low")
risk_test<-predprobtest
write.csv(predprobtest,"risk_validation.csv",row.names = F)

predprobmulti$riskgroup <- ifelse(predprobmulti$risk > 8.00552    , "high", "low")
risk_multi<-predprobmulti
write.csv(predprobmulti,"risk_mutli.csv",row.names = F)

predprobnac$riskgroup <- ifelse(predprobnac$risk > 8.00552    , "high", "low")
risk_nac<-predprobnac
write.csv(predprobnac,"risk_nac.csv",row.names = F)


dt <- rbind(risk_train, risk_test, risk_multi)  
#dt <- rbind(dt_test, dt_multi)  
dt2 <- rbind( risk_test, risk_multi)  

write.csv(dt,"FB_score.csv",row.names = F)
write.csv(dt2,"FB_score_notrain.csv",row.names = F)

risk_train$riskgroup <- factor(risk_train$riskgroup, levels = c('low', 'high'))  
risk_test$riskgroup <- factor(risk_test$riskgroup, levels = c('low', 'high'))  
risk_multi$riskgroup <- factor(risk_multi$riskgroup, levels = c('low', 'high'))  
risk_nac$riskgroup <- factor(risk_nac$riskgroup, levels = c('low', 'high'))  
 
fit_train <- survfit(Surv(time, status) ~ riskgroup, data = risk_train)  
pdf('KM_deephit_train.pdf', width=6.5, height=6) 
train<-ggsurvplot(fit_train,  
                  xlim = c(0, 140),   
                  xlab = 'Time (Months)',  
                  ylab = 'Overall Survival',  
                  pval = TRUE,   
                  pval.size = 3.5,   
                  pval.coord = c(10, 0.1),   
                  pval.method = TRUE,   
                  pval.method.coord = c(0.05, 0.05), 
                  break.time.by = 24,  
                  surv.median.line = "hv",  
                  ggtheme = theme_classic() +   
                    theme(legend.background = element_blank()),    
                  legend.labs = c('Low risk', 'High risk'),    
                  legend.title = 'Risk group',  
                  legend = c(0.15, 0.25),    
                  fontsize = 2.8,   
                  risk.table = 'abs_pct', 
                  risk.table.col = 'strata', 
                  tables.height = 0.23,  
                  palette = c("#82B0D2", "#C85D4D"), 
                  fun = 'pct' 
                  )   
  
print(train, newpage = FALSE)  

dev.off()

# 绘制测试集生存曲线  
fit_validation <- survfit(Surv(time, status) ~ riskgroup, data = risk_test)  
pdf('KM_deephit_validation.pdf', width=6.5, height=6) # 新建一张pdf  
validation<-ggsurvplot(fit_validation,  
                       xlim = c(0, 140),  # X轴范围  
                       xlab = 'Time (Months)',  # X轴标签  
                       ylab = 'Overall Survival',  # y轴标签  
                       pval = TRUE,  # 添加P值  
                       pval.size = 3.5,  # P值展示大小  
                       pval.coord = c(10, 0.1),  # P值位置，调整P值Y坐标，避免遮挡  
                       pval.method = TRUE,  # 添加P值检验方法  
                       pval.method.coord = c(0.05, 0.05),  # 检验方法位置  
                       break.time.by = 24,  # X轴按照24进行切分  
                       surv.median.line = "hv",  # 增加中位生存时间  
                       ggtheme = theme_classic() +   
                         theme(legend.background = element_blank()),  # 设置ggplot主题，图例背景透明  
                       legend.labs = c('Low risk', 'High risk'),  # 改变图例标签顺序  
                       legend.title = 'Risk group',  
                       legend = c(0.15, 0.25),  # 指定图例位置  
                       fontsize = 2.8,  # 风险表字体大小  
                       risk.table = 'abs_pct',  # 在下面添加风险表  
                       risk.table.col = 'strata',  # 风险表颜色  
                       tables.height = 0.23,  # 风险表高度占比  
                       palette = c("#82B0D2", "#C85D4D"),  # 颜色，确保低风险为蓝色，高风险为红色  
                       fun = 'pct'  # 生成绘图 
)  # 作图  
# 打印绘图  
print(validation, newpage = FALSE)  
dev.off() # 关闭pdf



# 绘制测试集生存曲线
fit_multi <- survfit(Surv(time, status) ~ riskgroup, data = risk_multi)
pdf('KM_deephit_mutli.pdf', width=6.5, height=6) # 新建一张pdf
test<-ggsurvplot(fit_multi,
                 xlim = c(0, 140),  # X轴范围  
                 xlab = 'Time (Months)',  # X轴标签  
                 ylab = 'Overall Survival',  # y轴标签  
                 pval = TRUE,  # 添加P值  
                 pval.size = 3.5,  # P值展示大小  
                 pval.coord = c(10, 0.1),  # P值位置，调整P值Y坐标，避免遮挡  
                 pval.method = TRUE,  # 添加P值检验方法  
                 pval.method.coord = c(0.05, 0.05),  # 检验方法位置  
                 break.time.by = 24,  # X轴按照24进行切分  
                 surv.median.line = "hv",  # 增加中位生存时间  
                 ggtheme = theme_classic() +   
                   theme(legend.background = element_blank()),  # 设置ggplot主题，图例背景透明  
                 legend.labs = c('Low risk', 'High risk'),  # 改变图例标签顺序  
                 legend.title = 'Risk group',  
                 legend = c(0.15, 0.25),  # 指定图例位置  
                 fontsize = 2.8,  # 风险表字体大小  
                 risk.table = 'abs_pct',  # 在下面添加风险表  
                 risk.table.col = 'strata',  # 风险表颜色  
                 tables.height = 0.23,  # 风险表高度占比  
                 palette = c("#82B0D2", "#C85D4D"),  # 颜色，确保低风险为蓝色，高风险为红色  
                 fun = 'pct'  # 生成绘图 
)  # 作图  
# 打印绘图  
print(test, newpage = FALSE)  
dev.off() # 关闭pdf
# 
# 
# # 绘制特殊测试集生存曲线  
fit_nac <- survfit(Surv(time, status) ~ riskgroup, data = risk_nac)
pdf('KM_deephit_nac.pdf', width=8, height=8) # 新建一张pdf
nac<-ggsurvplot(fit_nac,
                xlim = c(0, 140),  # X轴范围  
                xlab = 'Time (Months)',  # X轴标签  
                ylab = 'Overall Survival',  # y轴标签  
                pval = TRUE,  # 添加P值  
                pval.size = 3.5,  # P值展示大小  
                pval.coord = c(10, 0.1),  # P值位置，调整P值Y坐标，避免遮挡  
                pval.method = TRUE,  # 添加P值检验方法  
                pval.method.coord = c(0.05, 0.05),  # 检验方法位置  
                break.time.by = 24,  # X轴按照24进行切分  
                surv.median.line = "hv",  # 增加中位生存时间  
                ggtheme = theme_classic() +   
                  theme(legend.background = element_blank()),  # 设置ggplot主题，图例背景透明  
                legend.labs = c('Low risk', 'High risk'),  # 改变图例标签顺序  
                legend.title = 'Risk group',  
                legend = c(0.15, 0.25),  # 指定图例位置  
                fontsize = 2.8,  # 风险表字体大小  
                risk.table = 'abs_pct',  # 在下面添加风险表  
                risk.table.col = 'strata',  # 风险表颜色  
                tables.height = 0.23,  # 风险表高度占比  
                palette = c("#82B0D2", "#C85D4D"),  # 颜色，确保低风险为蓝色，高风险为红色  
                fun = 'pct'  # 生成绘图 
)  # 作图  
# 打印绘图  
print(nac, newpage = FALSE)  
dev.off() # 关闭pdf



library(ggplot2)
library(tidyverse)
library(ggpubr)
##########绘制风险评分图
cutpoint=round(opti_cut[["cutpoint"]]$cutpoint,3)

library(dplyr)  
# 假设 risk_train 数据框中有一列名为 risk  
risk_train <- risk_train %>%  
  arrange(risk) %>%  
  mutate(Samplerisk = row_number())  


p1=ggplot(risk_train,aes(Samplerisk,risk))+
  geom_area(aes(fill=riskgroup))+
  scale_fill_manual(values = c("#82B0D2","#C85D4D"))+
  labs(x="Patient ID(increasing Risk Score)",y="Risk Score")+
  annotate("text",x = sum(risk_train$riskgroup=="low"), y =cutpoint+0.5,size=5,
           label = paste("cutpoint=",cutpoint))+
  geom_hline(yintercept=cutpoint,
             colour="black", linetype="dashed",size=0.8)+#linetype="dotted"
  geom_vline(xintercept=sum(risk_train$riskgroup=="low"),
             colour="black", linetype="dashed",size=0.8)+
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     limits = c(0,max(risk_train$risk)+5))+
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)),
                     limits = c(0,max(risk_train$Samplerisk)+1))+
  coord_cartesian(ylim = c(7, max(risk_train$risk)+0.1)) +  # 设置Y轴可视范围为4到12  
  theme_bw()+
  theme(#panel.grid = element_blank(),
    axis.title = element_text(size=10,color='black'),
    axis.text = element_text(size=10,color='black'),
    legend.title = element_blank(),
    legend.text = element_text(size=12,color='black'),
    legend.background = element_blank(),
    legend.position = c(0.05,0.9))+
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


# 假设 risk_train 数据框中有一列名为 risk  
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
                     limits = c(0,max(risk_test$risk)+0.1))+
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)),
                     limits = c(0,max(risk_test$Samplerisk)+1))+
  coord_cartesian(ylim = c(7, max(risk_test$risk)+0.1)) +  # 设置Y轴可视范围为4到12  
  theme_bw()+
  theme(#panel.grid = element_blank(),
    axis.title = element_text(size=10,color='black'),
    axis.text = element_text(size=10,color='black'),
    legend.title = element_blank(),
    legend.text = element_text(size=12,color='black'),
    legend.background = element_blank(),
    legend.position = c(0.05,0.9))+
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

# 假设 risk_train 数据框中有一列名为 risk
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
                     limits = c(0,max(risk_multi$risk)+0.1))+
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)),
                     limits = c(0,max(risk_multi$Samplerisk)+1))+
  coord_cartesian(ylim = c(7, max(risk_multi$risk)+0.1)) +  # 设置Y轴可视范围为4到12  
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
ggsave("RISK_train.pdf", plot = trainp, width = 8.5, height = 6)  
testp<-p3 / p4
ggsave("RISK_validaitiont.pdf", plot = testp, width = 8.5, height = 6)  
multip<-p5 / p6
ggsave("RISK_multi.pdf", plot = multip, width = 8.5, height = 6) 
