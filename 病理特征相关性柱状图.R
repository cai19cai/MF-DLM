# 清空当前工作环境  
rm(list=ls())  
library(dplyr)

# 设置工作目录  
setwd("")   
dt_risk <- read.csv("") 
dt_risk <-dt_risk[,-4]
#dt_risk <- read.csv("")  
clinical<-read.csv("")  
clinical<-clinical[, c(1,22:28)]

data_list2 <- list(dt_risk, clinical)
dt <- Reduce(function(x, y) merge(x, y, by = "ID"), data_list2)  
#write.csv(dt_p,"dt_病理特征.csv",row.names = F)
library(dplyr)
dt <- dt %>%  
  mutate(riskgroup = ifelse(riskgroup == "high", "1", "0"))  
dt <- dt %>%
  mutate(status = ifelse(status == "death", "1", "0"))

# 将第 3 到 13 行的所有列转换为因子  
# dt_p <- dt_p %>%  
#   mutate(across(c(3,11:18,20), as.factor))


#install.packages("devtools")
#devtools::install_github('Asa12138/pcutils',dependencies=T)

library(pcutils)
library(survival)  
library(dplyr)  
library(survminer)  
library(ggplot2)
library(tidyverse)


colnames(dt)
dt1<-dt[c(4,8)]
# 创建列联表  
dt1_table <- table(dt1$riskgroup, dt1$VUC)  
# 进行卡方检验  
dt1_chisq <- chisq.test(dt1_table)  
# 输出 P 值  
p_value <- dt1_chisq$p.value  
p_value
dt1_count <- dt1 %>%  
  filter(!is.na(riskgroup) | !is.na(VUC)) %>%  
  group_by(riskgroup, VUC) %>%  
  summarise(count = n(), .groups = 'drop') %>%  
  arrange(riskgroup, VUC)  # 可选：按照feature和feature2排序  

p1 <- ggplot(dt1_count) +  
  geom_bar(aes(x = factor(riskgroup), y = count, fill = factor(VUC)),   
           color = "#f3f4f4", position = "fill", stat = "identity", size =  0.3) +  
  # 修改填充颜色：  
  scale_fill_manual(values = c( "#f6ddb4","#f6a34a"),  
                    # 图例标签：  
                    labels = c("No", "Yes")) +  
  # 添加星号注释：  
  annotate("text", x = 2, y = 0.85, label = " ", size = 5) +  
  annotate("text", x = 1.5, y = 1.05, label = expression("***" ~ italic("P<0.001")), size = 3.5) +  
  # 标题和副标题：  
  # ggtitle("VUC", subtitle = "(Risk group)") + 
  ggtitle("VUC") +  
  scale_x_discrete(labels = c("Low risk", "Higs risk")) +  
  # 添加x轴和y轴标签  
  xlab("") +  
  ylab("") +  
  # 设置主题：  
  theme_bw() +  
  theme(panel.grid = element_blank(),  
        # 标题和副标题居中  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),  
        # 修改背景色：  
        panel.background = element_rect(fill = "#f3f4f4")) +  
  # 调整图例顺序：  
  guides(fill = guide_legend(reverse = TRUE)) +  
  # 图例标题：  
  labs(fill = "VUC")  

# 显示图形  
print(p1)  



dt2<-dt[c(4,9)]
# 创建列联表  
dt2_table <- table(dt2$riskgroup, dt2$Pathological_garde)  
# 进行卡方检验  
dt2_chisq <- chisq.test(dt2_table)  
# 输出 P 值  
p_value <- dt2_chisq$p.value  
p_value
dt2_count <- dt2 %>%  
  filter(!is.na(riskgroup) | !is.na(Pathological_garde)) %>%  
  group_by(riskgroup, Pathological_garde) %>%  
  summarise(count = n(), .groups = 'drop') %>%  
  arrange(riskgroup, Pathological_garde)  # 可选：按照feature和feature2排序  
p2 <- ggplot(dt2_count) +  
  geom_bar(aes(x = factor(riskgroup), y = count, fill = factor(Pathological_garde)),   
           color = "#f3f4f4", position = "fill", stat = "identity", size =  0.3) +  
  # 修改填充颜色：  
  scale_fill_manual(values = c( "#A3C1DA","#276C9E" ),  
                    # 图例标签：  
                    labels = c("Low grade", "High grade")) +  
  # 添加星号注释：  
  annotate("text", x = 2, y = 0.85, label = " ", size = 5) +  
  annotate("text", x = 1.5, y = 1.05, label = expression("***" ~ italic("P<0.001")), size = 3.5) +  
  # 标题和副标题：  
  # ggtitle("Pathological_garde", subtitle = "(Risk group)") + 
  ggtitle("Pathological garde") +  
  scale_x_discrete(labels = c("Low risk", "Higs risk")) +  
  # 添加x轴和y轴标签  
  xlab("") +  
  ylab("") +  
  # 设置主题：  
  theme_bw() +  
  theme(panel.grid = element_blank(),  
        # 标题和副标题居中  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),  
        # 修改背景色：  
        panel.background = element_rect(fill = "#f3f4f4")) +  
  # 调整图例顺序：  
  guides(fill = guide_legend(reverse = TRUE)) +  
  # 图例标题：  
  labs(fill = "Pathological garde")  

# 显示图形  
print(p2)  

dt3<-dt[c(4,5)]
# 创建列联表  
dt3_table <- table(dt3$riskgroup, dt3$pT_stage)  
# 进行卡方检验  
dt3_chisq <- chisq.test(dt3_table)  
# 输出 P 值  
p_value <- dt3_chisq$p.value  
p_value
dt3_count <- dt3 %>%  
  filter(!is.na(riskgroup) | !is.na(pT_stage)) %>%  
  group_by(riskgroup, pT_stage) %>%  
  summarise(count = n(), .groups = 'drop') %>%  
  arrange(riskgroup, pT_stage)  # 可选：按照feature和feature2排序  

p3 <- ggplot(dt3_count) +  
  geom_bar(aes(x = factor(riskgroup), y = count, fill = factor(pT_stage)),   
           color = "#f3f4f4", position = "fill", stat = "identity", size = 0.3) +  
  # 修改填充颜色：  
  scale_fill_manual(values = c("#FFB2B2", "#FF8C8C", "#FF4D4D", "#C72D2D", "#A52A2A" ),  
                    labels = c("Ta", "T1", "T2", "T3", "T4")) +  
  # 添加星号注释：  
  annotate("text", x = 2, y = 0.85, label = " ", size = 5) +  
  annotate("text", x = 1.5, y = 1.05, label = expression("***" ~ italic("P<0.001")), size = 3.5) +  
  # 标题和副标题：  
  # ggtitle("pT_stage", subtitle = "(Risk group)") + 
  ggtitle("pT stage") +  
  scale_x_discrete(labels = c("Low risk", "Higs risk")) +  
  # 添加x轴和y轴标签  
  xlab("") +  
  ylab("") +  
  # 设置主题：  
  theme_bw() +  
  theme(panel.grid = element_blank(),  
        # 标题和副标题居中  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),  
        # 修改背景色：  
        panel.background = element_rect(fill = "#f3f4f4")) +  
  # 调整图例顺序：  
  guides(fill = guide_legend(reverse = TRUE)) +  
  # 图例标题：  
  labs(fill = "pT stage")  

# 显示图形  
print(p3)  


dt4<-dt[c(4,7)]
# 创建列联表  
dt4_table <- table(dt4$riskgroup, dt4$pN_stage)  
# 进行卡方检验  
dt4_chisq <- chisq.test(dt4_table)  
# 输出 P 值  
p_value <- dt4_chisq$p.value  
p_value
dt4_count <- dt4 %>%  
  filter(!is.na(riskgroup) | !is.na(pN_stage)) %>%  
  group_by(riskgroup, pN_stage) %>%  
  summarise(count = n(), .groups = 'drop') %>%  
  arrange(riskgroup, pN_stage)  # 可选：按照feature和feature2排序  

p4 <- ggplot(dt4_count) +  
  geom_bar(aes(x = factor(riskgroup), y = count, fill = factor(pN_stage)),   
           color = "#f3f4f4", position = "fill", stat = "identity", size = 0.3) +  
  # 修改填充颜色：  
  scale_fill_manual(values = c( "#D7B2E8","#7C4B92" ),  
                    labels = c("No", "Yes")) +  
  # 添加星号注释：  
  annotate("text", x = 2, y = 0.85, label = " ", size = 5) +  
  annotate("text", x = 1.5, y = 1.05, label = expression("***" ~ italic("P<0.001")), size = 3.5) +  
  # 标题和副标题：  
  # ggtitle("pN.stage", subtitle = "(Risk group)") + 
  ggtitle("pN stage") +  
  scale_x_discrete(labels = c("Low risk", "Higs risk")) +  
  # 添加x轴和y轴标签  
  xlab("") +  
  ylab("") +  
  # 设置主题：  
  theme_bw() +  
  theme(panel.grid = element_blank(),  
        # 标题和副标题居中  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),  
        # 修改背景色：  
        panel.background = element_rect(fill = "#f3f4f4")) +  
  # 调整图例顺序：  
  guides(fill = guide_legend(reverse = TRUE)) +  
  # 图例标题：  
  labs(fill = "pN stage")  

# 显示图形  
print(p4)  

dt5<-dt[c(4,11)]
# 创建列联表  
dt5_table <- table(dt5$riskgroup, dt5$Lymphovascular_invasion)  
# 进行卡方检验  
dt5_chisq <- chisq.test(dt5_table)  
# 输出 P 值  
p_value <- dt5_chisq$p.value  
p_value
dt5_count <- dt5 %>%  
  filter(!is.na(riskgroup) | !is.na(Lymphovascular_invasion)) %>%  
  group_by(riskgroup, Lymphovascular_invasion) %>%  
  summarise(count = n(), .groups = 'drop') %>%  
  arrange(riskgroup, Lymphovascular_invasion)  # 可选：按照feature和feature2排序  

p5 <- ggplot(dt5_count) +  
  geom_bar(aes(x = factor(riskgroup), y = count, fill = factor(Lymphovascular_invasion)),   
           color = "#f3f4f4", position = "fill", stat = "identity", size = 0.3) +  
  # 修改填充颜色：  
  scale_fill_manual(values = c(  "#B2D6A7","#329845"),  
                    labels = c("No", "Yes")) +  
  # 添加星号注释：  
  annotate("text", x = 2, y = 0.85, label = " ", size = 5) +  
  annotate("text", x = 1.5, y = 1.05, label = expression("***" ~ italic("P<0.001")), size = 3.5) +  
  # 标题和副标题：  
  # ggtitle("Lymphovascular.invasion", subtitle = "(Risk group)") + 
  ggtitle("Lymphovascular invasion") +  
  scale_x_discrete(labels = c("Low risk", "Higs risk")) +  
  # 添加x轴和y轴标签  
  xlab("") +  
  ylab("") +  
  # 设置主题：  
  theme_bw() +  
  theme(panel.grid = element_blank(),  
        # 标题和副标题居中  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),  
        # 修改背景色：  
        panel.background = element_rect(fill = "#f3f4f4")) +  
  # 调整图例顺序：  
  guides(fill = guide_legend(reverse = TRUE)) +  
  # 图例标题：  
  labs(fill = "Lymphovascular invasion")  

# 显示图形  
print(p5)  

dt6<-dt[c(4,6)]
# 创建列联表  
dt6_table <- table(dt6$riskgroup, dt6$MIBC)  
# 进行卡方检验  
dt6_chisq <- chisq.test(dt6_table)  
# 输出 P 值  
p_value <- dt6_chisq$p.value  
p_value
dt6_count <- dt6 %>%  
  filter(!is.na(riskgroup) | !is.na(MIBC)) %>%  
  group_by(riskgroup, MIBC) %>%  
  summarise(count = n(), .groups = 'drop') %>%  
  arrange(riskgroup, MIBC)  # 可选：按照feature和feature2排序  
p6 <- ggplot(dt6_count) +  
  geom_bar(aes(x = factor(riskgroup), y = count, fill = factor(MIBC)),   
           color = "#f3f4f4", position = "fill", stat = "identity", size = 0.3) +  
  # 修改填充颜色：  
  scale_fill_manual(values = c(  "#E6B8A2","#C76D5D"),  
                    labels = c("No", "Yes")) +  
  # 添加星号注释：  
  annotate("text", x = 2, y = 0.85, label = " ", size = 5) +  
  annotate("text", x = 1.5, y = 1.05, label = expression("***" ~ italic("P<0.001")), size = 3.5) +  
  # 标题和副标题：  
  # ggtitle("Lymphovascular.invasion", subtitle = "(Risk group)") + 
  ggtitle("MIBC") +  
  scale_x_discrete(labels = c("Low risk", "Higs risk")) +  
  # 添加x轴和y轴标签  
  xlab("") +  
  ylab("") +  
  # 设置主题：  
  theme_bw() +  
  theme(panel.grid = element_blank(),  
        # 标题和副标题居中  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),  
        # 修改背景色：  
        panel.background = element_rect(fill = "#f3f4f4")) +  
  # 调整图例顺序：  
  guides(fill = guide_legend(reverse = TRUE)) +  
  # 图例标题：  
  labs(fill = "MIBC")  

# 显示图形  
print(p6)  


##拼图###########################################################################
library(cowplot)

p1a <- p3 + theme(legend.position = "none")
p2a <- p2 + theme(legend.position = "none")
p3a <- p4 + theme(legend.position = "none")
p4a <- p1 + theme(legend.position = "none")
p5a <- p5 + theme(legend.position = "none")
p6a <- p6 + theme(legend.position = "none")


# 先合并无图例组：
p <- plot_grid(p1a, p6a,p2a, p3a,p4a,p5a,nrow = 1)
p
# 提取图例：
legend1 <- get_legend(p3)
legend6 <- get_legend(p6)
legend2 <- get_legend(p2)
legend3 <- get_legend(p4)
legend4 <- get_legend(p1)
legend5 <- get_legend(p5)
p_new <- plot_grid(p, plot_grid(legend1,legend6,legend2,legend3,legend4, legend5,ncol = 1,align = "v"),
                   rel_widths = c(2, 0.5))
p_new
ggsave("不同危险度的病例特征条形图.pdf", plot = p, height =5.5, width =18)




