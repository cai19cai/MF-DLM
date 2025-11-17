rm(list=ls())  
library(dplyr)
 
setwd("")   
dt_risk <- read.csv(".csv") 
dt_risk <-dt_risk[,-4]
#dt_risk <- read.csv(".csv")  
clinical<-read.csv(".csv")  

clinical <- na.omit(clinical)
clinical <- clinical[complete.cases(clinical), ]

data_list2 <- list(dt_risk, clinical)
dt <- Reduce(function(x, y) merge(x, y, by = "ID"), data_list2)  
library(dplyr)
dt <- dt %>%  
  mutate(riskgroup = ifelse(riskgroup == "high", "1", "0"))  
dt <- dt %>%
  mutate(status = ifelse(status == "death", "1", "0"))

library(pcutils)
library(survival)  
library(dplyr)  
library(survminer)  
library(ggplot2)
library(tidyverse)
library(cowplot)

# 通用主题设置
common_theme <- function() {
  theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      axis.text.x = element_text(size = 9, color = "black"),
      axis.text.y = element_text(size = 8, color = "black"),
      panel.background = element_rect(fill = "#f3f4f4"),
      legend.position = "right",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      plot.margin = unit(c(5, 5, 5, 5), "pt")
    )
}

# 创建统一的颜色方案
# color_palettes <- list(
#   TSR = c("#f6ddb4", "#f6a34a"),
#   TB = c("#A3C1DA", "#6A9BC3", "#276C9E"),
#   TISs = c("#FFB2B2", "#FF8C8C", "#FF4D4D", "#A52A2A"),
#   Spreading = c("#D7B2E8", "#7C4B92"),
#   Growth = c("#B2D6A7", "#5DB15D", "#329845"),
#   TIL = c("#E6B8A2", "#DEA792", "#D69682", "#C76D5D")
# )
color_palettes <- list(
  TSR = c("#E6F0FA", "#B8D4F0"),  # 极浅蓝到浅天蓝（起始状态）
  TB = c("#A8C8E6", "#7AAED9", "#4D94CC"),  # 标准蓝三阶（中等对比）
  TISs = c("#9BB5DD", "#6E96D1", "#3A75C2", "#1A4B8C"),  # 深蓝四阶（含海军蓝强调）
  Spreading = c("#C6D8EB", "#8FB3DB"),  # 灰蓝到中蓝双色
  Growth = c("#B5D0E7", "#7EB0D9", "#3C8BC2"),  # 鲜亮蓝三阶（生长活力感）
  TIL = c("#D1E0F0", "#A8C5E6", "#7EA9DB", "#4E89C7")  # 粉调蓝四阶（免疫特殊色）
)
# 统一的P值标注位置
p_value_annotation <- function(p_val) {
  annotate("text", x = 1.5, y = 1.02, 
           label = ifelse(p_val < 0.001, 
                          "*** P<0.001", 
                          sprintf("P=%.3f", p_val)),
           size = 3, vjust = 0)
}

# 1. TSR图
dt1 <- dt[c(4, 5)]
dt1_table <- table(dt1$riskgroup, dt1$TSR)
dt1_chisq <- chisq.test(dt1_table)
p1 <- dt1 %>%
  filter(!is.na(riskgroup) | !is.na(TSR)) %>%
  count(riskgroup, TSR) %>%
  ggplot() +  
  geom_bar(aes(x = factor(riskgroup), y = n, fill = factor(TSR)), 
           color = "white", position = "fill", stat = "identity", width = 0.95) +  
  scale_fill_manual(values = color_palettes$TSR, labels = c("Low", "High")) +
  scale_x_discrete(labels = c("Low risk", "High risk")) +
  p_value_annotation(dt1_chisq$p.value) +
  ggtitle("TSR") +  
  xlab("") + ylab("Proportion") +
  common_theme() +
  labs(fill = "TSR")
p1
# 2. Tumor budding图
dt2 <- dt[c(4, 6)]
dt2_table <- table(dt2$riskgroup, dt2$TB)
dt2_chisq <- chisq.test(dt2_table)
p2 <- dt2 %>%
  filter(!is.na(riskgroup) | !is.na(TB)) %>%
  count(riskgroup, TB) %>%
  ggplot() +  
  geom_bar(aes(x = factor(riskgroup), y = n, fill = factor(TB)), 
           color = "white", position = "fill", stat = "identity", width = 0.95) +  
  scale_fill_manual(values = color_palettes$TB, labels = c("Low", "Intermediate", "High")) +
  scale_x_discrete(labels = c("Low risk", "High risk")) +
  p_value_annotation(dt2_chisq$p.value) +
  ggtitle("Tumor budding") +  
  xlab("") + ylab("") +
  common_theme() +
  labs(fill = "Tumor\nbudding")

# 3. TISs图
dt3 <- dt[c(4, 10)]
dt3_table <- table(dt3$riskgroup, dt3$TISs)
dt3_chisq <- chisq.test(dt3_table)
p3 <- dt3 %>%
  filter(!is.na(riskgroup) | !is.na(TISs)) %>%
  count(riskgroup, TISs) %>%
  ggplot() +  
  geom_bar(aes(x = factor(riskgroup), y = n, fill = factor(TISs)), 
           color = "white", position = "fill", stat = "identity", width = 0.95) +  
  scale_fill_manual(values = color_palettes$TISs, 
                    labels = c("No", "Aggregates", "Primary TLSs", "Secondary TLSs")) +
  scale_x_discrete(labels = c("Low risk", "High risk")) +
  p_value_annotation(dt3_chisq$p.value) +
  ggtitle("TISs") +  
  xlab("") + ylab("") +
  common_theme() +
  labs(fill = "TISs")

# 4. Spreading pattern图
dt4 <- dt[c(4, 8)]
dt4_table <- table(dt4$riskgroup, dt4$Spreading)
dt4_chisq <- chisq.test(dt4_table)
p4 <- dt4 %>%
  filter(!is.na(riskgroup) | !is.na(Spreading)) %>%
  count(riskgroup, Spreading) %>%
  ggplot() +  
  geom_bar(aes(x = factor(riskgroup), y = n, fill = factor(Spreading)), 
           color = "white", position = "fill", stat = "identity", width = 0.95) +  
  scale_fill_manual(values = color_palettes$Spreading, 
                    labels = c("Compact", "Disseminating")) +
  scale_x_discrete(labels = c("Low risk", "High risk")) +
  p_value_annotation(dt4_chisq$p.value) +
  ggtitle("Spreading pattern") +  
  xlab("") + ylab("Proportion") +
  common_theme() +
  labs(fill = "Spreading\npattern")

# 5. Growth pattern图
dt5 <- dt[c(4, 9)]
dt5_table <- table(dt5$riskgroup, dt5$Growth)
dt5_chisq <- chisq.test(dt5_table)
p5 <- dt5 %>%
  filter(!is.na(riskgroup) | !is.na(Growth)) %>%
  count(riskgroup, Growth) %>%
  ggplot() +  
  geom_bar(aes(x = factor(riskgroup), y = n, fill = factor(Growth)), 
           color = "white", position = "fill", stat = "identity", width = 0.95) +  
  scale_fill_manual(values = color_palettes$Growth, 
                    labels = c("Cohesive", "Trabecular/nested", "Spindle/single")) +
  scale_x_discrete(labels = c("Low risk", "High risk")) +
  p_value_annotation(dt5_chisq$p.value) +
  ggtitle("Growth pattern") +  
  xlab("") + ylab("") +
  common_theme() +
  labs(fill = "Growth\npattern")

# 6. TIL图
dt6 <- dt[c(4, 7)]
dt6_table <- table(dt6$riskgroup, dt6$TIL)
dt6_chisq <- chisq.test(dt6_table)
p6 <- dt6 %>%
  filter(!is.na(riskgroup) | !is.na(TIL)) %>%
  count(riskgroup, TIL) %>%
  ggplot() +  
  geom_bar(aes(x = factor(riskgroup), y = n, fill = factor(TIL)), 
           color = "white", position = "fill", stat = "identity", width = 0.95) +  
  scale_fill_manual(values = color_palettes$TIL, 
                    labels = c("0", "1", "2", "3")) +
  scale_x_discrete(labels = c("Low risk", "High risk")) +
  p_value_annotation(dt6_chisq$p.value) +
  ggtitle("TIL") +  
  xlab("") + ylab("") +
  common_theme() +
  labs(fill = "TIL")


##拼图###########################################################################
library(cowplot)

p1a <- p1 + theme(legend.position = "none")
p2a <- p2 + theme(legend.position = "none")
p3a <- p3 + theme(legend.position = "none")
p4a <- p4 + theme(legend.position = "none")
p5a <- p5 + theme(legend.position = "none")
p6a <- p6 + theme(legend.position = "none")


# 先合并无图例组：
p <- plot_grid(p1a,p2a, p3a,p4a,p5a,p6a,nrow = 2)
p

# 提取图例：
legend1 <- get_legend(p1)
legend2 <- get_legend(p2)
legend3 <- get_legend(p3)
legend4 <- get_legend(p4)
legend5 <- get_legend(p5)
legend6 <- get_legend(p6)
p_new <- plot_grid(p, plot_grid(legend1,legend2,legend3,legend4,legend5, legend6,ncol = 1,align = "h"),
                   rel_widths = c(2, 0.6))
p_new
ggsave("特殊病理特征条形图.pdf", plot = p_new , height =6, width =8)




