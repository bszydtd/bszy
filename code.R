setwd("F:/work") #工作文件地址
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readxl)

data <- read_excel("财务指标2022（全A股）.xlsx")
df <- data.frame(
  stock_code = data$证券代码, # 2800家A股样本
  total_asset = data$总资产/1e8, # 总资产(亿)
  current_ratio = data$流动比率, # 流动比
  net_profit = data$净利润/1e8, # 净利润(亿)
  ROA = data$资产收益率...21, # 资产收益率
  ROE = data$净资产收益率...24, # 净资产收益率
  equity = data$股东权益合计/1e8, # 股东权益
  cfo = data$经营活动产生的现金流量净额/1e8 # 经营现金流(亿)
)
df[1:10,2:8]
# 数据预处理
df <- df %>% 
  mutate(across(c(current_ratio, ROA, ROE), ~ifelse(. < 0, 0, .))) %>% # 修正不合理负值
  drop_na() # 删除缺失值
rownames(df) <- df$stock_code # 证券代码为行名



#全局参数设置（ggplot2图表美化，统一商业图表风格）
theme_set(theme_bw() + theme(
  plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 10),
  legend.position = "bottom",
  legend.title = element_text(size = 11)
))


##-----方法一：描述性统计分析（基础层，数据基本面认知）------------

# ===== 描述性统计核心代码 =====
cat("===== A股2022年8大核心财务指标 描述性统计 =====\n")
# 1. 基础五数概括+均值（最核心）
base_summary <- summary(df[, -1]) # 排除证券代码，纯数值指标
print(base_summary)

# 2. base包计算离散程度指标（标准差、四分位距）
base_sd <- sapply(df[, -1], sd) # 标准差：反映数据波动
base_iqr <- sapply(df[, -1], IQR) # 四分位距：抗极值，反映核心数据波动
base_desc <- data.frame(标准差=round(base_sd,4), 四分位距=round(base_iqr,4))
cat("\n===== 离散程度指标（Base包）=====\n")
print(base_desc)

# 3. base包计算极值（最大值、最小值）
base_extreme <- data.frame(最大值=apply(df[, -1], 2, max), 最小值=apply(df[, -1], 2, min))
cat("\n===== 极值指标（Base包）=====\n")
print(base_extreme)



#----------------图表 1：盈利三核心指标（净利润 + ROA+ROE）箱线图-------------
# 数据重塑：长格式适配ggplot2分组绘图
df_melt1 <- df %>% select(net_profit, ROA, ROE) %>% melt()
colnames(df_melt1) <- c("指标", "数值")

# 绘图：箱线图（核心看中位数+四分位+异常值）
p1 <- ggplot(df_melt1, aes(x=指标, y=数值, fill=指标)) +
  geom_boxplot(alpha=0.7, outlier.size=0.8, outlier.color="red") +
  labs(title="A股2022年盈利核心指标分布", x="财务指标", y="指标数值") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = c("净利润", "ROA", "ROE"))+
  facet_wrap(~指标,ncol=3,scales='free_y')
print(p1)


#-----------------图表 2：规模 + 偿债指标（总资产 + 流动比率）对数刻度箱线图---------
# 数据重塑
df_melt2 <- df %>% select(total_asset, current_ratio) %>% melt()
colnames(df_melt2) <- c("指标", "数值")

# 绘图：对数刻度箱线图（总资产数值跨度极大，对数处理后更易观察分布）
p2 <- ggplot(df_melt2, aes(x=指标, y=数值, fill=指标)) +
  geom_boxplot(alpha=0.7, outlier.size=0.8) +
  scale_y_log10() + # 关键：对数转换，适配总资产的偏态分布
  labs(title="A股2022年企业规模与短期偿债能力分布", x="财务指标", y="指标数值") +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(labels = c("总资产", "流动比率"))+
  facet_wrap(~指标,ncol=2,scales='free_y')
print(p2)


#--------------图表 3：经营现金流净额密度分布图----------------------------
# 绘图：密度图（核心看现金流的集中趋势+分布形态）
p3 <- ggplot(df, aes(x=cfo)) +
  geom_density(fill="steelblue", alpha=0.5, color="black") +
  coord_cartesian(xlim=c(-50,250) )+
  geom_vline(xintercept = median(df$cfo), color="red", linetype="dashed", linewidth=1) +
  annotate("text", x=median(df$cfo)-30, y=0.15, label="中位数:1.79亿", color="red") +
  geom_vline(xintercept = mean(df$cfo), color="blue", linetype="dashed", linewidth=1)+
  annotate("text", x=mean(df$cfo)+35, y=0.15, label="平均数:16.69亿", color="blue") +
  labs(title="A股2022年经营现金流净额分布", x="经营活动现金流净额(亿)", y="密度")
print(p3)


##-------------方法二：单变量分布特征分析（递进层，指标深度挖掘）-------------------
#图表 1：资产收益率 (ROA) 分位数直方图 + 密度曲线叠加图
# 绘图：直方图+密度曲线，分位数分段，看盈利效率的区间分布
p4 <- ggplot(df, aes(x=ROA)) +
  geom_histogram(aes(y=..density..), bins=30, fill="lightgreen", alpha=0.6, color="black") +
  geom_density(color="darkgreen", linewidth=1) +
  geom_vline(xintercept = quantile(df$ROA, c(0.25,0.5,0.75)), color="red", linetype="dashed") +
  annotate("text", x=quantile(df$ROA,0.25)-0.01, y=8, label="25%", color="red") +
  annotate("text", x=quantile(df$ROA,0.5)+0.005, y=8, label="50%", color="red") +
  annotate("text", x=quantile(df$ROA,0.75)+0.01, y=8, label="75%", color="red") +
  labs(title="A股2022年ROA分布（直方图+密度曲线）", x="资产收益率(ROA)", y="密度/频数")
print(p4)

#图表 2：净资产收益率 (ROE) 分组核密度分布图（股东回报梯队划分）
# 对ROE做商业分层（A股通用标准），核心看股东回报梯队
df <- df %>% mutate(
  ROE_level = case_when(
    ROE < 0.05 ~ "低回报(≤5%)",
    ROE >=0.05 & ROE <0.1 ~ "中回报(5%-10%)",
    ROE >=0.1 ~ "高回报(≥10%)"
  )
)

# 绘图：分组核密度图，看不同回报梯队的分布特征
p5 <- ggplot(df, aes(x=ROE, color=ROE_level, fill=ROE_level)) +
  geom_density(alpha=0.3) +
  labs(title="A股2022年ROE股东回报梯队分布", x="净资产收益率(ROE)", y="密度", color="股东回报梯队", fill="股东回报梯队") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")
print(p5)

#图表 3：流动比率分段图（偿债能力风险分级）
# 流动比率的A股通用风控分级（核心商业逻辑）
df <- df %>% mutate(
  debt_risk = case_when(
    current_ratio < 1 ~ "高风险(＜1)", 
    current_ratio >=1 & current_ratio <2 ~ "合理区间(1-2)", 
    current_ratio >=2 ~ "低风险(≥2)" 
  )
)
debt_count <- df %>% count(debt_risk) %>% mutate(ratio = n/sum(n)*100)

p6 <- ggplot(debt_count, aes(x = 2, y = ratio, fill = debt_risk)) + # x=2 而非x=""
  geom_col(alpha=0.8, width = 1, color = "white") +
  geom_text(aes(label=paste0(round(ratio,1),"%")), position = position_stack(vjust = 0.5), size=4.2) +
  coord_polar(theta = "y", start = 0) + 
  theme_void() +
  xlim(0.5, 2.5)+ # 控制内圈大小，数值差越大，甜甜圈洞越大
  labs(title="A股2022年短期偿债风险等级分布", 
       x="", y="", fill = "流动比率") + # 图例标题更规范
  scale_fill_manual(values = c("lightgreen", "#F8766D", "#619CFF")) + # 优化配色（更高级，贴合风险等级）
  theme(
    plot.title = element_text(hjust = 0.5, size=14, face="bold"), # 标题居中+加粗
    legend.title = element_text(size=14),
    legend.text = element_text(size=10)
  )
print(p6)


##----------------方法三：多变量相关性与关联规律分析（核心层，商业价值挖掘）--------------------
#图表 1：8 大核心指标相关性热力图
cor_mat <- cor(df[, c("total_asset", "current_ratio", "net_profit", "ROA", "ROE", "equity", "cfo")])
cor_melt <- melt(cor_mat)
colnames(cor_melt) <- c("指标1", "指标2", "相关系数")

p7 <- ggplot(cor_melt, aes(x=指标1, y=指标2, fill=相关系数)) +
  # 只显示【左下三角+对角线】，去掉右上重复部分！
  geom_tile(alpha=0.9, color="white", size=1, 
            data = cor_melt[lower.tri(cor_mat, diag = TRUE), ]) + 
  # 正负相关系数 分色显示！
  geom_text(aes(label = round(相关系数, 2),
                color = ifelse(相关系数 < 0, "white", "black")), # 负数白字，正数黑字
            size=3.5, fontface="bold",
            data = cor_melt[lower.tri(cor_mat, diag = TRUE), ]) +
  # 专业渐变配色（深蓝→白→深红，财务图表经典配色）
  scale_fill_gradient2(low = "#1f77b4", mid = "white", high = "#d62728",
                       midpoint = 0, limits = c(-1, 1), name = "相关系数") +
  # 固定文字颜色，不生成多余图例
  scale_color_identity() +
  # 标题和轴标签
  labs(title="A股2022年核心财务指标相关性热力图", x="", y="") +
  # 主题优化（全套精细化调整）
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "right", # 图例放右侧，不遮挡
    panel.grid = element_blank(), # 清空网格线
    axis.ticks = element_blank()  # 清空刻度线
  )
print(p7)

#图表 2：ROE 与 ROA 散点图 + 拟合回归线 + 置信区间（核心盈利关联）
# 看股东回报与资产盈利的关系
p8 <- ggplot(df, aes(x=ROA, y=ROE)) +
  geom_point(alpha=0.5, color="navy", size=1) +
  geom_smooth(method="lm", color="red", linewidth=1, se=TRUE, fill="pink") + # 线性拟合+95%置信区间
  labs(title="A股2022年ROE与ROA关联关系", x="资产收益率(ROA)", y="净资产收益率(ROE)") +
  annotate("text", x=0.2, y=0.02, label=paste("拟合公式: ROE =", round(coef(lm(ROE~ROA,df))[1],4), "+", round(coef(lm(ROE~ROA,df))[2],4), "*ROA"), color="black",size=5)
print(p8)

#图表 3：净利润与经营现金流散点图 + 盈利质量分组（盈利质量核心分析）
# 盈利质量分组（A股财务风控核心标准：利润是否有现金流支撑）
df <- df %>% mutate(
  profit_quality = case_when(
    net_profit > 0 & cfo > 0 ~ "优质(利润+现金流双正)",
    net_profit > 0 & cfo < 0 ~ "存疑(利润正，现金流负)",
    net_profit < 0 & cfo > 0 ~ "潜力(利润负，现金流正)",
    net_profit < 0 & cfo < 0 ~ "劣质(利润+现金流双负)"
  )
)

# 绘图：散点图+分组颜色，看盈利质量的分布特征
p9 <- ggplot(df, aes(x=net_profit, y=cfo, color=profit_quality)) +
  geom_point(alpha=0.6, size=1.2, stroke = 0.3, color = "white") + # 白色描边，解决散点重叠糊成一团
  geom_point(alpha=0.7, size=1) + # 双层散点，分布密度一目了然
  geom_vline(xintercept=0, linetype="dashed", color="black", size=1) + # 竖线加粗，分割正负净利润
  geom_hline(yintercept=0, linetype="dashed", color="black", size=1) + # 横线加粗，分割正负现金流
  labs(title="A股2022年净利润与经营现金流关联分析（盈利质量四象限）", 
       x="净利润(亿)", y="经营现金流净额(亿)", color="盈利质量等级") +
  # 精准配色：优质绿、存疑橙、潜力蓝、劣质红
  scale_color_manual(values = c("darkorange", "firebrick", "royalblue", "forestgreen")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # 标题居中+加粗
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid = element_line(linetype = "dotted") # 网格线变淡变虚，不抢散点焦点
  )+
  coord_cartesian(xlim=c(-500,1000),ylim=c(-200,1000))
print(p9)

#图表 4：PCA 降维聚类散点图
seed=40 #聚类初始值不同，簇的排序不同，不影响结果，修改标签即可
# 缩尾函数,去掉极值保留99%数值分位数
winsorize <- function(x, min_val=0.005, max_val=0.995) {
  quantile_x <- quantile(x, c(min_val, max_val), na.rm=T)
  x[x < quantile_x[1]] <- quantile_x[1]
  x[x > quantile_x[2]] <- quantile_x[2]
  return(x)
}
# 对7个财务指标批量做缩尾处理
df_finance <- df[,2:8] # 索引提取7个财务指标
for(col in colnames(df_finance)){
  df_finance[, col] <- winsorize(df_finance[, col])
}
# 标准化数据（保证聚类/PCA准确性）
df_scale <- scale(df_finance)
# K-Means聚类（主成分分析得A股最优4类）
km_result <- kmeans(df_scale, centers = 4, nstart = 20)
df_finance$财务类型 <- as.factor(km_result$cluster) # 合并聚类结果

# PCA降维
pca_result <- prcomp(df_scale, scale. = FALSE, center = FALSE) 
# 提取前2个主成分用于可视化
df_finance$PC1 <- pca_result$x[,1] # 主成分1：企业综合体量因子：总资产+净利润+股东权益+经营现金流
df_finance$PC2 <- pca_result$x[,2] # 主成分2：盈利能力质量因子：资产收益率+净资产收益率

# 绘制PCA聚类散点图
cluster_center <- df_finance %>% 
  group_by(财务类型) %>% 
  summarise(PC1 = mean(-PC1), PC2 = mean(-PC2))

# 新增财务类型标签
df_finance$财务标签 <- factor(df_finance$财务类型, levels = 1:4, labels = c("盈利低效型", "劣质风险型","规模优质型" ,"成长潜力型" ))
cluster_center$财务标签 <- factor(cluster_center$财务类型, levels = 1:4, labels = c("盈利低效型", "劣质风险型","规模优质型" ,"成长潜力型" ))


p10 <- ggplot(df_finance, aes(x = -PC1, y = -PC2, color = 财务类型, fill = 财务类型)) +
  # 双层散点
  geom_point(alpha = 0.75, size = 2.2, shape = 21, stroke = 1.2, color = "white") +
  geom_point(alpha = 0.7, size = 2, shape = 21, stroke = 1) +
  
  # 核心新增：0轴分割线，划分规模/盈利四象限，财务逻辑完整
  geom_vline(xintercept = 0, linetype = 3, color = "gray40", size = 1) +
  geom_hline(yintercept = 0, linetype = 3, color = "gray40", size = 1) +
  labs(title = "A股上市公司财务分层聚类可视化",
       x = "企业综合体量因子 (PC1)",
       y = "盈利能力质量因子 (PC2)",
       color = "财务标签", fill = "财务标签") +
  # Set1配色，最优财务配色
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  geom_point(alpha = 0.5, size = abs(df_finance$PC1)/max(abs(df_finance$PC1))*3 +1, shape = 21, stroke =1)+  # 聚类中心编号标注
  stat_ellipse(level = 0.95, linetype = 2, size = 1.1) + # 95%置信区间椭圆：加粗+同色系，边界醒目
  geom_text(data = cluster_center, aes(x=PC1, y=PC2, label=财务类型), size=6, fontface="bold", color="black") +
  geom_text(data = cluster_center, aes(x=PC1, y=PC2, label=财务标签), size=5, hjust=-0.1, fontface="bold", color="black")+ # 绘图时追加标注代码
  theme(
    plot.title = element_text(hjust=0.5, size=16, face="bold", color = "gray20", margin = margin(b=10)),
    axis.title.x = element_text(size=12, face="bold", margin = margin(t=8)),
    axis.title.y = element_text(size=12, face="bold", margin = margin(r=8)),
    axis.text = element_text(size=10, color = "gray30"),
    legend.title = element_text(size=11, face="bold"),
    legend.text = element_text(size=10),
    legend.position = "top",
    legend.key.width = unit(1, "cm"),
    panel.grid = element_line(linetype = "dotted"),
    axis.ticks = element_blank()
  )
print(p10)


  


