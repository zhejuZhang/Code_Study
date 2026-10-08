# ==========================================
# Lab 2：描述统计与数据探索
# ==========================================

# 加载数据分析所需的 tidyverse 包
library(tidyverse)

# 从项目根目录或 R/Code 目录运行脚本时都能找到数据文件
data_file <- file.path("R", "Data", "lfsp_jm25_eul_pwt24.tab")
if (!file.exists(data_file)) {
  data_file <- file.path("..", "Data", "lfsp_jm25_eul_pwt24.tab")
}
if (!file.exists(data_file)) {
  stop("找不到数据文件 lfsp_jm25_eul_pwt24.tab，请从项目根目录或 R/Code 目录运行脚本。")
}

# 导入劳动调查数据，并保留本次分析所需的变量
lfs <- read_tsv(data_file, show_col_types = FALSE) %>%
  select(SEX, HOURPAY, AGE, GOR9D, EMPLEN, FLEX22W6, FTPT, HIQUL22D,
         INDE07M, JOBTYP, MPNR02, MARSTA, NSECMJ20, BUSHR)

# 清理缺失值、无效类别及极端值，沿用 Lab 1 的样本筛选条件
lfs <- lfs %>%
  filter(
    HOURPAY >= 6.4, HOURPAY <= 100,
    BUSHR >= 7, BUSHR < 97,
    AGE >= 16, AGE <= 64,
    SEX > 0, GOR9D > 0, EMPLEN > 0, FLEX22W6 > 0, FTPT > 0,
    HIQUL22D > 0, INDE07M > 0, JOBTYP > 0, MPNR02 > 0,
    MARSTA > 0, NSECMJ20 > 0,
    FTPT != 3, FTPT != 4, HIQUL22D != 7
  )

# 将类别编码转换为便于阅读的因子标签
lfs <- lfs %>%
  mutate(
    SEX = factor(SEX, levels = 1:2, labels = c("Male", "Female")),
    FLEX22W6 = factor(FLEX22W6, levels = 1:2, labels = c("Yes", "No")),
    FTPT = factor(FTPT, levels = 1:2, labels = c("Full-time", "Part-time")),
    EMPLEN = factor(
      EMPLEN,
      levels = 1:8,
      labels = c(
        "Less than 3 months", "Three months, less than 6",
        "6 months, less than 12", "1 year, less than 2",
        "2 years, less than 5", "5 years, less than 10",
        "10 years, less than 20", "20 years or more"
      )
    ),
    GOR9D = factor(
      GOR9D,
      levels = c(
        "E12000007", "E12000003", "E12000009", "E12000005",
        "E12000008", "E12000006", "E12000004", "E12000001",
        "E12000002", "W99999999", "N99999999", "S99999999"
      ),
      labels = c(
        "London", "Yorkshire and The Humber", "South West",
        "West Midlands", "South East", "East of England",
        "East Midlands", "North East", "North West", "Wales",
        "Northern Ireland", "Scotland"
      )
    ),
    HIQUL22D = factor(
      HIQUL22D,
      levels = 1:6,
      labels = c(
        "Degree or equivalent", "Higher education",
        "A level or equivalent", "GCSE A*-C or equivalent",
        "Other qualitfication", "No Qualification"
      )
    ),
    INDE07M = factor(
      INDE07M,
      levels = 1:9,
      labels = c(
        "Agriculture, forestry and fishing", "Energy and water",
        "Manufacturing", "Construction",
        "Distribution, hotels and restaurants",
        "Transport and communication", "Banking and finance",
        "Public admin, education and health", "Other services"
      )
    ),
    JOBTYP = factor(
      JOBTYP,
      levels = 1:2,
      labels = c("Permanent", "Not permanent in some way")
    ),
    MPNR02 = factor(
      MPNR02,
      levels = 1:9,
      labels = c(
        "1-10", "11-19", "20-24", "Don't know but under 25",
        "25-49", "50-249", "250-499",
        "Don't know but between 50 and 499", "500 or more"
      )
    ),
    MARSTA = factor(
      MARSTA,
      levels = 1:6,
      labels = c(
        "Single, never married", "Married, living with spouse",
        "Married separated from spouse", "Divorced", "Widowed",
        "Currently or previously in civil partnership"
      )
    ),
    NSECMJ20 = factor(
      NSECMJ20,
      levels = 1:8,
      labels = c(
        "Higher managerial and professional",
        "Lower managerial and professional",
        "Intermediate occupations",
        "Small employers and own account workers",
        "Lower supervisory and technical",
        "Semi-routine occupations", "Routine occupations",
        "Never worked"
      )
    )
  )

# 检查整理后的数据结构和样本量
glimpse(lfs)
dim(lfs)


# ==========================================
# 一、均值、中位数与工资分布
# ==========================================

# 手动计算示例及 mean() 函数
(1 + 9 + 8 + 2 + 7 + 3 + 2) / 7
mean(c(1, 9, 8, 2, 7, 3, 2))

# 平均时薪、最低时薪和最高时薪
summarise(lfs, mean(HOURPAY))

summarise(
  lfs,
  mean(HOURPAY),
  min(HOURPAY),
  max(HOURPAY)
)

summarise(
  lfs,
  mean_pay = mean(HOURPAY),
  minimum_pay = min(HOURPAY),
  maximum_pay = max(HOURPAY)
)

# 方差和标准差
summarise(
  lfs,
  wage_var = var(HOURPAY),
  wage_sd = sd(HOURPAY)
)

# 四分位数以及含最小值、最大值的分位数
reframe(
  lfs,
  quartiles = quantile(HOURPAY, probs = c(0.25, 0.5, 0.75)),
  q = c("First quartile", "Second Quartile", "Third Quartile")
)

reframe(
  lfs,
  quartiles = quantile(HOURPAY, probs = c(0, 0.25, 0.5, 0.75, 1)),
  q = c("Minimum", "First quartile", "Second Quartile",
        "Third Quartile", "Maximum")
)

# 比较平均时薪和中位时薪；summary() 一次列出常用分位数
summarise(
  lfs,
  mean_pay = mean(HOURPAY),
  median_pay = median(HOURPAY)
)

summary(lfs$HOURPAY)


# ==========================================
# 二、频数表与比例
# ==========================================

# 就业年限频数表及百分比
count(lfs, EMPLEN)
lfs %>% count(EMPLEN)
lfs %>% count(EMPLEN) %>% mutate(percent = n / sum(n) * 100)

# 其他分类变量的频数及百分比
lfs %>% count(SEX) %>% mutate(percent = n / sum(n) * 100)
lfs %>% count(GOR9D) %>% mutate(percent = n / sum(n) * 100)
lfs %>% count(FLEX22W6) %>% mutate(percent = n / sum(n) * 100)
lfs %>% count(FTPT) %>% mutate(percent = n / sum(n) * 100)
lfs %>% count(HIQUL22D) %>% mutate(percent = n / sum(n) * 100)
lfs %>% count(INDE07M) %>% mutate(percent = n / sum(n) * 100)
lfs %>% count(JOBTYP) %>% mutate(percent = n / sum(n) * 100)
lfs %>% count(MPNR02) %>% mutate(percent = n / sum(n) * 100)
lfs %>% count(MARSTA) %>% mutate(percent = n / sum(n) * 100)
lfs %>% count(NSECMJ20) %>% mutate(percent = n / sum(n) * 100)

# 基本通常工作时长的概要统计
summary(lfs$BUSHR)
lfs %>% pull(BUSHR) %>% summary()


# ==========================================
# 三、分组统计与交叉表
# ==========================================

# 按性别比较平均时薪和中位时薪
lfs %>%
  group_by(SEX) %>%
  summarise(
    mean_wage = mean(HOURPAY),
    median_wage = median(HOURPAY)
  )

# 性别与学历的交叉频数表
table(lfs$HIQUL22D, lfs$SEX)

# 单元格比例、行比例和列比例
table(lfs$HIQUL22D, lfs$SEX) %>% prop.table() %>% round(2)
table(lfs$HIQUL22D, lfs$SEX) %>% prop.table(1) %>% round(2)
table(lfs$HIQUL22D, lfs$SEX) %>% prop.table(2) %>% round(2)

# 性别与行业的列比例
table(lfs$INDE07M, lfs$SEX) %>% prop.table(2) %>% round(2)


# ==========================================
# 四、查看和排序数据
# ==========================================

# 查看数据的前几行
head(lfs)

# 按时薪升序排列，并查看最高时薪的记录
lfs %>% arrange(HOURPAY) %>% tail()

