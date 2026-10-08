# ==========================================
# 第一部分：R 语言基础语法测试（熟悉 R 环境）
# ==========================================

# 1. 基础数学运算
3 + 2

# 2. 变量赋值与打印
result <- 3 + 2  # 将计算结果赋值给名为 result 的对象
result           # 打印 result 的值

# 3. 对已有变量进行运算
result * 2       # 将 result 的值乘以 2，但这不会改变 result 原本的值

# 4. 存储多个值（向量）并进行批量运算
numbers <- c(1, 2, 3, 4, 5)  # 使用 c() 函数创建一个包含多个数字的向量
numbers + 1                  # 向量中的每个数字都加 1

# 5. 使用基础函数
sum(3, 2)        # 调用 sum() 函数计算参数的总和


# ==========================================
# 第二部分：数据分析环境准备与数据导入
# ==========================================

# 1. 安装 Tidyverse 包合集（注意：如果在你的电脑上是首次使用，只需运行一次）
install.packages("tidyverse")

# 2. 加载 Tidyverse 包（每次重启 R 之后进行数据分析前都需要加载）
library(tidyverse)

# 3. 导入数据
# 使用 read_tsv 函数读取制表符分隔的 .tab 数据文件
# 请确保文件路径正确
lfs <- read_tsv("R/Data/lfsp_jm25_eul_pwt24.tab")


# ==========================================
# 第三部分：变量选择与数据过滤（数据清洗）
# ==========================================

# 1. 选取需要的变量
# 使用 select 函数保留分析所需的列，覆盖原有的 lfs 数据框
lfs <- select(lfs, SEX, HOURPAY, AGE, GOR9D, EMPLEN, FLEX22W6, FTPT, HIQUL22D,
              INDE07M, JOBTYP, MPNR02, MARSTA, NSECMJ20, BUSHR)

# 查看当前数据的行数和列数（检查点）
dim(lfs)

# 2. 过滤异常/不相关的薪资数据 (HOURPAY)
# 保留时薪大于等于最低工资 6.4 镑的样本
lfs <- filter(lfs, HOURPAY >= 6.4)
# 保留时薪小于等于 100 镑的合理样本
lfs <- filter(lfs, HOURPAY <= 100)

# 3. 过滤异常的工作时长数据 (BUSHR)
# 排除零工或工作时长少于7小时的人（7小时约等于一个全职工作日）
lfs <- filter(lfs, BUSHR >= 7)
# 排除工作时长大于等于 97 小时的样本（因为在问卷中 97 代表 97 小时及以上，具体数值未知）
lfs <- filter(lfs, BUSHR < 97)

# 4. 过滤年龄范围 (AGE)
# 只保留处于传统工作年龄段 (16 - 64岁) 的样本
lfs <- filter(lfs, AGE >= 16, AGE <= 64)

# 5. 清理缺失值
# 问卷中负数代表缺失值或未回答，因此筛选出所有变量均大于 0 的有效样本
lfs <- filter(lfs, SEX > 0, GOR9D > 0, EMPLEN > 0, FLEX22W6 > 0, FTPT > 0, 
              HIQUL22D > 0, INDE07M > 0, JOBTYP > 0, MPNR02 > 0, MARSTA > 0, NSECMJ20 > 0)

# 6. 清理其他无用值或难以解释的类别
# FTPT中 3 和 4 是特殊的全职/兼职类别，HIQUL22D中 7 代表“不知道”最高学历
lfs <- filter(lfs, FTPT != 3, FTPT != 4, HIQUL22D != 7)

# 再次查看清洗后的数据维度（最终样本量检查）
dim(lfs)


# ==========================================
# 第四部分：数据类型转换（将数值标签转换为因子）
# ==========================================

# 1. 转换性别 (SEX)
lfs <- mutate(lfs, SEX = factor(SEX,
                                levels = 1:2,
                                labels = c("Male", "Female")))

# 2. 批量转换多个二元或有序分类变量 (FLEX22W6, FTPT, EMPLEN)
lfs <- mutate(lfs, 
              FLEX22W6 = factor(FLEX22W6,
                                levels = 1:2,
                                labels = c("Yes", "No")),
              FTPT = factor(FTPT,
                            levels = 1:2,
                            labels = c("Full-time", "Part-time")),
              EMPLEN = factor(EMPLEN,
                              levels = 1:8,
                              labels = c("Less than 3 months", "Three months, less than 6",
                                         "6 months, less than 12", "1 year, less than 2",
                                         "2 years, less than 5", "5 years, less than 10",
                                         "10 years, less than 20", "20 years or more")))

# 3. 转换地理区域 (GOR9D)
lfs <- mutate(lfs,
              GOR9D = factor(GOR9D,
                             levels = c("E12000007","E12000003","E12000009","E12000005",
                                        "E12000008","E12000006","E12000004","E12000001",
                                        "E12000002","W99999999","N99999999","S99999999"),
                             labels = c("London", "Yorkshire and The Humber", "South West",
                                        "West Midlands", "South East", "East of England",
                                        "East Midlands", "North East", "North West",
                                        "Wales", "Northern Ireland", "Scotland")))

# 4. 转换学历 (HIQUL22D) 与 行业部门 (INDE07M)
lfs <- mutate(lfs,
              HIQUL22D = factor(HIQUL22D,
                                levels = 1:6,
                                labels = c("Degree or equivalent", "Higher education",
                                           "A level or equivalent", "GCSE A*-C or equivalent",
                                           "Other qualitfication", "No Qualification")),
              INDE07M = factor(INDE07M,
                               levels = 1:9,
                               labels = c("Agriculture, forestry and fishing", "Energy and water",
                                          "Manufacturing", "Construction",
                                          "Distribution, hotels and restaurants",
                                          "Transport and communication", "Banking and finance",
                                          "Public admin, education and health", "Other services")))

# 5. 转换工作类型 (JOBTYP) 与 公司员工人数 (MPNR02)
lfs <- mutate(lfs,
              JOBTYP = factor(JOBTYP,
                              levels = 1:2,
                              labels = c("Permanent", "Not permanent in some way")),
              MPNR02 = factor(MPNR02,
                              levels = 1:9,
                              labels = c("1-10", "11-19", "20-24", "Don't know but under 25",
                                         "25-49", "50-249", "250-499",
                                         "Don't know but between 50 and 499",
                                         "500 or more")))

# 6. 转换婚姻状态 (MARSTA) 与 职业社会阶层 (NSECMJ20)
lfs <- mutate(lfs,
              MARSTA = factor(MARSTA,
                              levels = 1:6,
                              labels = c("Single, never married",
                                         "Married, living with spouse",
                                         "Married separated from spouse",
                                         "Divorced", "Widowed",
                                         "Currently or previously in civil partnership")),
              NSECMJ20 = factor(NSECMJ20,
                                levels = 1:8,
                                labels = c("Higher managerial and professional",
                                           "Lower managerial and professional",
                                           "Intermediate occupations",
                                           "Small employers and own account workers",
                                           "Lower supervisory and technical",
                                           "Semi-routine occupations",
                                           "Routine occupations",
                                           "Never worked")))


# ==========================================
# 第五部分：最终检查

# ==========================================
# 以电子表格形式在 RStudio 中查看清洗后的数据
# View(lfs)

# 在控制台中概览数据的结构（确认所有 factor 转换正确无误，数值变量依旧是数值类型）
glimpse(lfs)

# 课程2笔记

(1 + 9 + 8 + 2 + 7 + 3 + 2) / 7

mean(c(1,9,8,2,7,3,2))

summarise(lfs, mean(HOURPAY))

summarise(lfs,mean(HOURPAY),
          min(HOURPAY),
          max(HOURPAY))

summarise(lfs,mean_pay = mean(HOURPAY),
          minimum_pay = min(HOURPAY),
          maximum_pay = max(HOURPAY))

summarise(lfs,
          wage_var = var(HOURPAY),
          wage_sd = sd(HOURPAY))

reframe(lfs, quartiles = quantile(HOURPAY, probs = c(0.25, 0.5, 0.75)), 
        q = c("First quartile","Second Quartile","Third Quartile"))

reframe(lfs, quartiles = quantile(HOURPAY, probs = c(0,0.25, 0.5, 0.75,1)), 
        q = c("Minimum","First quartile","Second Quartile",
              "Third Quartile","Maximum"))

summarise(lfs,mean_pay = mean(HOURPAY),
          median_pay = median(HOURPAY))

summary(lfs$HOURPAY)

count(lfs, EMPLEN)

lfs %>% count(EMPLEN)

lfs %>% count(EMPLEN) %>% mutate(percent = n / sum(n)*100)

lfs %>% count(SEX) %>% mutate(percent = n / sum(n)*100)

lfs %>% count(GOR9D) %>% mutate(percent = n /sum(n)*100)

lfs %>% count(FLEX22W6) %>% mutate(percent = n / sum(n)*100)

lfs %>% count(FTPT) %>% mutate(percent = n / sum(n)*100)

lfs %>% count(HIQUL22D) %>% mutate(percent = n / sum(n)*100)

lfs %>% count(INDE07M) %>% mutate(percent = n / sum(n)*100)

lfs %>% count(JOBTYP) %>% mutate(percent = n / sum(n)*100)

lfs %>% count(MPNR02) %>% mutate(percent = n / sum(n)*100)

lfs %>% count(MARSTA) %>% mutate(percent = n / sum(n)*100)

lfs %>% count(NSECMJ20) %>% mutate(percent = n / sum(n)*100)

summary(lfs$BUSHR)

lfs %>% pull(BUSHR) %>% summary()

lfs %>% group_by(SEX) %>% summarise(mean_wage = mean(HOURPAY), 
                                    median_wage = median(HOURPAY))
table(lfs$HIQUL22D, lfs$SEX)

table(lfs$HIQUL22D, lfs$SEX) %>% prop.table() %>% round(2)

table(lfs$HIQUL22D, lfs$SEX) %>% prop.table(1) %>% round(2)

table(lfs$HIQUL22D, lfs$SEX) %>% prop.table(2) %>% round(2)




