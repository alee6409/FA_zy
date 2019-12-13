setwd()
ch_2019 <- read.csv('ch_2019.csv')

#量表全部内容
scale_data <- ch_2019[10:25]
head(scale_data)

#全变量相关

scale_total <- apply(scale_data, 1, sum)
xm <- ch_2019[26]
#外向
wx_sum <- ch_2019[27] + ch_2019[35]
wx <- apply(wx_sum, 1, sum)
#宜人
yr_sum <- ch_2019[29] + ch_2019[36]
yr <- apply(yr_sum, 1, sum)
#尽责
jz_sum <- ch_2019[30] + ch_2019[38]
jz <- apply(jz_sum, 1, sum)
#稳定
wd_sum <- ch_2019[32] + ch_2019[39]
wd <- apply(wd_sum, 1, sum)
#开放
kf_sum <- ch_2019[33] + ch_2019[41]
kf <- apply(kf_sum, 1, sum)

data1 <-data.frame(scale_data, wx, yr, jz, wd, kf, xm)
library(psych)
total_cor <- corr.test(data1)
library(corrplot)
corrplot(total_cor)

#可行性检验
KMO(cor(scale_data))

#碎石图
fa.parallel(scale_data)

#因子分析
fit_fa <- fa(scale_data, nfactors=1, rotate='varimax')
#载荷图
fa.diagram(fit_fa)
fit_fa

#信度
reliability.function <- function(df)
{
     n <- ncol(df)
     p <- sum(apply(df, 2, var)) / var(apply(df, 1, sum))  #2为按列处理，1为按行处理
     alpha = n / (n-1) * (1-p)
     #结果输出
     cat("alpha=", alpha, "\n")
}
