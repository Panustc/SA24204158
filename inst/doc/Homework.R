## -----------------------------------------------------------------------------
library(ggplot2)
### 数据集预览
mtcars
summary(mtcars)

## -----------------------------------------------------------------------------
### 生成箱线图
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot() +
  labs(title = "Miles per Gallon by Number of Cylinders")

## -----------------------------------------------------------------------------
xtable::xtable(head(iris))

## ----message=FALSE, warning=FALSE---------------------------------------------
# 加载knitr包
library(knitr)

# 创建数据框
data <- data.frame(
  Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5.0, 5.4),
  Sepal.Width = c(3.5, 3.0, 3.2, 3.1, 3.6, 3.9),
  Petal.Length = c(1.4, 1.4, 1.3, 1.5, 1.4, 1.7),
  Petal.Width = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.4),
  Species = c("setosa", "setosa", "setosa", "setosa", "setosa", "setosa")
)

# 使用kable生成表格
kable(data, caption = "Iris数据集的部分内容")

## -----------------------------------------------------------------------------
# 设置图形布局为1行2列
par(mfrow = c(1, 2))  

# 生成100个标准正态分布的随机点
set.seed(123)
data_100 <- rnorm(100)

# 绘制100个样本点的直方图
hist(data_100, breaks = 10, probability = TRUE, 
     main = "标准正态分布的直方图\n(样本点数 = 100)", 
     xlab = "X", 
     col = "lightblue")
curve(dnorm(x), col = "red", lwd = 2, add = TRUE)

# 生成1000个标准正态分布的随机点
data_1000 <- rnorm(1000)

# 绘制1000个样本点的直方图
hist(data_1000, breaks = 20, probability = TRUE, 
     main = "标准正态分布的直方图\n(样本点数 = 1000)", 
     xlab = "X", 
     col = "lightblue")
curve(dnorm(x), col = "red", lwd = 2, add = TRUE)

# 恢复默认的图形布局
par(mfrow = c(1, 1))



## -----------------------------------------------------------------------------
# 生成模拟数据
set.seed(123)
x <- rnorm(100)
y <- 2*x + rnorm(100)

# 线性回归模型
model <- lm(y ~ x)

# 输出回归结果
summary(model)

# 绘制回归图
plot(x, y, main = "线性回归图", col = "blue")
abline(model, col = "red", lwd = 2)

## -----------------------------------------------------------------------------
set.seed(123)

# 设定参数
sigma_values <- c(1,3,5,7)  # 不同的尺度参数
n <- 10000  # 生成随机数的数量

# 循环遍历每个sigma值
for (sigma in sigma_values) {
  # 生成均匀分布的随机数
  U <- runif(n)
  
  # 使用逆变换法生成Rayleigh分布的随机数
  X <- sigma * sqrt(-2 * log(1-U))
  
  # 绘制直方图
  hist(X, breaks=30, probability=TRUE,
       main=paste("Rayleigh Distribution (sigma =", sigma, ")"),
       xlab="Value", ylab="Density", xlim=c(0, 15))
  
  # 添加理论密度曲线
  curve((x/(sigma^2)) * exp(-x^2/(2*sigma^2)), add=TRUE, col="blue", lwd=2)
}


## -----------------------------------------------------------------------------
N <- 1000 # 样本容量
set.seed(123)
p1 = 0.75
p <- c(p1, 1-p1)

# 生成混合分布
k <- sample(0:1, size = N, replace = TRUE, prob = p)
y1 <- rnorm(N)
y2 <- rnorm(N, mean = 3, sd = 1)
x <- (1 - k) * y1 + k * y2

# 绘制直方图和核密度估计曲线
hist(x, probability = TRUE, main = paste("p1 =", p1), col = 4, breaks = 100)
lines(density(x), lwd = 3, col = 2)

## -----------------------------------------------------------------------------
N <- 1000  # 样本容量
set.seed(123)

# 定义不同的p1值
p1_vals <- seq(from = 0.1, to = 0.9, by = 0.1)

# 生成图形
for (p1 in p1_vals) {
  p <- c(p1, 1 - p1)
  k <- sample(0:1, size = N, replace = TRUE, prob = p)
  
  # 生成两个正态分布
  y1 <- rnorm(N)
  y2 <- rnorm(N, mean = 3, sd = 1)
  
  # 混合分布
  x <- (1 - k) * y1 + k * y2
  
  # 绘制直方图和核密度估计曲线
  hist(x, probability = TRUE, main = paste("p1 =", p1), col = 4, breaks = 50, border = "white", xlab = "", ylab = "")
  lines(density(x), lwd = 2, col = 2)
}



## -----------------------------------------------------------------------------
set.seed(123)  # 设置随机种子保证结果可重复

# 参数设置
lambda <- 4    # 泊松过程的λ参数
shape <- 5     # Gamma分布的形状参数 (shape)
scale <- 6     # Gamma分布的尺度参数 (scale)
t <- 10        # 时间t
n_sim <- 10000 # 模拟次数

# 到达间隔时间随速率λ呈指数分布
pp.exp <- function(t0) {
  Tn <- rexp(1000, lambda)  # 生成间隔时间的指数分布
  Sn <- cumsum(Tn)          # 累积和计算事件发生的时间
  return(min(which(Sn > t0)) - 1)  # 返回第一个超过时间t的索引
}

# 生成服从泊松分布的N(t)
N_t <- replicate(n_sim, pp.exp(t))

# 生成复合泊松过程的X(t)
X_t <- sapply(N_t, function(n) {
  Y <- rgamma(n = n, shape = shape, scale = scale)  # Gamma分布
  sum(Y[1:n])  # 求和得到X(t)
})

# 估计均值和方差
mean_X_t <- mean(X_t)
var_X_t <- var(X_t)

# 理论均值和方差
theoretical_mean <- lambda * t * shape * scale
theoretical_var <- lambda * t * (shape + 1) * shape * scale^2

# 输出结果
matrix(c(mean_X_t, theoretical_mean, var_X_t, theoretical_var), 
       ncol = 4, 
       dimnames = list(c("value"), c("Estimated Mean", "Theoretical Mean", "Estimated Variance", "Theoretical Variance")))



## -----------------------------------------------------------------------------
set.seed(123)  # 设置随机种子保证结果可重复

# 参数设置
lambda <- 4    # 泊松过程的λ参数
t <- 10        # 时间t
shape <- 5     # Gamma分布的形状参数 (shape)
scale <- 6     # Gamma分布的尺度参数 (scale)
n_sim <- 10000 # 模拟次数

# 模拟复合泊松过程
X_t <- numeric(n_sim)  # 存储X(t)的结果

for (i in 1:n_sim) {
  N_t <- rpois(1, lambda * t)  # 生成N(t)，泊松分布
  if (N_t > 0) {
    Y <- rgamma(N_t, shape=shape, scale=scale)  # 生成Y_i，Gamma分布 (使用shape和scale)
    X_t[i] <- sum(Y)  # 求和得到X(t)
  } else {
    X_t[i] <- 0  # 当N(t)=0时，X(t)=0
  }
}

# 估计均值和方差
mean_X_t <- mean(X_t)
var_X_t <- var(X_t)

# 理论均值和方差
theoretical_mean <- lambda * t * shape * scale
theoretical_var <- lambda * t * shape*(shape+1) * (scale^2)

# 输出结果
matrix(c(mean_X_t,theoretical_mean,var_X_t,theoretical_var),ncol = 4,
          dimnames = list(c("value"),c("Estimated Mean","Theoretical Mean","Estimated Variance", "Theoretical Variance")))

## -----------------------------------------------------------------------------
# 设置参数
alpha <- 3
beta <- 3

# Monte Carlo方法估计Beta分布的CDF
mc_beta_cdf <- function(x, n = 10000) {
  # 从Beta(3, 3)分布中生成n个随机样本
  samples <- rbeta(n, alpha, beta)
  # 计算P(X <= x)的比例
  return(mean(samples <= x))
}

# 估计不同x值下的CDF
x_values <- seq(0.1, 0.9, by = 0.1)
mc_estimates <- sapply(x_values, mc_beta_cdf)

# 使用pbeta函数计算真实值
pbeta_values <- pbeta(x_values, alpha, beta)

# 打印结果
results <- data.frame(x = x_values, MonteCarlo_Estimate = mc_estimates, pbeta_Value = pbeta_values)
print(results)

# 比较两个结果
plot(x_values, mc_estimates, type = "b", col = "blue", pch = 19, ylim = range(c(mc_estimates, pbeta_values)),
     xlab = "x", ylab = "F(x)", main = "Comparison of Monte Carlo and pbeta")
lines(x_values, pbeta_values, type = "b", col = "red", pch = 17)
legend("topleft", legend = c("Monte Carlo Estimate", "pbeta Value"), col = c("blue", "red"), pch = c(19, 17))

## -----------------------------------------------------------------------------
# 设置Rayleigh分布的参数
sigma <- 1
n <- 10000  # 样本数量

# 定义生成Rayleigh分布的函数，这里采用逆变换法
rayleigh_sample <- function(n, sigma) {
  U <- runif(n)
  X <- sigma * sqrt(-2 * log(U))
  return(X)
}

# 生成对偶变量样本
rayleigh_antithetic <- function(n, sigma) {
  U <- runif(n/2)
  X1 <- sigma * sqrt(-2 * log(U))
  X2 <- sigma * sqrt(-2 * log(1 - U))
  return(c(X1, X2))
}

# 计算独立样本的均值和方差
X_ind <- rayleigh_sample(n, sigma)
mean_ind <- mean(X_ind)
var_ind <- var(X_ind)

# 计算对偶变量样本的均值和方差
X_antithetic <- rayleigh_antithetic(n, sigma)
X_combined <- (X_antithetic[1:(n/2)] + X_antithetic[(n/2 + 1):n]) / 2
mean_combined <- mean(X_combined)
var_combined <- var(X_combined)

# 计算方差减少百分比
percent_reduction <- (var_ind - var_combined) / var_ind * 100

# 打印结果
cat("独立样本的均值:", mean_ind, "\n")
cat("对偶变量样本的均值:", mean_combined, "\n")
cat("独立样本的方差:", var_ind, "\n")
cat("对偶变量样本的方差:", var_combined, "\n")
cat("方差减少百分比:", percent_reduction, "%\n")


## -----------------------------------------------------------------------------
f <- function(x) {
  (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

result <- integrate(f, lower = 1, upper = Inf)

cat("积分结果: ", result$value, "\n")


## ----eval = FALSE-------------------------------------------------------------
#  set.seed(123)
#  # 定义 g(x) 函数
#  g <- function(x) {
#    (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
#  }
#  
#  # 截断的正态分布 (f_1) 密度函数
#  f1 <- function(x) {
#    dnorm(x) / (1 - pnorm(1))  # 截断的标准正态分布（x > 1）
#  }
#  
#  # 指数分布 (f_2) 密度函数，lambda = 1
#  f2 <- function(x) {
#    exp(-(x - 1))  # 指数分布的 PDF (x > 1)
#  }
#  
#  # Rayleigh 分布 (f_3) 密度函数，sigma = 1
#  f3 <- function(x) {
#    ifelse(x > 1, (x * exp(-x^2 / 2)) / (1 - (1 - exp(-1/2))), 0)  # 归一化的 PDF
#  }
#  
#  # 生成 f_1 和 f_2 下的样本
#  sample_f1 <- function(n) {
#    # 初始化一个空的向量，用来存储大于1的随机数
#    result <- c()
#  
#    # 继续生成随机数，直到我们收集到n个大于1的数
#    while (length(result) < n) {
#      # 从标准正态分布中生成随机数
#      random_numbers <- rnorm(n)
#  
#      # 选择大于1的随机数
#      valid_numbers <- random_numbers[random_numbers > 1]
#  
#      # 将这些大于1的数添加到结果中
#      result <- c(result, valid_numbers)
#    }
#  
#    # 返回前n个大于1的数
#    return(result[1:n])
#  }
#  
#  sample_f2 <- function(n) {
#    result <- c()
#    while (length(result) < n) {
#      random_numbers <- rexp(n)
#      valid_numbers <- random_numbers[random_numbers > 1]
#      result <- c(result, valid_numbers)
#    }
#    return(result[1:n])
#  }
#  
#  # 从 Rayleigh 分布中生成样本，确保归一化
#  sample_f3 <- function(n) {
#    result <- c()
#  
#    # 继续生成随机数，直到我们收集到n个大于1的数
#    while (length(result) < n) {
#      # 生成 Rayleigh 分布的随机数，使用 sqrt(-2 * log(U))
#      random_numbers <- sqrt(-2 * log(runif(n)))
#      valid_numbers <- random_numbers[random_numbers > 1]
#      result <- c(result, valid_numbers)
#    }
#  
#    # 返回前n个大于1的数
#    return(result[1:n])
#  }
#  
#  # 重要性采样估计积分
#  importance_sampling <- function(f_sample, f_density, g, n) {
#    x <- f_sample(n)
#    weights <- g(x) / f_density(x)
#    return(mean(weights))
#  }
#  
#  # 设置样本数
#  n <- 10000
#  
#  # 使用 f_1 进行重要性采样
#  estimate_f1 <- importance_sampling(sample_f1, f1, g, n)
#  cat("使用 f_1 的重要性采样估计值:", estimate_f1, "\n")
#  
#  # 使用 f_2 进行重要性采样
#  estimate_f2 <- importance_sampling(sample_f2, f2, g, n)
#  cat("使用 f_2 的重要性采样估计值:", estimate_f2, "\n")
#  
#  # 使用 f_3 进行重要性采样
#  estimate_f3 <- importance_sampling(sample_f3, f3, g, n)
#  cat("使用 f_3 的重要性采样估计值:", estimate_f3, "\n")
#  
#  # 计算方差比较
#  samples_f1 <- sample_f1(n)
#  samples_f2 <- sample_f2(n)
#  samples_f3 <- sample_f3(n)
#  
#  variance_f1 <- var(g(samples_f1) / f1(samples_f1))
#  variance_f2 <- var(g(samples_f2) / f2(samples_f2))
#  variance_f3 <- var(g(samples_f3) / f3(samples_f3))
#  
#  cat("f_1 的重要性采样方差:", variance_f1, "\n")
#  cat("f_2 的重要性采样方差:", variance_f2, "\n")
#  cat("f_3 的重要性采样方差:", variance_f3, "\n")
#  
#  # 计算方差减少百分比
#  percent_reduction_f1_f2 <- (variance_f1 - variance_f2) / variance_f1 * 100
#  cat("f_2 相对于 f_1 的方差减少百分比:", percent_reduction_f1_f2, "%\n")
#  
#  percent_reduction_f1_f3 <- (variance_f1 - variance_f3) / variance_f3 * 100
#  cat("f_3 相对于 f_1 的方差减少百分比:", percent_reduction_f1_f3, "%\n")
#  

## -----------------------------------------------------------------------------
# 设置图形设备
set.seed(123)
x <- seq(0, 5, length.out = 1000)

# 定义 g(x) 函数
g <- function(x) {
  (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

# 计算各个分布的概率密度函数
g_values <- g(x)
normal_values <- dnorm(x)  # 正态分布
exponential_values <- dexp(x - 1)  # 指数分布，偏移至 x > 1
rayleigh_values <- ifelse(x > 0, (x / 1) * exp(-x^2 / 2), 0)  # Rayleigh 分布，sigma = 1

# 绘图
plot(x, g_values, type = "l", col = "blue", lwd = 3,xlim = c(1, 5), ylim = c(0, 1),
     xlab = "x", ylab = "Density", main = "Comparison of g(x) and Various Distributions")
lines(x, normal_values, col = "orange", lwd = 3)
lines(x, exponential_values, col = "black", lwd = 3)
lines(x, rayleigh_values, col = "red", lwd = 3)
legend("topright", legend = c("g(x)", "Normal Distribution (f1)", "Exponential Distribution (f2)", "Rayleigh Distribution (f3)"),
       col = c("blue", "orange", "black", "red"), lty = 1, lwd = 3)
grid()


## -----------------------------------------------------------------------------
g <- function(x) {
  (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

# 截断的正态分布 (f_1) 密度函数
f1 <- function(x) {
  dnorm(x) / (1 - pnorm(1))  # 截断的标准正态分布（x > 1）
}

# 指数分布 (f_2) 密度函数，lambda = 1
f2 <- function(x) {
  exp(-(x - 1))  # 指数分布的 PDF (x > 1)
}

# Rayleigh 分布 (f_3) 密度函数，sigma = 1
f3 <- function(x) {
(x * exp(-x^2 / 2)) / (1 - exp(-1/2))  # 归一化的 Rayleigh 分布
}

# 设置 x 的取值范围
x_values <- seq(1, 6, length.out = 1000)

# 计算 g(x) / f1(x), g(x) / f2(x), g(x) / f3(x)
y_f1 <- g(x_values) / f1(x_values)
y_f2 <- g(x_values) / f2(x_values)
y_f3 <- g(x_values) / f3(x_values)

# 绘图
plot(x_values, y_f1, type = "l", col = "blue", lwd = 2,xlim = c(1,5),ylim = c(0, max(c(y_f1, y_f2, y_f3))),
     xlab = "x", ylab = "g(x) / f(x)", main = "g(x) / f(x) for f1, f2, f3 Distributions")
lines(x_values, y_f2, col = "green", lwd = 2)
lines(x_values, y_f3, col = "red", lwd = 2)

# 添加图例
legend("topright", legend = c("Normal Distribution g(x) / f1(x)", " Exponential Distribution g(x) / f2(x)", " Rayleigh Distribution g(x) / f3(x)"),
       col = c("blue", "green", "red"), lwd = 2)


## ----eval = FALSE-------------------------------------------------------------
#  set.seed(123)
#  
#  # 定义要测试的n值
#  n_values <- c(10^4, 2 * 10^4, 4 * 10^4, 6 * 10^4, 8 * 10^4)
#  
#  # 存储每个n的平均计算时间
#  avg_times <- numeric(length(n_values))
#  
#  # 进行模拟，测试每个n的排序时间
#  for (i in seq_along(n_values)) {
#    n <- n_values[i]
#    times <- numeric(100)  # 存储100次模拟的时间
#  
#    # 对每个n进行100次排序模拟
#    for (j in 1:100) {
#      vec <- sample(1:n, n)  # 生成1到n的随机排列
#      times[j] <- system.time(sort(vec, method = "quick"))[3]  # 记录排序时间
#    }
#  
#    avg_times[i] <- mean(times)  # 计算平均时间
#  }
#  
#  # 计算 t_n = n * log(n)
#  log_n_values <- n_values * log(n_values)
#  
#  # 回归分析: avg_times 作为因变量，log_n_values 作为自变量
#  regression <- lm(avg_times ~ log_n_values)
#  
#  # 打印回归结果
#  summary(regression)
#  
#  # 绘图：散点图和回归线
#  plot(log_n_values, avg_times, main = "Sorting Time vs n * log(n)",
#       xlab = "n * log(n)", ylab = "Average Sorting Time (seconds)", pch = 16, col = "blue")
#  abline(regression, col = "red", lwd = 2)  # 添加回归线
#  

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(123)
#  
#  # 定义要测试的n值
#  n_values <- c(10^4, 2 * 10^4, 4 * 10^4, 6 * 10^4, 8 * 10^4)
#  
#  # 存储每个n的平均计算时间
#  avg_times <- numeric(length(n_values))
#  
#  # 定义快速排序算法
#  quick_sort <- function(vec) {
#    if (length(vec) <= 1) {
#      return(vec)  # 如果只有一个元素或空，直接返回
#    } else {
#      pivot <- vec[1]  # 选择第一个元素作为主元
#      left <- quick_sort(vec[vec < pivot])  # 小于主元的部分递归排序
#      right <- quick_sort(vec[vec > pivot])  # 大于主元的部分递归排序
#      return(c(left, vec[vec == pivot], right))  # 连接结果
#    }
#  }
#  
#  # 进行模拟，测试每个n的排序时间
#  for (i in seq_along(n_values)) {
#    n <- n_values[i]
#    times <- numeric(100)  # 存储100次模拟的时间
#  
#    # 对每个n进行100次排序模拟
#    for (j in 1:100) {
#      vec <- sample(1:n, n)  # 生成1到n的随机排列
#      times[j] <- system.time(quick_sort(vec))[3]  # 使用手写快速排序，并记录排序时间
#    }
#  
#    avg_times[i] <- mean(times)  # 计算平均时间
#  }
#  
#  # 计算 t_n = n * log(n)
#  log_n_values <- n_values * log(n_values)
#  
#  # 回归分析: avg_times 作为因变量，log_n_values 作为自变量
#  regression <- lm(avg_times ~ log_n_values)
#  
#  # 打印回归结果
#  summary(regression)
#  
#  # 绘图：散点图和回归线
#  plot(log_n_values, avg_times, main = "Sorting Time vs n * log(n)",
#       xlab = "n * log(n)", ylab = "Average Sorting Time (seconds)", pch = 16, col = "blue")
#  abline(regression, col = "red", lwd = 2)  # 添加回归线
#  

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
# 第一个函数：数据生成
generate_data <- function(m, n) {
  # 偏度函数
  sk <- function(x) {
    x_bar <- mean(x)
    b <- mean((x - x_bar)^3) / (mean((x - x_bar)^2))^1.5
    return(b)
  }

  b <- numeric(m)
  
  set.seed(0)
  for (i in 1:m) {
    X <- rnorm(n)
    b[i] <- sk(X)
  }

  # 返回排序后的b值
  b_order <- sort(b)
  
  # 内存清理（确保返回的数据不会丢失）
  rm(list = setdiff(ls(), "b_order"))
  
  return(b_order)
}

# 第二个函数：统计推断
statistical_inference <- function(b_order, n) {
  
  # 分位数计算
  qutile <- numeric(4)
  qutile[1] <- b_order[25]   # 0.025 分位数
  qutile[2] <- b_order[50]   # 0.05 分位数
  qutile[3] <- b_order[950]  # 0.95 分位数
  qutile[4] <- b_order[975]  # 0.975 分位数

  # 渐进分布密度函数
  f <- function(x, n) {
    return((1 / sqrt(2 * pi * 6 * (n - 2) / ((n + 1) * (n + 3)))) * exp(-x^2 / (2 * 6 * (n - 2) / ((n + 1) * (n + 3)))))
  }

  # 分位数标准误差计算
  sd_qutile <- numeric(4)
  sd_qutile[1] <- 0.025 * 0.975 / (n * (f(qutile[1], n))^2)
  sd_qutile[2] <- 0.05 * 0.95 / (n * (f(qutile[2], n))^2)
  sd_qutile[3] <- 0.05 * 0.95 / (n * (f(qutile[3], n))^2)
  sd_qutile[4] <- 0.025 * 0.975 / (n * (f(qutile[4], n))^2)

  # 渐近分位数计算
  cv <- numeric(4)
  cv[1] <- qnorm(0.025, 0, sqrt(6 / n))
  cv[2] <- qnorm(0.05, 0, sqrt(6 / n))
  cv[3] <- qnorm(0.95, 0, sqrt(6 / n))
  cv[4] <- qnorm(0.975, 0, sqrt(6 / n))

  # 返回所有计算结果
  inference_results <- list(qutile = qutile, sd_qutile = sd_qutile, cv = cv)
  
  # 内存清理（保留 inference_results）
  rm(list = setdiff(ls(), "inference_results"))
  
  return(inference_results)
}

# 第三个函数：结果报告
report_results <- function(inference_results) {

  # 生成结果数据框
  outcome <- data.frame(
    Estimate = c("0.025 分位数", "0.05 分位数", "0.95 分位数", "0.975 分位数"),
    MC_es = inference_results$qutile,
    asymptotic = inference_results$cv,
    MC_es_sd = inference_results$sd_qutile
  )

  # 展示结果
  print(outcome)
  
  # 内存清理
  rm(list = ls())
}

# 主程序：依次调用三个函数
main <- function() {
  m <- 1000
  n <- 1000

  # 1. 数据生成
  b_order <- generate_data(m, n)
  
  # 2. 统计推断
  inference_results <- statistical_inference(b_order, n)
  
  # 3. 结果报告
  report_results(inference_results)
}

# 执行主程序
main()


## -----------------------------------------------------------------------------
# 定义 x 的范围
x <- seq(-3, 3, length.out = 400)

# 计算 y = exp(x) - 3*x
y <- exp(x) - 3*x

# 绘制图像
plot(x, y, type = "l", col = "blue", lwd = 2,
     main = expression(paste("Plot of ", e^x - 3*x)),
     xlab = "x", ylab = "y")
abline(h = 0, v = 0, col = "black")  # 添加 x 和 y 轴
grid()  # 显示网格


## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量
set.seed(123)
# 定义功效计算函数
calculate_power <- function(cor_method = "pearson", reps = 1000, alpha = 0.05) {
  reject_null <- numeric(reps)  # 存储是否拒绝原假设的结果
  
  for (i in 1:reps) {
    # 生成非线性相关数据（作为备择假设）
    x <- rnorm(30,0,1)
    sigma <- rnorm(30,0,1)
    y <- exp(x)-3*x+sigma
    
    # 根据选择的检验方法计算相关性
    if (cor_method == "pearson") {
      test <- cor.test(x, y, method = "pearson")
    } else if (cor_method == "kendall") {
      test <- cor.test(x, y, method = "kendall")
    } else if (cor_method == "spearman") {
      test <- cor.test(x, y, method = "spearman")
    }
    
    # 如果 p 值小于显著性水平 alpha，拒绝原假设
    reject_null[i] <- test$p.value < alpha
  }
  
  # 计算功效，即拒绝原假设的频率
  power <- mean(reject_null)
  return(power)
}

# 计算不同检验方法的功效
pearson_power <- calculate_power(cor_method = "pearson")
kendall_power <- calculate_power(cor_method = "kendall")
spearman_power <- calculate_power(cor_method = "spearman")

# 打印功效
pearson_power  # Pearson 的功效
kendall_power  # Kendall 的功效
spearman_power  # Spearman 的功效

rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(123)
#  N <- 1000           # 假设总数
#  m <- 10000          # 模拟次数
#  alpha <- 0.1        # 显著性水平
#  
#  # 初始化结果存储
#  fwer_bon <- fdr_bon <- tpr_bon <- numeric(m)
#  fwer_bh <- fdr_bh <- tpr_bh <- numeric(m)
#  
#  for (i in 1:m) {
#    # 原假设下的p值（950个）
#    p_null <- runif(950)
#  
#    # 备择假设下的p值（50个）
#    p_alt <- rbeta(50, 0.1, 1)
#  
#    # 所有假设的p值
#    p_values <- c(p_null, p_alt)
#  
#    # Bonferroni校正
#    bon_p <- p.adjust(p_values, method = "bonferroni")
#    reject_bon <- bon_p < alpha
#    fwer_bon[i] <- sum(reject_bon[1:950]) > 0  # FWER
#    fdr_bon[i] <- sum(reject_bon[1:950]) / max(sum(reject_bon), 1)  # FDR
#    tpr_bon[i] <- sum(reject_bon[951:1000]) / 50  # TPR
#  
#    # B-H校正
#    bh_p <- p.adjust(p_values, method = "BH")
#    reject_bh <- bh_p < alpha
#    fwer_bh[i] <- sum(reject_bh[1:950]) > 0  # FWER
#    fdr_bh[i] <- sum(reject_bh[1:950]) / max(sum(reject_bh), 1)  # FDR
#    tpr_bh[i] <- sum(reject_bh[951:1000]) / 50  # TPR
#  }
#  
#  # 平均结果
#  result <- matrix(c(mean(fwer_bon), mean(fdr_bon), mean(tpr_bon),
#                     mean(fwer_bh), mean(fdr_bh), mean(tpr_bh)),
#                   nrow = 3, byrow = FALSE,
#                   dimnames = list(c("FWER", "FDR", "TPR"),
#                                   c("Bonferroni", "B-H")))
#  print(result)
#  
#  rm(list = ls()) #清除变量
#  

## ----eval = FALSE-------------------------------------------------------------
#  # 载入所需包
#  library(boot)
#  
#  # 故障间隔时间数据
#  failure_times <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
#  
#  # 对于指数分布 Exp(λ)，λ 的最大似然估计为 1/样本均值
#  lambda_hat <- 1 / mean(failure_times)
#  
#  # 定义用于 bootstrap 的统计函数
#  mle_function <- function(data, indices) {
#    # 使用索引 indices 进行抽样
#    sample_data <- data[indices]
#    # 返回 λ 的 MLE
#    return(1 / mean(sample_data))
#  }
#  
#  # 使用 bootstrap 估计偏差和标准误差
#  bootstrap_result <- boot(failure_times, mle_function, R = 1000)
#  
#  # 输出结果
#  cat("MLE of λ:", lambda_hat, "\n")
#  cat("Bootstrap Bias:", mean(bootstrap_result$t) - lambda_hat, "\n")
#  cat("Bootstrap Standard Error:", sd(bootstrap_result$t), "\n")

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(123)
#  # 定义用于 bootstrap 的统计函数（1/λ）
#  mean_time_function <- function(data, indices) {
#    sample_data <- data[indices]
#    return(mean(sample_data))
#  }
#  
#  # 计算不同方法的 95% 置信区间
#  ci_norm <- boot.ci(bootstrap_result, type = "norm")
#  ci_basic <- boot.ci(bootstrap_result, type = "basic")
#  ci_percentile <- boot.ci(bootstrap_result, type = "perc")
#  ci_bca <- boot.ci(bootstrap_result, type = "bca")
#  
#  # 输出置信区间
#  cat("95% Confidence Intervals for 1/λ:\n")
#  cat("Normal:", ci_norm$normal[2:3], "\n")
#  cat("Basic:", ci_basic$basic[4:5], "\n")
#  cat("Percentile:", ci_percentile$percent[4:5], "\n")
#  cat("BCa:", ci_bca$bca[4:5], "\n")
#  
#  rm(list = ls()) #清除变量

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(123)
#  
#  # 参数设定
#  lambda_true <- 2    # 真正的lambda值
#  n_values <- c(5, 10, 20)  # 样本量
#  B <- 1000  # Bootstrap重复次数
#  m <- 1000  # 模拟次数
#  
#  # 存储结果的矩阵
#  results <- data.frame(n = integer(0),
#                        mean_bootstrap = numeric(0),
#                        mean_bias_bootstrap = numeric(0),
#                        mean_bias_theoretical = numeric(0),
#                        se_bootstrap = numeric(0),
#                        se_theoretical = numeric(0))
#  
#  # 循环遍历不同的样本量n
#  for (n in n_values) {
#  
#    # 初始化存储偏差和标准误的向量
#    bootstrap <- numeric(m)
#    bias_bootstrap <- numeric(m)
#    se_bootstrap <- numeric(m)
#  
#    # 模拟过程
#    for (i in 1:m) {
#      # 生成样本数据，来自指数分布，参数lambda_true
#      sample_data <- rexp(n, rate = lambda_true)
#  
#      # 计算MLE的lambda估计值
#      lambda_hat <- 1 / mean(sample_data)
#  
#      # 进行B次Bootstrap抽样
#      bootstrap_estimates <- numeric(B)
#      for (b in 1:B) {
#        bootstrap_sample <- sample(sample_data, size = n, replace = TRUE)
#        bootstrap_estimates[b] <- 1 / mean(bootstrap_sample)
#      }
#  
#      # 计算Bootstrap偏差和标准误
#      bootstrap[i] <- mean(bootstrap_estimates)
#      bias_bootstrap[i] <- mean(bootstrap_estimates) - lambda_hat
#      se_bootstrap[i] <- sd(bootstrap_estimates)
#    }
#  
#    # 理论偏差和标准误
#    theoretical_bias <- lambda_true / (n - 1)
#    theoretical_se <- lambda_true * n / ((n - 1) * sqrt(n - 2))
#  
#    # 计算平均Bootstrap偏差和标准误
#    mean_bootstrap <- mean(bootstrap)
#    mean_bias_bootstrap <- mean(bias_bootstrap)
#    mean_se_bootstrap <- mean(se_bootstrap)
#  
#    # 将结果存储到数据框中
#    results <- rbind(results, data.frame(
#      n = n,
#      mean_bootstrap = mean_bootstrap,
#      mean_bias_bootstrap = mean_bias_bootstrap,
#      mean_bias_theoretical = theoretical_bias,
#      se_bootstrap = mean_se_bootstrap,
#      se_theoretical = theoretical_se
#    ))
#  }
#  
#  # 打印结果
#  print(results)
#  
#  rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
set.seed(123)

library(ggplot2)

Metropolis <- function(m, f, g_sd, sigm) {
  k <- 0
  x <- numeric(m)
  x[1] <- rnorm(1, mean = 0, sd = sigm)  # 初始化采样起点
  u <- runif(m)  # 接受-拒绝随机数

  for (i in 2:m) {
    xt <- x[i - 1]
    y <- rnorm(1, mean = xt, sd = sigm)  # 候选样本
    num <- f(y) * dnorm(xt, mean = y, sd = sigm)
    den <- f(xt) * dnorm(y, mean = xt, sd = sigm)
    
    if (u[i] <= num / den) {
      x[i] <- y
    } else {
      x[i] <- xt
      k <- k + 1
    }
  }
  cat("Reject probability:", k / m, "\n")
  return(x)
}

# 参数设置
m <- 100000
burn <- 1000
sigm <- c(0.05, 0.5, 1, 2, 16)
f <- function(x) { dcauchy(x) }  # 标准柯西分布
g_sd <- 1  # 正态分布标准差

# 运行 Metropolis-Hastings 采样
x <- Metropolis(m, f, g_sd, sigm[4])

DrawQQPlot <- function(x, burn, scale) {
  # 去除烧入期的样本
  x <- sort(x[(burn + 1):length(x)])
  
  # 生成与样本数相同的标准柯西分布样本，并排序
  y <- sort(rcauchy(length(x)))
  
  # 计算分位数
  quantileX <- quantile(x, probs = seq(0, 1, 0.01))
  quantileY <- quantile(y, probs = seq(0, 1, 0.01))
  
  # 绘制 Q-Q 图
  plot(quantileY, quantileX, col = "blue", pch = 16,
       xlim = scale, ylim = scale,
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  
  # 添加参考线
  abline(0, 1, col = "red", lwd = 2)
}

# 设置参数并调用函数
burn <- 1000
scale <- c(-5, 5)  # 可根据需求调整
DrawQQPlot(x, burn, scale)

# 去除烧入期样本
samples <- x[(burn + 1):m]

# 计算生成样本和标准柯西分布的分位数
sample_deciles <- quantile(samples, probs = seq(0.1, 0.9, by = 0.1))
cauchy_deciles <- qcauchy(seq(0.1, 0.9, by = 0.1))

# 比较生成样本的分位数和理论分位数
comparison <- data.frame(
  Decile = seq(0.1, 0.9, by = 0.1),
  Sample_Deciles = sample_deciles,
  Cauchy_Deciles = cauchy_deciles
)

print(comparison)

# 绘制直方图与密度图
ggplot(data.frame(samples = samples), aes(x = samples)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "black", fill = "skyblue") +
  geom_density(color = "red") +
  ggtitle("Histogram of Samples from Cauchy Distribution (after burn-in)") +
  xlab("Value") +
  ylab("Density")
 

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
# 设置参数
a <- 2     # 参数 a
b <- 2     # 参数 b
n <- 10    # 二项分布的样本数

# 设置 Gibbs 采样的初始值和迭代次数
num_iterations <- 5000
samples_x <- numeric(num_iterations)
samples_y <- numeric(num_iterations)
samples_x[1] <- 5   # 初始 x 值
samples_y[1] <- 0.5 # 初始 y 值

# Gibbs 采样过程
for (i in 2:num_iterations) {
  # 给定 y 条件下，从二项分布采样 x
  samples_x[i] <- rbinom(1, n, samples_y[i - 1])
  
  # 给定 x 条件下，从 Beta 分布采样 y
  samples_y[i] <- rbeta(1, samples_x[i] + a, n - samples_x[i] + b)
}

# 绘制样本结果
par(mfrow = c(1, 2))
plot(samples_x, type = "l", col = "blue", main = "样本 x 的轨迹", xlab = "迭代次数", ylab = "x 值")
plot(samples_y, type = "l", col = "red", main = "样本 y 的轨迹", xlab = "迭代次数", ylab = "y 值")

# 计算和展示联合分布的样本
plot(samples_x, samples_y, pch = 16, col = rgb(0, 0, 1, 0.5), 
     main = "联合分布样本 (x, y)", xlab = "x", ylab = "y")

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
# 定义目标分布的密度函数（标准柯西分布）
target_density <- function(x) {
  1 / (pi * (1 + x^2))
}

# Metropolis-Hastings 采样函数
metropolis_hastings <- function(iter, proposal_sd = 1, init_val = 0) {
  samples <- numeric(iter)
  samples[1] <- init_val
  for (i in 2:iter) {
    proposal <- rnorm(1, mean = samples[i - 1], sd = proposal_sd)
    accept_prob <- min(1, target_density(proposal) / target_density(samples[i - 1]))
    if (runif(1) < accept_prob) {
      samples[i] <- proposal
    } else {
      samples[i] <- samples[i - 1]
    }
  }
  return(samples)
}

# Gelman-Rubin 检验函数
Gelman.Rubin <- function(psi) {
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)

  psi.means <- rowMeans(psi)     # 每条链的均值
  B <- n * var(psi.means)        # 链间方差
  psi.w <- apply(psi, 1, "var")  # 链内方差
  W <- mean(psi.w)               # 链内方差的平均值
  v.hat <- W * (n - 1) / n + B / n  # 联合后验分布的估计方差
  r.hat <- v.hat / W             # Gelman-Rubin 统计量
  return(r.hat)
}

# 设置参数
n_samples <- 31000
burn_in <- 1000
num_chains <- 4
proposal_sd <- 1
initial_values <- c(-10, -5, 5, 10)  # 不同的初始值

# 生成多个链
set.seed(12345)
chains <- matrix(0, nrow = num_chains, ncol = n_samples - burn_in)
for (i in 1:num_chains) {
  chain <- metropolis_hastings(n_samples, proposal_sd, init_val = initial_values[i])
  chains[i, ] <- chain[(burn_in + 1):n_samples]
}

# 计算每条链的累积均值
psi <- t(apply(chains, 1, cumsum))
for (i in 1:nrow(psi)) {
  psi[i,] <- psi[i,] / (1:ncol(psi))
}

# 计算 Gelman-Rubin 统计量随采样点数量的变化
R_values <- numeric(n_samples - burn_in )
for (j in 1:(n_samples -  burn_in)) {
  R_values[j] <- Gelman.Rubin(psi[, 1:j])
}

# 绘制 Gelman-Rubin 统计量随采样点数量变化的图像
plot(R_values, type = 'l', col = 'purple', xlab = 'Number of Sampling Points (n)',
     ylab = 'R Statistic', main = 'Gelman-Rubin Diagnostic R vs. Number of Sampling Points')
abline(h = 1.2, col = 'red', lty = 2)  # 添加 R = 1.2 的参考线
legend("topright", legend = c("R Statistic", "R = 1.2"), col = c("purple", "red"), lty = c(1, 2))


## -----------------------------------------------------------------------------

# 绘制每条链的累积均值随采样点数量变化的图像
plot(1:(n_samples - burn_in), psi[1, ], type = "l", col = "blue", ylim = range(psi),
     xlab = "Number of Sampling Points (n)", ylab = "Cumulative Mean",
     main = "Cumulative Mean of Each Chain")
for (i in 2:num_chains) {
  lines(1:(n_samples - burn_in), psi[i, ], col = i)
}
legend("topright", legend = paste("Chain", 1:num_chains), col = 1:num_chains, lty = 1)


## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## ----eval = FALSE-------------------------------------------------------------
#  # 设置 Gibbs 采样器函数
#  gibbs_sampler <- function(num_iterations, n, a, b, init_x, init_y) {
#    samples_x <- numeric(num_iterations)
#    samples_y <- numeric(num_iterations)
#    samples_x[1] <- init_x
#    samples_y[1] <- init_y
#  
#    for (i in 2:num_iterations) {
#      # 给定 y 条件下，从二项分布采样 x
#      samples_x[i] <- rbinom(1, n, samples_y[i - 1])
#  
#      # 给定 x 条件下，从 Beta 分布采样 y
#      samples_y[i] <- rbeta(1, samples_x[i] + a, n - samples_x[i] + b)
#    }
#  
#    return(list(samples_x = samples_x, samples_y = samples_y))
#  }
#  
#  # Gelman-Rubin 检验函数
#  gelman_rubin <- function(chains) {
#    m <- ncol(chains)  # 链的数量
#    n <- nrow(chains)  # 每条链的采样点数量
#  
#    # Step 1: 计算每条链的均值
#    chain_means <- colMeans(chains)
#  
#    # Step 2: 计算链间方差 B
#    overall_mean <- mean(chain_means)
#    B <- n * sum((chain_means - overall_mean)^2) / (m - 1)
#  
#    # Step 3: 计算每条链内的方差 W
#    W <- sum(apply(chains, 2, var)) / m
#  
#    # Step 4: 估计联合后验分布的方差 V_hat
#    V_hat <- (1 - 1/n) * W + (1/n) * B
#  
#    # Step 5: 计算 R 统计量
#    R_hat <- sqrt(V_hat / W)
#  
#    return(R_hat)
#  }
#  
#  # 设置参数
#  a <- 2
#  b <- 2
#  n <- 10
#  num_iterations <- 10000
#  
#  # 生成 3 条链
#  set.seed(123)
#  chain1 <- gibbs_sampler(num_iterations, n, a, b, init_x = 5, init_y = 0.5)
#  chain2 <- gibbs_sampler(num_iterations, n, a, b, init_x = 6, init_y = 0.6)
#  chain3 <- gibbs_sampler(num_iterations, n, a, b, init_x = 4, init_y = 0.4)
#  
#  # 仅对 y 值进行 Gelman-Rubin 检验
#  chains_y <- cbind(chain1$samples_y, chain2$samples_y, chain3$samples_y)
#  
#  # 检验结果
#  R_hat <- gelman_rubin(chains_y)
#  cat("Gelman-Rubin 诊断的 R 统计量:", R_hat, "\n")
#  
#  # 检查 R 统计量是否小于 1.2
#  if (R_hat < 1.2) {
#    cat("链已收敛。\n")
#  } else {
#    cat("链未收敛。\n")
#  }

## ----eval = FALSE-------------------------------------------------------------
#  # 设置 Gibbs 采样器函数
#  gibbs_sampler <- function(num_iterations, n, a, b, init_x, init_y) {
#    samples_x <- numeric(num_iterations)
#    samples_y <- numeric(num_iterations)
#    samples_x[1] <- init_x
#    samples_y[1] <- init_y
#  
#    for (i in 2:num_iterations) {
#      # 给定 y 条件下，从二项分布采样 x
#      samples_x[i] <- rbinom(1, n, samples_y[i - 1])
#  
#      # 给定 x 条件下，从 Beta 分布采样 y
#      samples_y[i] <- rbeta(1, samples_x[i] + a, n - samples_x[i] + b)
#    }
#  
#    return(samples_y)
#  }
#  
#  # 设置参数
#  num_iterations <- 10000
#  n <- 10
#  a <- 2
#  b <- 2
#  
#  # 生成 3 条链
#  set.seed(123)  # 设置随机种子以保持可重复性
#  chain1_y <- gibbs_sampler(num_iterations, n, a, b, init_x = 5, init_y = 0.5)
#  chain2_y <- gibbs_sampler(num_iterations, n, a, b, init_x = 6, init_y = 0.6)
#  chain3_y <- gibbs_sampler(num_iterations, n, a, b, init_x = 4, init_y = 0.4)
#  
#  # 计算每条链的均值
#  chain_means1 <- cumsum(chain1_y) / (1:num_iterations)
#  chain_means2 <- cumsum(chain2_y) / (1:num_iterations)
#  chain_means3 <- cumsum(chain3_y) / (1:num_iterations)
#  
#  # 绘图
#  plot(chain_means1, type = 'l', col = 'blue', ylim = range(c(chain_means1, chain_means2, chain_means3)),
#       xlab = 'Number of Sampling Points (n)', ylab = 'Mean of Samples', main = 'Chain Means vs. Number of Sampling Points')
#  lines(chain_means2, col = 'orange')
#  lines(chain_means3, col = 'green')
#  
#  # 添加最终均值的虚线
#  abline(h = mean(chain1_y), col = 'blue', lty = 2)
#  abline(h = mean(chain2_y), col = 'orange', lty = 2)
#  abline(h = mean(chain3_y), col = 'green', lty = 2)
#  
#  # 添加图例
#  legend("topright", legend = c("Chain 1 Mean", "Chain 2 Mean", "Chain 3 Mean", "Chain 1 Final Mean",
#                                 "Chain 2 Final Mean", "Chain 3 Final Mean"),
#         col = c("blue", "orange", "green", "blue", "orange", "green"),
#         lty = c(1, 1, 1, 2, 2, 2))

## ----eval = FALSE-------------------------------------------------------------
#  # Gelman-Rubin 检验函数
#  Gelman.Rubin <- function(psi) {
#    psi <- as.matrix(psi)
#    n <- ncol(psi)
#    k <- nrow(psi)
#  
#    psi.means <- rowMeans(psi)     # 每条链的均值
#    B <- n * var(psi.means)        # 链间方差
#    psi.w <- apply(psi, 1, "var")  # 每条链内的方差
#    W <- mean(psi.w)               # 链内方差的平均值
#    v.hat <- W * (n - 1) / n + B / n  # 联合后验分布的估计方差
#    r.hat <- v.hat / W             # Gelman-Rubin 统计量
#    return(r.hat)
#  }
#  
#  # Gibbs 采样函数
#  gibbs_sampler <- function(num_iterations, n, a, b, init_x, init_y) {
#    samples_x <- numeric(num_iterations)
#    samples_y <- numeric(num_iterations)
#    samples_x[1] <- init_x
#    samples_y[1] <- init_y
#  
#    for (i in 2:num_iterations) {
#      # 给定 y 条件下，从二项分布采样 x
#      samples_x[i] <- rbinom(1, n, samples_y[i - 1])
#  
#      # 给定 x 条件下，从 Beta 分布采样 y
#      samples_y[i] <- rbeta(1, samples_x[i] + a, n - samples_x[i] + b)
#    }
#  
#    return(samples_y)
#  }
#  
#  # 设置参数
#  a <- 2
#  b <- 2
#  n <- 10
#  num_iterations <- 5000
#  k <- 4  # 生成的链数
#  burn_in <- 1000  # 烧入期
#  
#  # 设置不同链的初始值
#  initial_values <- list(c(5, 0.5), c(6, 0.6), c(4, 0.4), c(7, 0.3))
#  
#  # 生成 k 条链
#  set.seed(12345)
#  chains <- matrix(0, nrow = k, ncol = num_iterations)
#  for (i in 1:k) {
#    init_x <- initial_values[[i]][1]
#    init_y <- initial_values[[i]][2]
#    chains[i, ] <- gibbs_sampler(num_iterations, n, a, b, init_x, init_y)
#  }
#  
#  # 计算每条链的累积均值
#  psi <- t(apply(chains, 1, cumsum))
#  for (i in 1:nrow(psi)) {
#    psi[i,] <- psi[i,] / (1:ncol(psi))
#  }
#  
#  # Gelman-Rubin 检验
#  R_values <- numeric(num_iterations)
#  for (j in (burn_in + 1):num_iterations) {
#    R_values[j] <- Gelman.Rubin(psi[, 1:j])
#  }
#  
#  # 绘制 R 统计量随采样点数量变化的图像
#  plot(R_values[(burn_in + 1):num_iterations], type = 'l', col = 'purple',
#       xlab = 'Number of Sampling Points (n)', ylab = 'R Statistic',
#       main = 'Gelman-Rubin Diagnostic R vs. Number of Sampling Points')
#  abline(h = 1.2, col = 'red', lty = 2)  # 添加 R = 1.2 的参考线
#  legend("topright", legend = c("R Statistic", "R = 1.2"), col = c("purple", "red"), lty = c(1, 2))
#  

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------

# 定义计算第 k 项的函数
compute_kth_term <- function(k, d, a) {
  # 计算范数 ||a||
  norm_a <- norm(a, type = "2")
  
  # 计算第 k 项的分子和分母
  numerator <- (-1)^k * norm_a^(2*k + 2) * gamma((d + 1) / 2) * gamma(k + 3 / 2)
  denominator <- factorial(k) * 2^k * (2 * k + 1) * (2 * k + 2) * gamma(k + d / 2 + 1)
  
  # 返回第 k 项的值
  return(numerator / denominator)
}

# 计算整个和的函数
compute_sum <- function(d, a, tol = 1e-10) {
  sum_value <- 0
  k <- 0
  
  # 迭代求和，直到第 k 项小于容差 tol
  repeat {
    term <- compute_kth_term(k, d, a)
    sum_value <- sum_value + term
    if (abs(term) < tol) break
    k <- k + 1
  }
  
  return(sum_value)
}

# 参数设置
d <- 2
a <- c(1, 2)

# 计算和的值
result <- compute_sum(d, a)
cat("和的值为:", result, "\n")

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
# 加载必要的库
#if(!requireNamespace("stats", quietly = TRUE)) install.packages("stats")
library(stats)

# 定义概率函数
Sk_minus_1 <- function(a, k) {
  threshold <- sqrt(a^2 * (k - 1) / (k - a^2))
  1 - pt(threshold, df = k - 1)
}

Sk <- function(a, k) {
  threshold <- sqrt(a^2 * k / (k + 1 - a^2))
  1 - pt(threshold, df = k)
}

# 定义找到交点的函数
find_intersection <- function(k, tol = 1e-6) {
  # 定义目标函数，求解使 Sk_minus_1 和 Sk 接近的 a 值
  target_function <- function(a) abs(Sk_minus_1(a, k) - Sk(a, k))
  
  # 使用 optimize 函数在 (0, sqrt(k)) 区间内找到最小值
  result <- optimize(target_function, interval = c(0, sqrt(k)), tol = tol)
  a_opt <- result$minimum
  return(a_opt)
}

# 计算并打印 k = 100, 150, 200 对应的交点 A(k)
k_values <- c(100,150,200)
A_k <- sapply(k_values, find_intersection)

# 显示结果
for (i in 1:length(k_values)) {
  cat("k =", k_values[i], ", A(k) =", A_k[i], "\n")
}

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
# 定义目标函数
k=100
ck <- function(a, k) {
    sqrt(a^2 * k / (k + 1 - a^2))
  }
left_integral <- function(a, k) {
    ck_val <- ck(a, k)
      result <- integrate(function(u) (1 + u^2 / (k - 1))^(-k / 2), 0, ck_val - 1)$value
      coef <- 2 * gamma(k / 2) / (sqrt(pi * (k - 1)) * gamma((k - 1) / 2))
      return(coef * result)
    }
  
right_integral <- function(a, k) {
    ck_val <- ck(a, k)
      result <- integrate(function(u) (1 + u^2 / k)^(-(k + 1) / 2), 0, ck_val)$value
      coef <- 2 * gamma((k + 1) / 2) / (sqrt(pi * k) * gamma(k / 2))
      return(coef * result)
    }
target_function <- function(a) {
  (left_integral(a, k) - right_integral(a, k))*1e14
}

# 绘制目标函数的值在区间的变化情况
a_values <- seq(6.7,6.9, length.out = 1000)
target_values <- sapply(a_values, target_function)

plot(a_values, target_values, type = "l", col = "blue", lwd = 2,
     xlab = "a", ylab = "target_function(a)", main = "Plot of target_function(a)")
abline(h = 0, col = "red", lty = 2)  # 添加 y = 0 的水平线


## -----------------------------------------------------------------------------
# 加载所需的包
library(stats)

# 定义方程函数
solve_equation <- function(k) {
  # 定义 c_k 的函数
  ck <- function(a, k) {
    sqrt(a^2 * k / (k + 1 - a^2))
  }
  
  # 定义积分函数
  left_integral <- function(a, k) {
    ck_val <- ck(a, k)
      result <- integrate(function(u) (1 + u^2 / (k - 1))^(-k / 2), 0, ck_val - 1)$value
      coef <- 2 * gamma(k / 2) / (sqrt(pi * (k - 1)) * gamma((k - 1) / 2))
      return(coef * result)
    }
  
  right_integral <- function(a, k) {
    ck_val <- ck(a, k)
      result <- integrate(function(u) (1 + u^2 / k)^(-(k + 1) / 2), 0, ck_val)$value
      coef <- 2 * gamma((k + 1) / 2) / (sqrt(pi * k) * gamma(k / 2))
      return(coef * result)
    }
  
  # 定义求解的目标函数
  target_function <- function(a) {
    (left_integral(a, k) - right_integral(a, k))*1e14
  }
  
  # 使用 uniroot() 求解方程，扩大求解区间
  solution <- uniroot(target_function, interval = c(6.7,6.9))
  return(solution$root)
}

k <- 100
a_value <- solve_equation(k)
cat("当 k =", k, "时，求解出的 a 值为：", a_value, "\n")



## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
# 观测数据
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
tau <- 1  # 截尾点

# 初始化
n <- length(Y)
lambda_est <- 1 / mean(Y)  # 初始估计值

# E-M算法
epsilon <- 1e-6  # 收敛条件
max_iter <- 1000  # 最大迭代次数
for (i in 1:max_iter) {
  # E步：计算未观测部分的期望
  expected_T <- ifelse(Y < tau, Y, tau + 1 / lambda_est)
  
  # M步：更新 lambda 的估计值
  new_lambda_est <- n / sum(expected_T)
  
  # 检查收敛条件
  if (abs(new_lambda_est - lambda_est) < epsilon) {
    break
  }
  lambda_est <- new_lambda_est
}

# 输出E-M算法估计结果
cat("E-M算法估计的lambda:", lambda_est, "\n")

# 直接的观测数据MLE估计
lambda_mle <- 1 / mean(Y)
cat("观测数据的MLE估计lambda:", lambda_mle, "\n")
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
# 加载boot包
library(boot)

# 目标函数系数
a <- c(4, 2, 9)

# 约束矩阵（注意负号，因为simplex默认使用 <= 形式的约束）
A1 <- rbind(c(2, 1, 1), c(1, -1, 3))

# 约束条件的右侧值
b1 <- c(2, 3)

# 求解最小化问题
result <- simplex(a = a, A1 = A1, b1 = b1, maxi = FALSE)

# 输出结果
result


## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
# 习题 3
# 定义公式列表
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 使用for循环拟合线性模型
models_for <- list()
for (i in seq_along(formulas)) {
  models_for[[i]] <- lm(formulas[[i]], data = mtcars)
}

# 使用lapply拟合线性模型
models_lapply <- lapply(formulas, function(f) lm(f, data = mtcars))

# 习题 4
# 生成bootstrap样本
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), replace = TRUE)
  mtcars[rows, ]
})

# 使用for循环拟合模型 mpg ~ disp
models_boot_for <- list()
for (i in seq_along(bootstraps)) {
  models_boot_for[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
}

# 使用lapply拟合模型 mpg ~ disp
models_boot_lapply <- lapply(bootstraps, function(data) lm(mpg ~ disp, data = data))

# 习题 5
# 定义提取 R^2 的函数
rsq <- function(mod) summary(mod)$r.squared

# 提取习题 3 中的模型的 R^2
rsq_for <- sapply(models_for, rsq)
rsq_lapply <- sapply(models_lapply, rsq)

# 提取习题 4 中的 bootstrap 模型的 R^2
rsq_boot_for <- sapply(models_boot_for, rsq)
rsq_boot_lapply <- sapply(models_boot_lapply, rsq)

# 输出结果
list(
  rsq_for = rsq_for,
  rsq_lapply = rsq_lapply,
  rsq_boot_for = rsq_boot_for,
  rsq_boot_lapply = rsq_boot_lapply
)

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
# 生成100次t检验结果
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

# 使用sapply和匿名函数提取每次检验的p值
p_values_anonymous <- sapply(trials, function(test) test$p.value)

# 直接使用[[去除匿名函数，提取每次检验的p值
p_values_direct <- sapply(trials, `[[`, "p.value")

# 输出结果
p_values_anonymous
p_values_direct

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
# 定义一个自定义的并行迭代函数
my_lapply_variant <- function(X, FUN, FUN.VALUE, simplify = FALSE, ...) {
  # 使用 Map() 并行应用函数 FUN 到列表 X 中的每个元素
  results <- Map(function(x) vapply(x, FUN, FUN.VALUE, ...), X)
  
  # 如果 simplify 参数为 TRUE，则将结果简化为数组
  if (simplify) {
    return(simplify2array(results))
  }
  
  # 否则返回结果列表
  results
}
# 定义测试列表
testlist <- list(iris, mtcars, cars)
# 使用 my_lapply_variant 计算每列的均值
lapply(testlist, function(x) vapply(x, mean, numeric(1)))
result <- my_lapply_variant(testlist, mean, numeric(1))
print(result)


## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
fast_chisq_test <- function(x, y) {
  # 检查输入是否为数值型向量且无缺失值
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("输入必须是数值型向量")
  }
  if (anyNA(x) || anyNA(y)) {
    stop("输入不能包含缺失值")
  }
  
  # 创建列联表
  m <- rbind(x, y)
  margin1 <- rowSums(m)  # 计算行总和
  margin2 <- colSums(m)  # 计算列总和
  n <- sum(m)  # 总和
  me <- tcrossprod(margin1, margin2) / n  # 计算期望频数

  # 计算卡方统计量
  x_stat <- sum((m - me)^2 / me)

  # 计算自由度
  dof <- (length(margin1) - 1) * (length(margin2) - 1)

  # 计算p值
  p <- pchisq(x_stat, df = dof, lower.tail = FALSE)

  # 返回结果
  return(list(x_stat = x_stat, df = dof, `p-value` = p))
}

# 使用示例
a <- 12:16
b <- c(11, 13, 15, 17, 19)

# 使用我们改进后的 fast_chisq_test
fast_chisq_result <- fast_chisq_test(a, b)
print(fast_chisq_result)
# 使用原本的 fast_chisq_test
m_test <- cbind(a, b)
chisq_result <- chisq.test(m_test)
print(chisq_result)

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
fast_table <- function(x, y) {
  # 确保 x 和 y 是整数向量
  if (!is.integer(x)) x <- as.integer(x)
  if (!is.integer(y)) y <- as.integer(y)
  
  # 获取 x 和 y 的所有唯一值
  levels_x <- unique(x)
  levels_y <- unique(y)
  
  # 创建一个空的矩阵
  result_matrix <- matrix(0, nrow = length(levels_x), ncol = length(levels_y))
  
  # 为每个值找到索引位置
  x_index <- match(x, levels_x)
  y_index <- match(y, levels_y)
  
  # 直接通过索引填充矩阵
  result_matrix[cbind(x_index, y_index)] <- result_matrix[cbind(x_index, y_index)] + 1
  
  # 将矩阵命名
  dimnames(result_matrix) <- list(levels_x, levels_y)
  
  return(result_matrix)
}

# 使用示例
x <- 12:16
y <- c(11, 13, 15, 17, 19)

# 调用快速版的 table()
fast_table_result <- fast_table(x, y)
print(fast_table_result)
table(x,y)


## -----------------------------------------------------------------------------
# 定义加速版的 table 函数
fast_table <- function(x, y) {
  # 确保 x 和 y 是整数向量
  if (!is.integer(x)) x <- as.integer(x)
  if (!is.integer(y)) y <- as.integer(y)
  
  # 获取 x 和 y 的所有唯一值
  levels_x <- unique(x)
  levels_y <- unique(y)
  
  # 初始化空矩阵
  result_matrix <- matrix(0, nrow = length(levels_x), ncol = length(levels_y))
  
  # 创建一个元素的索引映射，方便查找
  x_index <- match(x, levels_x)
  y_index <- match(y, levels_y)
  
  # 填充频数矩阵
  for (i in 1:length(x)) {
    result_matrix[x_index[i], y_index[i]] <- result_matrix[x_index[i], y_index[i]] + 1
  }
  
  # 将矩阵命名
  dimnames(result_matrix) <- list(levels_x, levels_y)
  
  return(result_matrix)
}

# 自定义的卡方检验
fast_chisq_test <- function(x, y) {
  # 使用加速版的 table 创建列联表
  tbl <- fast_table(x, y)
  
  # 计算观测频数
  O <- as.vector(tbl)
  
  # 计算期望频数
  margin1 <- rowSums(tbl)
  margin2 <- colSums(tbl)
  n <- sum(tbl)
  E <- outer(margin1, margin2, FUN = "*") / n
  
  # 计算卡方统计量
  x_stat <- sum((O - E)^2 / E)
  
  # 计算自由度
  dof <- (nrow(tbl) - 1) * (ncol(tbl) - 1)
  
  # 计算p值
  p <- pchisq(x_stat, df = dof, lower.tail = FALSE)
  
  # 返回结果
  return(list(x_stat = x_stat, df = dof, `p-value` = p))
}

# 使用示例
a <- 12:16
b <- c(11, 13, 15, 17, 19)

# 使用加速版的卡方检验
fast_chisq_result <- fast_chisq_test(a, b)
print(fast_chisq_result)
chisq_result <- chisq.test(a, b)
print(chisq_result)

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
options(warn = -1)  # 关闭所有警告(因为使用的R版本较低，调用一些R包时会显示警告)
rm(list = ls()) #清除变量

## -----------------------------------------------------------------------------
library(Rcpp)
library(SA24204158)

# 设定参数
n_iter <- 10000  # 迭代次数
n <- 10          # Binomial 的参数
a <- 2           # Beta 分布的第一个参数
b <- 2           # Beta 分布的第二个参数

# 调用 Gibbs 采样器
set.seed(123)
samples <- gibbs_sampler(n_iter, n, a, b)

# 转换为数据框，方便可视化
samples_df <- as.data.frame(samples)
colnames(samples_df) <- c("x", "y")

# 可视化
library(ggplot2)
ggplot(samples_df, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  labs(title = "Gibbs 采样结果", x = "x", y = "y")

## -----------------------------------------------------------------------------
# 设置参数
a <- 2
b <- 2
n <- 10
num_iterations <- 5000

# 使用 Rcpp 的 Gibbs 采样器生成样本
set.seed(123)
cpp_samples <- gibbs_sampler(num_iterations, n, a, b)

# 使用 R 实现的 Gibbs 采样器生成样本
set.seed(123)
samples_x_r <- numeric(num_iterations)
samples_y_r <- numeric(num_iterations)
samples_x_r[1] <- 5
samples_y_r[1] <- 0.5

for (i in 2:num_iterations) {
  samples_x_r[i] <- rbinom(1, n, samples_y_r[i - 1])
  samples_y_r[i] <- rbeta(1, samples_x_r[i] + a, n - samples_x_r[i] + b)
}

# 使用 qqplot 比较 x 和 y 的分布
par(mfrow = c(1, 2))
qqplot(cpp_samples[, 1], samples_x_r, main = "Rcpp vs R (x)", xlab = "Rcpp x", ylab = "R x")
abline(0, 1, col = "red", lwd = 2)

qqplot(cpp_samples[, 2], samples_y_r, main = "Rcpp vs R (y)", xlab = "Rcpp y", ylab = "R y")
abline(0, 1, col = "red", lwd = 2)


## -----------------------------------------------------------------------------
library(microbenchmark)

# 定义 R 实现的 Gibbs 采样器为一个函数
gibbs_r <- function(num_iterations, n, a, b) {
  samples_x <- numeric(num_iterations)
  samples_y <- numeric(num_iterations)
  samples_x[1] <- 5
  samples_y[1] <- 0.5
  for (i in 2:num_iterations) {
    samples_x[i] <- rbinom(1, n, samples_y[i - 1])
    samples_y[i] <- rbeta(1, samples_x[i] + a, n - samples_x[i] + b)
  }
  list(x = samples_x, y = samples_y)
}

# 比较时间
benchmark_result <- microbenchmark(
  Rcpp = gibbs_sampler(num_iterations, n, a, b),
  R = gibbs_r(num_iterations, n, a, b),
  times = 20
)

print(benchmark_result)

## -----------------------------------------------------------------------------
rm(list = ls()) #清除变量

