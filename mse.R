# 数値実験：正規母集団の母分散の推定
# 不偏分散とMSE最小分散に対するbias, 標準誤差, 平均2乗誤差の比較
library(purrr)
library(dplyr)
library(ggplot2)

# MSE最小分散
var_mse <- function(x){sum((x - mean(x))^2) / (length(x) + 1)}

# 数値実験のパラメータ
n_iter <- 10^5    # 実験の繰り返し数
size <- 25    # サンプルサイズ

# 正規分布の乱数生成と不偏分散, MSE最小分散の計算
args <- list(n = rep(size, each = n_iter),
             mean = rep(0.0, each = n_iter),
             sd = rep(10.0, each = n_iter))
x <- args %>% pmap(rnorm)
dat <- tibble(vars_umvue = vars_umvue <- x %>% map_dbl(var),
               vars_mse = vars_mse <- x %>% map_dbl(var_mse)) 

# 推定量のヒストグラム
ggplot(data = dat %>% gather(), mapping = aes(x = value, fill = key)) + 
  geom_histogram(alpha = 0.25, position = "identity", stat = "density") + 
  xlab("母分散の推定値") +
  ylab("密度")

# MSE = bias^2 + std_error^2
dat %>%
  summarize(bias_umvue = 100.0 - mean(vars_umvue),
            bias_mse = 100.0 - mean(vars_mse),
            error_umvue = sd(vars_umvue),
            error_mse = sd(vars_mse)) %>%
  mutate(mse_umvue = bias_umvue^2 + error_umvue^2, mse_mse = bias_mse^2 + error_mse^2)
