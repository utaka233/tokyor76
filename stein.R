# 必要なデータのロード
library(Lahman)
library(dplyr)
library(ggplot2)

# データの準備
# 元データの確認
as_tibble(Batting)
# ground truthの作成とplayer listの作成
ground_truth <-
  as_tibble(Batting) %>%
  group_by(playerID) %>%
  summarise(hit = sum(H), AB = sum(AB)) %>%
  filter(AB > 1000) %>%
  sample_n(size = 30) %>%
  mutate(truth = hit / AB)
players <- ground_truth %>% pull(playerID)
# 推定する際のデータの作成
dat <-
  as_tibble(Batting) %>%
  filter(playerID %in% players) %>%
  group_by(playerID) %>%
  do(top_n(x = ., n = 1)) %>%
  summarise(hit = sum(H), AB = sum(AB))

# 推定
# 最尤推定
dat %>%
  mutate(MLE = hit / AB) %>%
  inner_join(y = ground_truth, by = "playerID") %>%
  select(playerID, MLE, truth)
# Stein型推定
stein_estimator <- function(mle, total, size, ground_mean){    # x : vector of hits, total : sum of ABs, n : sample size
  sigma <- ground_mean * (1 - ground_mean) / total
  JS <- ground_mean +
    (1 - (size - 3) * sigma / sum((mle - ground_mean)^2)) * (mle - ground_mean)
  return(JS)
}
# Stein推定量
result <-
  dat %>%
  mutate(MLE = hit / AB) %>%
  mutate(ground_mean = mean(MLE)) %>%
  mutate(stein = stein_estimator(MLE, median(dat$AB), nrow(dat), ground_mean)) %>%
  inner_join(y = ground_truth, by = "playerID") %>%
  select(playerID, ground_mean, MLE, stein, truth)
result

# 推定量の評価
result %>%
  summarise(mse_MLE = sum((MLE - truth)^2) / nrow(result),
            mse_stein = sum((stein - truth)^2) / nrow(result)) %>%
  mutate(efficiency = mse_stein / mse_MLE)
