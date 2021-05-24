# パッケージの読み込み
library(rstan)


library(ggfortify)


# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# 分析対象のデータ
file_beer_sales_1 <- read.csv("2-4-1-beer-sales-1.csv")

# データの確認
head(file_beer_sales_1, n = 3)


# サンプルサイズ
sample_size <- nrow(file_beer_sales_1)
sample_size

# listにまとめる
data_list <- list(sales = file_beer_sales_1$sales, N = sample_size)
data_list


# 乱数の生成
mcmc_result <- stan(
  file = "2-4-1-calc-mean-variance.stan", # stanファイル
  data = data_list,                       # 対象データ
  seed = 1,                               # 乱数の種
  chains = 4,                             # チェーン数
  iter = 2000,                            # 乱数生成の繰り返し数
  warmup = 1000,                          # バーンイン期間
  thin = 1                                # 間引き数(1なら間引き無し) 
)

# 結果の表示
print(
  mcmc_result,                   # MCMCサンプリングの結果
  probs = c(0.025, 0.5, 0.975)   # 中央値と95%信用区間を出力
)


# トレースプロット(バーンイン期間無し)
traceplot(mcmc_result)

# トレースプロット(バーンイン期間あり)
traceplot(mcmc_result, inc_warmup = T)


# MCMCサンプルの抽出
mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)

# クラス
class(mcmc_sample)

# 次元数
dim(mcmc_sample)

# 各々の名称
dimnames(mcmc_sample)

# パラメタmuの1回目のチェーンのMCMCサンプルのburn-in後の最初のMCMCサンプル
mcmc_sample[1,"chain:1","mu"]

# パラメタmuの1回目のチェーンのMCMCサンプル
mcmc_sample[,"chain:1","mu"]

# パラメタmuの1回目のチェーンのMCMCサンプルの個数
length(mcmc_sample[,"chain:1","mu"])

# 4つのチェーンすべてのMCMCサンプルの個数
length(mcmc_sample[,,"mu"])

# 4つのチェーンがあるので、1000iter×4ChainのMatrix
dim(mcmc_sample[,,"mu"])
class(mcmc_sample[,,"mu"])

# ベクトルにする
mu_mcmc_vec <- as.vector(mcmc_sample[,,"mu"])

# 事後中央値
median(mu_mcmc_vec)

# 事後期待値
mean(mu_mcmc_vec)

# 95%ベイズ信用区間
quantile(mu_mcmc_vec, probs = c(0.025, 0.975))

# 参考
print(
  mcmc_result,                   # MCMCサンプリングの結果
  probs = c(0.025, 0.5, 0.975)   # 事後分布の四分位点を出力
)

library(ggfortify)
autoplot(ts(mcmc_sample[,,"mu"]), 
         facets = F,  # 4つのChainをまとめて1つのグラフにする
         ylab = "mu", # y軸ラベル
         main = "トレースプロット")

# データの整形
mu_df <- data.frame(
  mu_mcmc_sample = mu_mcmc_vec
)

# 図示
ggplot(data = mu_df, mapping = aes(x = mu_mcmc_sample)) +
  geom_density(size = 1.5)





