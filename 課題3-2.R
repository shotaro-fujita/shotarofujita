#第3章
#分析の準備
#パッケージの読み込み
library(rstan)
library(bayesplot)

#計算の高速化
rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

#分析対象のデータ
file_beer_sample_2<-read.csv("3-2-1-beer-sales-2.csv")

#サンプルサイズ
sample_size<-nrow(file_beer_sales_2)

temperature_pred<-15:30
temperature_pred

#listにまとめる
data_list_pred<-list(
  N=sample_size,
  sales=file_beer_sales_2$sales,
  temperature=file_beer_sales_2$temperature,
  N_pred=length(temperature_pred),
  temperature_pred=temperature_pred
)

mcmc_result_pred<-stan(
  file="3-3-1-simple-lm-pred.stan",
  data=data_list_pred,
  seed=1
)
print(mcmc_result_pred,probs=c(0.025,0.5,0.975))
