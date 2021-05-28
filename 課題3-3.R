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

#MCMCサンプルの抽出
mcmc_sample_pred<-rstan::extract(mcmc_result_pred,
                                 permuted=FALSE)

#95%区間の比較
mcmc_intervals(
  mcmc_sample_pred,
  regex_pars=c("sales_pred."),
  prob=0.8,
  prob_outer = 0.95
)

mcmc_intervals(
  mcmc_sample_pred,
  pars=c("mu_pred[1]","sales_pred[1]"),
  prob=0.8,
  prob_outer = 0.95
)

mcmc_areas(
  mcmc_sample_pred,
  pars=c("sales_pred[1]","sales_pred[16]"),
  prob=0.6,
  prob_outer = 0.99
)