#パッケージの読み込み
library(rstan)
install.packages("brms",
                 repos = c("http://rstudio.org/_packages",
                           "http://cran.rstudio.com"))
library(brms)

#計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#予測のための説明変数
new_data <- data.frame(temperature = 20)

#回帰直線の信頼区間付きの予測値
fitted(simple_lm_brms, new_data)

#予測区間付きの予測値
set.seed(1)
predict(simple_lm_brms, new_data)

#MCMCサンプルを取り出す
mcmc_sample <- as.mcmc(simple_lm_brms, combine_chains = TRUE)
head(mcmc_sample, n = 2)

#指定されたパラメータ別に保存しておく
mcmc_b_Intercept   <- mcmc_sample[,"b_Intercept"]
mcmc_b_temperature <- mcmc_sample[,"b_temperature"]
mcmc_sigma         <- mcmc_sample[,"sigma"]

saigen_fitted <- mcmc_b_Intercept + 20 * mcmc_b_temperature

#fittedの再現
mean(saigen_fitted)
quantile(saigen_fitted, probs = c(0.025, 0.975))
fitted(simple_lm_brms, new_data)

#予測分布のMCMCサンプルを得る
set.seed(1)
saigen_predict <- do.call(
  rnorm, 
  c(4000, list(mean = saigen_fitted, sd = mcmc_sigma))
)

#predictの再現
mean(saigen_predict)
quantile(saigen_predict, probs = c(0.025, 0.975))
set.seed(1)
predict(simple_lm_brms, data.frame(temperature = 20))

#回帰直線の95%ベイズ信用区間付きのグラフ
eff <- marginal_effects(simple_lm_brms)
plot(eff, points = TRUE)

#95%予測区間付きのグラフ
set.seed(1)
eff_pre <- marginal_effects(simple_lm_brms, method = "predict")
plot(eff_pre, points = TRUE)





