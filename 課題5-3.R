# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# カテゴリ×数量：モデル化 

# 分析対象のデータ
interaction_2 <- read.csv("3-10-2-interaction-2.csv")
head(interaction_2, n = 3)

# データの要約
summary(interaction_2)

# 参考：デザイン行列の作成
model.matrix(sales ~ publicity * temperature, interaction_2)

# モデル化
interaction_brms_2 <- brm(
  formula = sales ~ publicity * temperature,
  family = gaussian(link = "identity"),
  data = interaction_2,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)


# MCMCの結果の確認
interaction_brms_2

# 参考：事後分布の図示
plot(interaction_brms_2)
# カテゴリ×数量：係数の解釈 

# 交互作用の効果の確認

# 説明変数を作る
newdata_2 <- data.frame(
  publicity   = rep(c("not", "to_implement"), each = 2),
  temperature = c(0,10,0,10)
)
newdata_2
# 予測
round(fitted(interaction_brms_2, newdata_2), 2)
# カテゴリ×数量：モデルの図示 

# 回帰直線の図示
eff_2 <- marginal_effects(interaction_brms_2,
                          effects = "temperature:publicity")
plot(eff_2, points = T)
