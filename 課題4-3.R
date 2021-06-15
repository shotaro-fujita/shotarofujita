#パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#分析対象のデータ
sales_weather <- read.csv("3-6-1-beer-sales-3.csv")
head(sales_weather, 3)

#データの要約
summary(sales_weather)

ggplot(data = sales_weather, mapping = aes(x = weather, y = sales)) +
  geom_violin() +
  geom_point(aes(color = weather)) +
  labs(title = "ビールの売り上げと天気の関係")

#分散分析モデルを作る
anova_brms <- brm(
  formula = sales ~ weather,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = sales_weather,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

anova_brms

#推定された天気別の平均売り上げのグラフ
eff <- marginal_effects(anova_brms)
plot(eff, points = FALSE)

