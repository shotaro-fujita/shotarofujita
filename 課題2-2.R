#目標分布を((sqrt(2*pi))^(-1))*exp(-((x-5)^2)/2)とする。

target=function(x){
  ((sqrt(2*pi))^(-1))*exp(-((x-100)^2)/2)
}

# 初期値
s0 = 100
# 初期値を目標分布へつっこむ
pi0 =target(s0)
s = c(s0)
for(n in 1:10000){
  # 正規分布でランダムウォーク連鎖を作成
  s1 = s0 + rnorm(1)
  # 一様分布
  u = runif(1)
  # 候補の値の代入
  pi1 = target(s1)
  # 採択確率を求める
  alpha = min(1, pi1/pi0)
  #採択について
  if(alpha>u){
    # s1をs0に上書き
    s0 = s1
    # pi0に候補値の尤度を代入
    pi0 = pi1
  }
  # s0をサンプル集合sに追加
  s <- c(s, s0)
}

plot(s)

s = tail(s, length(s)-1000)

plot(s)

class(target)

hist(s)

kernel_density<-density(s)
plot(kernel_density)