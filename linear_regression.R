# library
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(gridExtra)
library(showtext)
library(ggplot2)

# 폰트 불러오기
showtext_auto()
font_add_google('Noto Sans KR', 'notosanskr')

# 데이터 불러오기
ins <- read.csv("./insurance.csv")
str(ins)

# 요인으로 변환
ins$age <- as.numeric(ins$age)
ins$sex <- as.factor(ins$sex)
ins$children <- as.factor(ins$children)
ins$smoker <- as.factor(ins$smoker)
ins$region <- as.factor(ins$region)
str(ins)


# 성별(sex) 나누기
male <- ins[ins$sex == "male", ] # 남자
female <- ins[ins$sex == "female", ] # 여자


# bmi수치(bmi) 나누기 - 서양 국가에서는 bmi 30 이상을 비만으로 정의
standard <- ins[ins$bmi < 30, ] # 표준(bmi 30 미만)
obesity <- ins[ins$bmi >= 30, ] # 비만(bmi 30 이상)


# 흡연여부(smoker) 나누기
yes <- ins[ins$smoker == "yes", ] # 흡연자
no <- ins[ins$smoker == "no", ] # 비흡연자


# 상관계수 구하기 위해 as.numeric()
ins$sex <- as.numeric(ins$sex)
ins$children <- as.numeric(ins$children)
ins$smoker <- as.numeric(ins$smoker)
ins$region <- as.numeric(ins$region)


# 상관계수
cor.test(ins$age, ins$charges) # 0.2990082
cor.test(ins$sex, ins$charges) # 0.05729206 
cor.test(ins$bmi, ins$charges) # 0.198341
cor.test(ins$children, ins$charges) # 0.06799823
cor.test(ins$smoker, ins$charges) # 0.7872514
cor.test(ins$region, ins$charges) # -0.006208235


# 그래프 그리기 위해 as.factor
ins$sex <- as.factor(ins$sex)
ins$children <- as.factor(ins$children)
ins$smoker <- as.factor(ins$smoker)
ins$region <- as.factor(ins$region)
str(ins)


# 흡연여부에 따른 나이에 대한 보험금
ggplot(ins, aes(x = age, y = charges, fill = smoker)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("나이에 따른 보험금") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "나이", y = "보험금")

# 성별 구분 시각화 - 성별(sex)에 대해
g <-ggplot(ins, aes(x = sex, y = charges, fill = sex)) +
  geom_bar(stat = "identity")
g <- g + ggtitle("성별에 따른 보험금") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "성별", y = "보험금")
g


# 표준, 비만 구분 시각화 - bmi에 대해, 흡연여부(smoker)에 따른 
g_stan <- ggplot(standard, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(size = 2, color = "burlywood") +
  stat_smooth(method = "lm") +
  ggtitle("흡연여부에 따른 \n bmi수치가 표준인 사람의 보험금") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "bmi수치", y = "보험금")

g_obes <- ggplot(obesity, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(size = 2, color = "burlywood") +
  stat_smooth(method = "lm") +
  ggtitle("흡연여부에 따른 \n bmi수치가 비만인 사람의 보험금") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "bmi수치", y = "보험금")

grid.arrange(g_stan, g_obes, ncol = 2, nrow = 1)
## bmi수치가 증가하면 보험금도 증가하는 양적 선형관계를 띄고 있다
## 성별에 상관없이 흡연자라면 bmi수치가 높을수록 보험금에 영향을 더 많이 준다.


# 자녀 수(children) 구분 시각화
gg <- ggplot(ins, aes(x = children, y = charges, fill = children)) +
  geom_boxplot(outlier.color = 'red') +
  stat_summary(fun.y = "mean",
               geom = "point",
               shape = 22,
               size = 3,
               fill = "blue")
gg <- gg + ggtitle("자녀 수에 따른 보험금") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "자녀 수", y = "보험금") 
gg


# 흡연여부(smoker)에 대해
g_male <- ggplot(male, aes(x = smoker, y = charges)) +
  geom_boxplot(outlier.color = 'red') +
  stat_summary(fun.y = "mean",
               geom = "point",
               shape = 22,
               size = 3,
               fill = "blue")
g_male <- g_male + ggtitle("남성 흡연유무에 대한 보험금") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "흡연 유무", y = "보험금")

g_female <- ggplot(female, aes(x = smoker, y = charges)) +
  geom_boxplot(outlier.color = 'red') +
  stat_summary(fun.y = "mean",
               geom = "point",
               shape = 22,
               size = 3,
               fill = "blue")
g_female <- g_female + ggtitle("여성 흡연유무에 대한 보험금") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "흡연 유무", y = "보험금")

grid.arrange(g_male, g_female, ncol = 2, nrow = 1)
## 흡연 여부의 경우는 비흡연자보다 흡연자가 더 많은 보험금을 낸다.
## 남성이 여성보다 근소한 차이로 더 많은 보험금을 낸다.


# 지역(region) 구분 시각화
ggg <- ggplot(ins, aes(x = region, y = charges, fill = region)) +
  geom_boxplot(outlier.color = 'red') +
  stat_summary(fun.y = "mean",
               geom = "point",
               shape = 22,
               size = 3,
               fill = "blue")
ggg <- ggg + ggtitle("지역에 따른 보험금") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "지역", y = "보험금") 
ggg
## southeast에 사는 사람들이 평균적으로 가장 보험금이 높았다.
## 이상치는 네 개 지역 모두에서 많이 나타났다.


# 수치형 설명변수 간 상관관계 확인
ins$children <- as.numeric(ins$children)
ins$smoker <- as.numeric(ins$smoker)
ins$sex <- as.numeric(ins$sex)
ins$region <- as.numeric(ins$region)

library(corrplot)
corrplot(cor(ins))


# train, test set 나누기
nrow(ins)*0.7 # = 936.6

set.seed(14)
samp <- sample(1:nrow(ins), 936)

train <- ins[samp, ]
test <- ins[-samp, ]


# 모든 설명변수를 넣은 다중회귀분석
options(scipen = 100)

fit.all <- lm(charges ~ ., data = train)
summary(fit.all) # .은 모든 변수가 됨

# 변수선택
# 상수항만 넣은 회귀분석
fit.0 <- lm(charges ~ 1, data = train)
fit.0 # 1은 상수항만 변수로 취급함


# 전진선택법
# 아무 변수없는 회귀모형으로 시작
# 그 뒤에 F통계량이 큰 변수부터 선택
fit.forw <- step(fit.0, scope = list(lower = fit.0, upper = fit.all),
                 direction = "forward", trace = F)
s.f <- summary(fit.forw)
s.f


# 후진제거법
# 모든 독립변수를 포함하는 회귀모형을 최소제곱법 등의 방법으로 적합시킴
# 그 다음 가장 작은 영향을 미치는 독립변수의 회귀계수에 대한 검정 실시
# 기각시키지 못 하면 full모형이 최종으로 결정
# 기각시키는 경우 해당 변수를 제거하고 다시 모형을 적합시킨 뒤 반복
fit.back <- step(fit.all, direction = "backward", trace = F)
s.b <- summary(fit.back)
s.b


# 단계적선택법
# 전진선택법과 후진제거법을 번갈아 진행
# 변수선택 방법 중 가장 많이 사용됨
fit.step <- step(fit.all, direction = "both",
                 scope = list(lower = fit.0, upper = fit.all))
s.s <- summary(fit.step)
s.s


# 결과비교
# 이상치를 제거한 결정계수
s.f$adj.r.squared # 전진선택법 # 0.7415975
s.b$adj.r.squared # 후진제거법 # 0.7415975
s.s$adj.r.squared # 단계적선택법 # 0.7415975

# 모델 피팅이 잘 되었는지에 대한 수치
# 이탈 정도를 의미하는데, 작을수록 좋음
# 모델이 적합됐는지를 평가할 수 있음
deviance(fit.forw) # 33208899090
deviance(fit.back) # 33208899090
deviance(fit.step) # 33208899090


# 최종모델선정
# 선택적단계법 선정
fit.f <- fit.step
summary(fit.f)


# 다중공선성 확인 및 최종 모델 재선정
library(car) # vif() 내장
vif(fit.f)
# vif() => 다중공선성 # 10이상이면 다중공선성이 존재한다고 간주
fit.f2 <- lm(charges ~ age + bmi + children + smoker, data = train)
summary(fit.f2)
vif(fit.f2) # 10보다 전부 작기 때문에 문제없음

# 추정된 회귀식에 test set을 넣은 결과 확인하기
# predict() : lm(), glm()과 같은 명령문으로 얻어낸 회귀선으로
# 주어준 x값에 해당하는 새로운 y값을 predict하는 커맨드
# newdata에 사용할 x값을 지정할 때에는 data.frame의 colname을
# 회귀분석에서 사용한 설명변수 명과 동일하게 설명해야
# 오류가 발생하지 않음

# 신뢰구간
conf <- predict(fit.f2, newdata = test, interval = "confidence")
head(conf)
conf2 <- as.data.frame(conf)
conf2$act <- test$charges
conf2$YN <- ifelse(conf2$fit >= conf2$lwr &
                     conf2$fit <= conf2$upr, 1, 0)
head(conf2)
sum(conf2$YN) # [1] 402
sum(conf2$YN) / nrow(test) # [1]
confint(fit.f2)


# 예측구간
pred <- predict(fit.f2, newdata = test, interval = "prediction")
pred2 <- as.data.frame(pred)
pred2$act <- test$charges
pred2$YN <- ifelse(pred2$fit >= pred2$lwr &
                     pred2$fit <= pred2$upr, 1, 0)
# lwr과 upr은 각각 신뢰 구간의 하한과 상한 값을 의미
head(pred2) 
sum(pred2$YN) # [1] 402
sum(pred2$YN) / nrow(test) # [1] 1


# 그래프
model <- lm(charges ~ age, ins)
model

predict(model, newdata = test)
predict(model, newdata = test, interval = "prediction")

pred.int <- predict(model, interval = "prediction")
mydata <- cbind(ins, pred.int)

pr1 <- ggplot(mydata, aes(x = age, y = charges)) +
  geom_point() +
  stat_smooth(method = "lm") +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  ggtitle("나이에 따른 보험금 예측치") +                                                                             
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "나이", y = "보험금") 
pr1

# 그래프
model <- lm(charges ~ bmi, ins)
model

predict(model, newdata = test)
predict(model, newdata = test, interval = "prediction")

pred.int <- predict(model, interval = "prediction")
mydata <- cbind(ins, pred.int)

pr2 <- ggplot(mydata, aes(x = bmi, y = charges)) +
  geom_point() +
  stat_smooth(method = "lm") +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  ggtitle("bmi에 따른 보험금 예측치") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "bmi수치", y = "보험금") 
pr2

# 그래프
model <- lm(charges ~ children, ins)
model

predict(model, newdata = test)
predict(model, newdata = test, interval = "prediction")

pred.int <- predict(model, interval = "prediction")
mydata <- cbind(ins, pred.int)

pr3 <- ggplot(mydata, aes(x = children, y = charges)) +
  geom_point() +
  stat_smooth(method = "lm") +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  ggtitle("자녀 수에 따른 보험금 예측치") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "자녀 수", y = "보험금") 
pr3

grid.arrange(pr1, pr2, pr3, ncol = 2, nrow = 2)

# 결정계수
# age
summary(aov(charges ~ age, ins)) # pr(>F)가 유의수준보다 작아서 유의함
SST <- sum((ins$charges - mean(ins$age))^2)
SSE <- sum((fit.f2$fitted.values - mean(ins$charges))^2)
SSE/SST # [1] 0.2228505

# bmi
summary(aov(charges ~ bmi, ins)) # pr(>F)가 유의수준보다 작아서 유의함
SST <- sum((ins$charges - mean(ins$bmi))^2)
SSE <- sum((fit.f2$fitted.values - mean(ins$charges))^2)
SSE/SST # [1] 0.2226939

# children
summary(aov(charges ~ children, ins)) # pr(>F)가 유의수준보다 작아서 유의함
SST <- sum((ins$charges - mean(ins$children))^2)
SSE <- sum((fit.f2$fitted.values - mean(ins$charges))^2)
SSE/SST # [1] 0.2221711
