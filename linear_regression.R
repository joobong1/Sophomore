# library
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(gridExtra)
library(showtext)
library(ggplot2)

# ��Ʈ �ҷ�����
showtext_auto()
font_add_google('Noto Sans KR', 'notosanskr')

# ������ �ҷ�����
ins <- read.csv("./insurance.csv")
str(ins)

# �������� ��ȯ
ins$age <- as.numeric(ins$age)
ins$sex <- as.factor(ins$sex)
ins$children <- as.factor(ins$children)
ins$smoker <- as.factor(ins$smoker)
ins$region <- as.factor(ins$region)
str(ins)


# ����(sex) ������
male <- ins[ins$sex == "male", ] # ����
female <- ins[ins$sex == "female", ] # ����


# bmi��ġ(bmi) ������ - ���� ���������� bmi 30 �̻��� ������ ����
standard <- ins[ins$bmi < 30, ] # ǥ��(bmi 30 �̸�)
obesity <- ins[ins$bmi >= 30, ] # ��(bmi 30 �̻�)


# ��������(smoker) ������
yes <- ins[ins$smoker == "yes", ] # ������
no <- ins[ins$smoker == "no", ] # ��������


# ������ ���ϱ� ���� as.numeric()
ins$sex <- as.numeric(ins$sex)
ins$children <- as.numeric(ins$children)
ins$smoker <- as.numeric(ins$smoker)
ins$region <- as.numeric(ins$region)


# ������
cor.test(ins$age, ins$charges) # 0.2990082
cor.test(ins$sex, ins$charges) # 0.05729206 
cor.test(ins$bmi, ins$charges) # 0.198341
cor.test(ins$children, ins$charges) # 0.06799823
cor.test(ins$smoker, ins$charges) # 0.7872514
cor.test(ins$region, ins$charges) # -0.006208235


# �׷��� �׸��� ���� as.factor
ins$sex <- as.factor(ins$sex)
ins$children <- as.factor(ins$children)
ins$smoker <- as.factor(ins$smoker)
ins$region <- as.factor(ins$region)
str(ins)


# �������ο� ���� ���̿� ���� �����
ggplot(ins, aes(x = age, y = charges, fill = smoker)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("���̿� ���� �����") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "����", y = "�����")

# ���� ���� �ð�ȭ - ����(sex)�� ����
g <-ggplot(ins, aes(x = sex, y = charges, fill = sex)) +
  geom_bar(stat = "identity")
g <- g + ggtitle("������ ���� �����") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "����", y = "�����")
g


# ǥ��, �� ���� �ð�ȭ - bmi�� ����, ��������(smoker)�� ���� 
g_stan <- ggplot(standard, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(size = 2, color = "burlywood") +
  stat_smooth(method = "lm") +
  ggtitle("�������ο� ���� \n bmi��ġ�� ǥ���� ����� �����") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "bmi��ġ", y = "�����")

g_obes <- ggplot(obesity, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(size = 2, color = "burlywood") +
  stat_smooth(method = "lm") +
  ggtitle("�������ο� ���� \n bmi��ġ�� ���� ����� �����") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "bmi��ġ", y = "�����")

grid.arrange(g_stan, g_obes, ncol = 2, nrow = 1)
## bmi��ġ�� �����ϸ� ����ݵ� �����ϴ� ���� �������踦 ��� �ִ�
## ������ ������� �����ڶ�� bmi��ġ�� �������� ����ݿ� ������ �� ���� �ش�.


# �ڳ� ��(children) ���� �ð�ȭ
gg <- ggplot(ins, aes(x = children, y = charges, fill = children)) +
  geom_boxplot(outlier.color = 'red') +
  stat_summary(fun.y = "mean",
               geom = "point",
               shape = 22,
               size = 3,
               fill = "blue")
gg <- gg + ggtitle("�ڳ� ���� ���� �����") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "�ڳ� ��", y = "�����") 
gg


# ��������(smoker)�� ����
g_male <- ggplot(male, aes(x = smoker, y = charges)) +
  geom_boxplot(outlier.color = 'red') +
  stat_summary(fun.y = "mean",
               geom = "point",
               shape = 22,
               size = 3,
               fill = "blue")
g_male <- g_male + ggtitle("���� ���������� ���� �����") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "���� ����", y = "�����")

g_female <- ggplot(female, aes(x = smoker, y = charges)) +
  geom_boxplot(outlier.color = 'red') +
  stat_summary(fun.y = "mean",
               geom = "point",
               shape = 22,
               size = 3,
               fill = "blue")
g_female <- g_female + ggtitle("���� ���������� ���� �����") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "���� ����", y = "�����")

grid.arrange(g_male, g_female, ncol = 2, nrow = 1)
## ���� ������ ���� �������ں��� �����ڰ� �� ���� ������� ����.
## ������ �������� �ټ��� ���̷� �� ���� ������� ����.


# ����(region) ���� �ð�ȭ
ggg <- ggplot(ins, aes(x = region, y = charges, fill = region)) +
  geom_boxplot(outlier.color = 'red') +
  stat_summary(fun.y = "mean",
               geom = "point",
               shape = 22,
               size = 3,
               fill = "blue")
ggg <- ggg + ggtitle("������ ���� �����") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "����", y = "�����") 
ggg
## southeast�� ��� ������� ��������� ���� ������� ���Ҵ�.
## �̻�ġ�� �� �� ���� ��ο��� ���� ��Ÿ����.


# ��ġ�� �������� �� ������� Ȯ��
ins$children <- as.numeric(ins$children)
ins$smoker <- as.numeric(ins$smoker)
ins$sex <- as.numeric(ins$sex)
ins$region <- as.numeric(ins$region)

library(corrplot)
corrplot(cor(ins))


# train, test set ������
nrow(ins)*0.7 # = 936.6

set.seed(14)
samp <- sample(1:nrow(ins), 936)

train <- ins[samp, ]
test <- ins[-samp, ]


# ��� ���������� ���� ����ȸ�ͺм�
options(scipen = 100)

fit.all <- lm(charges ~ ., data = train)
summary(fit.all) # .�� ��� ������ ��

# ��������
# ����׸� ���� ȸ�ͺм�
fit.0 <- lm(charges ~ 1, data = train)
fit.0 # 1�� ����׸� ������ �����


# �������ù�
# �ƹ� �������� ȸ�͸������� ����
# �� �ڿ� F��跮�� ū �������� ����
fit.forw <- step(fit.0, scope = list(lower = fit.0, upper = fit.all),
                 direction = "forward", trace = F)
s.f <- summary(fit.forw)
s.f


# �������Ź�
# ��� ���������� �����ϴ� ȸ�͸����� �ּ������� ���� ������� ���ս�Ŵ
# �� ���� ���� ���� ������ ��ġ�� ���������� ȸ�Ͱ���� ���� ���� �ǽ�
# �Ⱒ��Ű�� �� �ϸ� full������ �������� ����
# �Ⱒ��Ű�� ��� �ش� ������ �����ϰ� �ٽ� ������ ���ս�Ų �� �ݺ�
fit.back <- step(fit.all, direction = "backward", trace = F)
s.b <- summary(fit.back)
s.b


# �ܰ������ù�
# �������ù��� �������Ź��� ������ ����
# �������� ��� �� ���� ���� ����
fit.step <- step(fit.all, direction = "both",
                 scope = list(lower = fit.0, upper = fit.all))
s.s <- summary(fit.step)
s.s


# �����
# �̻�ġ�� ������ �������
s.f$adj.r.squared # �������ù� # 0.7415975
s.b$adj.r.squared # �������Ź� # 0.7415975
s.s$adj.r.squared # �ܰ������ù� # 0.7415975

# �� ������ �� �Ǿ������� ���� ��ġ
# ��Ż ������ �ǹ��ϴµ�, �������� ����
# ���� ���յƴ����� ���� �� ����
deviance(fit.forw) # 33208899090
deviance(fit.back) # 33208899090
deviance(fit.step) # 33208899090


# �����𵨼���
# �������ܰ�� ����
fit.f <- fit.step
summary(fit.f)


# ���߰����� Ȯ�� �� ���� �� �缱��
library(car) # vif() ����
vif(fit.f)
# vif() => ���߰����� # 10�̻��̸� ���߰������� �����Ѵٰ� ����
fit.f2 <- lm(charges ~ age + bmi + children + smoker, data = train)
summary(fit.f2)
vif(fit.f2) # 10���� ���� �۱� ������ ��������

# ������ ȸ�ͽĿ� test set�� ���� ��� Ȯ���ϱ�
# predict() : lm(), glm()�� ���� ���ɹ����� �� ȸ�ͼ�����
# �־��� x���� �ش��ϴ� ���ο� y���� predict�ϴ� Ŀ�ǵ�
# newdata�� ����� x���� ������ ������ data.frame�� colname��
# ȸ�ͺм����� ����� �������� ���� �����ϰ� �����ؾ�
# ������ �߻����� ����

# �ŷڱ���
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


# ��������
pred <- predict(fit.f2, newdata = test, interval = "prediction")
pred2 <- as.data.frame(pred)
pred2$act <- test$charges
pred2$YN <- ifelse(pred2$fit >= pred2$lwr &
                     pred2$fit <= pred2$upr, 1, 0)
# lwr�� upr�� ���� �ŷ� ������ ���Ѱ� ���� ���� �ǹ�
head(pred2) 
sum(pred2$YN) # [1] 402
sum(pred2$YN) / nrow(test) # [1] 1


# �׷���
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
  ggtitle("���̿� ���� ����� ����ġ") +                                                                             
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "����", y = "�����") 
pr1

# �׷���
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
  ggtitle("bmi�� ���� ����� ����ġ") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "bmi��ġ", y = "�����") 
pr2

# �׷���
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
  ggtitle("�ڳ� ���� ���� ����� ����ġ") +
  theme(plot.title = element_text(family = "notosanskr",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "Navy Blue")) +
  theme(axis.title = element_text(family = "notosanskr",
                                  face = "bold",
                                  size = 16,
                                  color = "grey15")) +
  labs(x = "�ڳ� ��", y = "�����") 
pr3

grid.arrange(pr1, pr2, pr3, ncol = 2, nrow = 2)

# �������
# age
summary(aov(charges ~ age, ins)) # pr(>F)�� ���Ǽ��غ��� �۾Ƽ� ������
SST <- sum((ins$charges - mean(ins$age))^2)
SSE <- sum((fit.f2$fitted.values - mean(ins$charges))^2)
SSE/SST # [1] 0.2228505

# bmi
summary(aov(charges ~ bmi, ins)) # pr(>F)�� ���Ǽ��غ��� �۾Ƽ� ������
SST <- sum((ins$charges - mean(ins$bmi))^2)
SSE <- sum((fit.f2$fitted.values - mean(ins$charges))^2)
SSE/SST # [1] 0.2226939

# children
summary(aov(charges ~ children, ins)) # pr(>F)�� ���Ǽ��غ��� �۾Ƽ� ������
SST <- sum((ins$charges - mean(ins$children))^2)
SSE <- sum((fit.f2$fitted.values - mean(ins$charges))^2)
SSE/SST # [1] 0.2221711