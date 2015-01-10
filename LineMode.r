#多元回归举列
blood<-data.frame(x1=c(76.0,91.5,85.5,82.5,79.0,80.5,74.5,
                       79.0,85.0,76.5,82,95,92.5),
                   x2=c(50,20,20,30,30,50,
                        60,50,40,55,40,40,20),
                   y=c(120,141,124,126,117,
                       125,123,125,132,123,132,155,147))
lm.sol<-lm(y~x1+x2,data=blood)
summary(lm.sol)
###公式应该为：  y=-62.96+2.136x1+0.4002x2
x<-iris[,c(1:4)]
plot(x)
#5.1         3.5          1.4         
swiss   #瑞士数据集

s<-lm(Fertility~.,data=swiss)   #应变量为Fertility, .代表除去因变量的左右自变量
print(s)
summary(s)
# Fertility Agriculture Examination Education Catholic Infant.Mortality
#去除自变量Examination后结果变得非常好
s<-lm(Fertility~Agriculture+Education+Catholic+Infant.Mortality,data=swiss)

step(x,dirction="forward")  #向前引入法，backward向后剔除法step(object, scope, scale = 0,
                            #direction = c("both", "backward", "forward"),
                            #trace = 1, keep = NULL, steps = 1000, k = 2, ...)

#1：样本是否符合正态分布 2是否离群值导致模型产生很大误差 3：线性模型是否合理 
#4：是否存在多重共线性

residuals(lm.rb)   #求线性回归的残差
shapiro.test()     #自变量是否符合正态分布的检验
shapiro.test(iris$Sepal.Length) 
shapiro.test(iris$Petal.Length)
glm()  #广义线性回归

#数据蓝分析
install.packages("arules")
library(arules)
data(Grocenes)
inspect(Groceries)
rules<-apriori(Groceries,parameter=list(support=0.01,confidence=0.5))
summary(rules)
inspect(rules)

#MASS包与线性建模函数lda()
install.packages("MASS")
library(MASS)
ld<-lba(G~x1+x2)
ld