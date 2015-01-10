#��Ԫ�ع����
blood<-data.frame(x1=c(76.0,91.5,85.5,82.5,79.0,80.5,74.5,
                       79.0,85.0,76.5,82,95,92.5),
                   x2=c(50,20,20,30,30,50,
                        60,50,40,55,40,40,20),
                   y=c(120,141,124,126,117,
                       125,123,125,132,123,132,155,147))
lm.sol<-lm(y~x1+x2,data=blood)
summary(lm.sol)
###��ʽӦ��Ϊ��  y=-62.96+2.136x1+0.4002x2
x<-iris[,c(1:4)]
plot(x)
#5.1         3.5          1.4         
swiss   #��ʿ���ݼ�

s<-lm(Fertility~.,data=swiss)   #Ӧ����ΪFertility, .�����ȥ������������Ա���
print(s)
summary(s)
# Fertility Agriculture Examination Education Catholic Infant.Mortality
#ȥ���Ա���Examination������÷ǳ���
s<-lm(Fertility~Agriculture+Education+Catholic+Infant.Mortality,data=swiss)

step(x,dirction="forward")  #��ǰ���뷨��backward����޳���step(object, scope, scale = 0,
                            #direction = c("both", "backward", "forward"),
                            #trace = 1, keep = NULL, steps = 1000, k = 2, ...)

#1�������Ƿ������̬�ֲ� 2�Ƿ���Ⱥֵ����ģ�Ͳ����ܴ���� 3������ģ���Ƿ���� 
#4���Ƿ���ڶ��ع�����

residuals(lm.rb)   #�����Իع�Ĳв�
shapiro.test()     #�Ա����Ƿ������̬�ֲ��ļ���
shapiro.test(iris$Sepal.Length) 
shapiro.test(iris$Petal.Length)
glm()  #�������Իع�

#����������
install.packages("arules")
library(arules)
data(Grocenes)
inspect(Groceries)
rules<-apriori(Groceries,parameter=list(support=0.01,confidence=0.5))
summary(rules)
inspect(rules)

#MASS�������Խ�ģ����lda()
install.packages("MASS")
library(MASS)
ld<-lba(G~x1+x2)
ld