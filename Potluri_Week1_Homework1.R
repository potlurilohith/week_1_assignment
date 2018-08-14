# Basic Operations on the variables in Insurance Dataset

#view the data in the console
insurance

#overview of dataset
str(insurance)
class(insurance$age)
class(insurance$sex)
class(insurance$bmi)
class(insurance$children)
class(insurance$smoker)
class(insurance$region)
class(insurance$charges)
dim(insurance)
insurance[5,]
insurance[,1]
nrow(insurance)
ncol(insurance)
  
#EAD
sapply(insurance, kurtosis)
mean(insurance$charges)
median(insurance$bmi)
psych::describe(insurance)
summary(insurance)
e1071::kurtosis(insurance$sex)
e1071::skewness(insurance$sex)
sd(insurance$children)
var(insurance)
IQR(insurance$smoker)
quantile(insurance$age)

table(insurance$sex)
table(insurance$children)
table(insurance$region)
table(insurance$smoker)
table(insurance$smoker[insurance$sex == "female"])
table(insurance$smoker[insurance$sex == "male"])

pairs(insurance, col = "navy", main = "Scatterplot Matrix for all the variables")

pairs(insurance, col=insurance$sex, main = "Scatterplot Matrix for all the variables grouped by variable Sex where Red is Male and Black is Female")

library(lattice)
splom(insurance[c(1,2,3,4,5,6,7)], main = "Scatter Plot Matrix", 
      varnames = c("AGE","SEX", "BMI","CHILDREN","SMOKER", "REGION", "CHARGES"),
    varnames.labels = 9)

library(psych)
pairs.panels(insurance[c(1,3,4,7)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
              main = "Graph"
             )

cor_data <- insurance[, c(1,3,4,7)]
cor(na.omit(cor_data))

plot()

hist(insurance$age, breaks = 5, density = 10, probability = TRUE ,angle = 75, col = "navy", border = "red", main = "Distribution of values in Age variable", 
     xlab = "Age Groups", ylab = "Count of People")

hist(insurance$charges, angle = 75, col = "orange", border = "navy", main = "Distribution of values in Charge variable", 
     xlab = "Charge Values", ylab = "Count of People")

sex_smokers<-table(insurance$sex[insurance$smoker == "yes"])
sex_smokers
pie(sex_smokers,col = c("orange","green"),labels = insurance$sex, border = "black", main = "Classification by Gender Among Smokers")

boxplot(insurance$charges~insurance$region, col = "Navy", border = "orange", main = "Box plot for 4 regions in the Dataset", xlab = "4 Regions", ylab = "Charge values")

library(ggpubr)
library(easyGgplot2)
library(devtools)
plot(insurance$charges~insurance$age, col = "red", main = " hii")
ggplot(insurance, aes(y = charges, x =age, colour = sex)) + geom_point() + 
  ggtitle("Scatter Plot between Age and Charges") +   
  theme(plot.title = element_text(hjust = 0.5))+ geom_smooth(method='lm')
ggplot(insurance, aes(x =insurance$children , y =insurance$charges )) + geom_point() + facet_grid(~insurance$sex) + ggtitle("Scatter Plot between Children and Charges") +
  theme(plot.title = element_text(hjust = 0.5))+ geom_smooth(method='lm') + xlab("Number of Children") + ylab("Value of Charges") 
xyplot(insurance$charges ~ insurance$age, insurance, groups = insurance$region, pch= 20)

install.packages("HistData")
data(Arbuthnot, package="HistData")
plot(Ratio ~ Year, data=Arbuthnot,
     pch=16,
     ylim=c(1, 1.20),
     cex.lab = 1.3,
     ylab="Sex Ratio (M/F)")
# connect points by lines
lines(Ratio ~ Year, data = Arbuthnot, col="gray")
# add reference line
abline(h=1, col="red", lwd=3)
text(1640, 1, "Males = Females", col="red")
# add linear regression line
abline(lm(Ratio ~ Year, data=Arbuthnot),
       col="darkgreen")
# add loess smooth
Arb.smooth <- with(Arbuthnot,
                   loess.smooth(Year, Ratio))
lines(Arb.smooth$x, Arb.smooth$y,
      col="blue", lwd=2)
# add internal figure caption
text(1690, 1.19, "Arbuthnot's data on the\nMale /
     Female Sex Ratio", cex=1.2)

############################ Example: Predicting Medical Expenses ----
############# Step 2: Exploring and preparing the data ----
insurance <- read.csv(file.choose(),header=T)  
str(insurance)
View(insurance)

# summarize the charges variable
summary(insurance$charges)

# histogram of insurance charges
hist(insurance$charges)


qqnorm(insurance$charges)
qqline(insurance$charges)

agevscharges<-lm(insurance$charges~insurance$age)
summary(agevscharges)

x<-lm(insurance$charges~insurance$age+insurance$sex["female"]+insurance$bmi+insurance$children+insurance$smoker+insurance$region)
summary(x)

glm(charges ~age+sex+bmi+children+smoker+region, data = insurance, family = "gaussian")

glm(charges ~age+sex+bmi+children+smoker+region, data = insurance, family = "poisson")

ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region,
                data = insurance)
summary(ins_model)

insurance$age2 <- insurance$age^2
ins_model <- lm(charges ~  age + children + bmi + sex + smoker + region + age2,
                data = insurance)
summary(ins_model)

insurance$age2 <- log(insurance$age)
ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region + age2,
                data = insurance)
summary(ins_model)

insurance$bmi2 <- log(insurance$bmi2)
ins_model <- lm(charges ~ age2+ age +  children + bmi + sex + smoker + region + bmi2,
                data = insurance)
summary(ins_model)


x<-lm(insurance$charges~insurance$age+insurance$sex+insurance$bmi+insurance$children+insurance$smoker+insurance$region)
summary(x)
