#setting the work directory

setwd('C:/Users/matheussilva1_i/Desktop/Cursos/Projeto03/LinearRegressionModel')
getwd()
dir()

#loading the dataset
df <- read.csv('despesas.csv')

#exploring the dataset
head(df)
str(df)
summary(df)
#there is no na data
any(is.na(df))

#as we can see, the ones which are smokers IN GENERAL spend more with hospital expenses
table(df$fumante)
boxplot(gastos~fumante, data = df)

#we can see the correlation of the numeric variables as follows
cor(df_numeric)
pairs(df_numeric)

#visualizing the data through another vision
#install.packages('psych')
library(psych)
pairs.panels(df_numeric)

#slicing just for the numeric columns
df_numeric <- df[, sapply(df, is.numeric)]

#taking a sample from the data of 70%
library(caTools)

?sample.split
sample <- sample.split(df$idade, SplitRatio = 0.7)

#spliting the data we'll use to train and test
train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)

#training the model
model_v1 <- lm(gastos ~ ., data = train)

#predicting by the mean of the model
?predict
predict_modelv1 <- predict(model_v1, test)
class(predict_modelv1)
head(predict_modelv1)

#viewing and anylising the results of the model
summary(model_v1)
model_v1

#adding the squared age
df2 <- df
df2$idade2 <- df2$idade^2

?ifelse

df2$bmi30 <- ifelse(df2$bmi > 30 , 1 , 0)
head(df2)

#creating the final model

model_v2 <- lm(gastos ~ idade + idade2 + filhos + bmi + sexo +
                  bmi30 * fumante + regiao, data = df2)

summary(model_v2)
