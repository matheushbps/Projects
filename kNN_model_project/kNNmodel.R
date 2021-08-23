#setting work directory
setwd("C:/Users/matheussilva1_i/Desktop/Cursos/Projeto02")
getwd()
dir()

#loading the dataset and seeing whether there are null values
df <- read.csv('bc_data.csv')
head(df)
str(df)
any(is.na(df))

#installing the package which have kNN model
#install.packages('class')
library(class)

#removing id column
df <- df[,-1]


#selecting only numeric columns
length(df)
df_nums <- df[,2:length(df)]

#as we can see as the data shows following, we have different dimensions of
#columns of the dataframe, so we'll normalize it 
head(df[,c('radius_mean', 'smoothness_mean')],15)

#creating the function which will normalize the data
normalizing <- function(z) {
  return ((z - min(z))/(max(z) - min(z)))}

#testing the function
normalizing(c(10,20,30,40,50))
normalizing(c(1,2,3,4,5))

#applying the normalizing function to the dataframe
df_nums_normalized <- sapply(df_nums, normalizing)


#creating the factor vector which we'll use within the kNN model
df$diagnosis <- factor(df$diagnosis, levels = c('B','M'), labels = c('benign','malignant'))

#install.packages('dplyr')
library(dplyr)

#setting the train and test data
train_data <- df_nums[1:400, ]
train_data_result <- df[1:400, 1]

test_data <- df_nums[401:569, ]
test_data_result <- df[401:569, 1]

#applying the model with k=11
model_v1 <- knn(train = train_data,
    test = test_data,
    cl = train_data_result,
    k = 11)

#install.packages('gmodels')
library(gmodels)

#looking at the CrossTable to see the results of the model
CrossTable(test_data_result, model_v1, chisq.test = FALSE)


#starting a loop to see which k would be better using the current model
error_tax1 = NULL

for (i in 1:100) {
  set.seed(101)
  resultado = knn(train_data, test_data, train_data_result, i)
  error_tax1[i] = mean(resultado != test_data_result)}

#install.packages('ggplot2')
library(ggplot2)

#making the dataframe which we'll use to plot
aux_vector = 1:100
df_error1 <- data.frame(error_tax1, vetor_aux)

#making the plot with the results 
ggplot(df_error1, aes(x=aux_vector, y=error_tax1)) +
  geom_point() + 
  geom_line(lty='dotted', color='red')

#trying the best k == 4~5

model_v2 <- knn(train = train_data,
                test = test_data,
                cl = train_data_result,
                k=5)

CrossTable(x = test_data_result, y = model_v2, prop.chisq = FALSE)

#the result is a little better, because we have 7 error and not 8 anymore,
# but we still have 7 errors (2 false positive and 5 false negative)
# ------------------------
#we'll try another normalizing method, which is z-scale to see whether we can get
# a better result

#applying z-scale
data_z <- as.data.frame(scale(df_nums))

#slicing the data into train and test again, using z-data-scale
train_data_v2 <- data_z[1:400, ]
train_data_result_v2 <- df[1:400, 1]

test_data_v2 <- data_z[401:569, ]
test_data_result_v2 <- df[401:569, 1]


#using the loop to know the best k-value
error_tax2 = NULL

for (i in 1:100) {
  set.seed(101)
  resultado = knn(train_data_v2, test_data_v2, train_data_result_v2, i)
  error_tax2[i] = mean(resultado != test_data_result_v2)}


#build the aux dataframe, which we will use to make the plot
df_erro <- data.frame(error_tax2, aux_vector)

#making the plot
ggplot(df_erro, aes(x=vetor_aux, y=taxa_erro2)) +
  geom_point() + 
  geom_line(lty='dotted', color='red')

#using the best k=5 to apply the kNN model
modelo_v3 <- knn(train=train_data_v2,
                 test=test_data_v2,
                 cl=train_data_result_v2,
                 k=5)

CrossTable(test_data_result_v2, modelo_v3, prop.chisq = FALSE)

#as we can see, we have a better result using the z-scale method, because know 
#we have 3 error (not 7 anymore), where 2 are false negative and 1 is false positive.





