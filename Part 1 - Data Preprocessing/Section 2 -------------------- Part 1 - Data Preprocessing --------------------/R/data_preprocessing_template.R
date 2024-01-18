# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('Data.csv')
dataset

# MISSING DATA 

sum(is.na(dataset))  #existem dados NA
#Podemos remover as linhas onde tem missing data, mas pode ser perigoso
#Devemos então fazer a média das colunas

dataset$Age = ifelse(is.na(dataset$Age), #se a coluna Age tiver um valor NA
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)), #usamos a função de mean da coluna 
                     dataset$Age) #else usamos o valor que lá está

dataset$Salary = ifelse(is.na(dataset$Salary),
                     ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)), 
                     dataset$Salary)

# ENCODING cATEGORICAL DATA
dataset$Country = factor(dataset$Country,
                         levels = c('France', 'Spain', 'Germany'),
                         labels = c(1,2,3))

dataset$Purchased = factor(dataset$Purchased,
                         levels = c('No','Yes'),
                         labels = c(0,1))
help(factor)

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.80)
split
#TRUE está no training set, FALSE no test set

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# FEATURE SCALING
#Não aplicamos feature scaling nas colunas que são fatores
training_set[, 2:3] = scale(training_set[, 2:3])
test_set[, 2:3] = scale(test_set[, 2:3])
