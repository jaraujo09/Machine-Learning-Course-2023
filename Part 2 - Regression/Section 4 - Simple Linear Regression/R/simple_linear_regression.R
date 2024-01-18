# Simple Linear Regression

# Importing the dataset
dataset = read.csv('Salary_Data.csv')

sum(is.na(dataset))
#não tem valores nulos, então não temos mais passos de pre-processing

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



# Feature Scaling
#Não precisamos de usar pois o package de simple regression irá tratar disso sozinho

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary~YearsExperience,
               data = training_set)
#formula - variável dependente~Independente
#data - training_set que é onde treinamos o modelo

summary(regressor)
#os * nos coeficientes indicam a significancia estatística
#ou seja, quando temos *** significa que a variável tem alta significancia estatística, como o YearsExperience
#podemos prever que vai hvaer uma relação linear forte entre o salário e os anos de experiência
#p-value quanto mais baixo, mais impacto a variavel independente vai ter na dependente, alta relação estatística


# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
#new_data contem as novas observações que queremos prever os resultados

# Visualising the Training set results
#install.packages('ggplot2')
library(ggplot2)
#geom_point são os observation points
#plotting X and Y axis com a variável aes (escolher o training_set)
#geom_line plota a linha dos salários previstos no training_Set
#ou seja, vamos ter os valores reais em pontos, e a linha é conforme os valores previstos

ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training Set') +
  xlab('Years of Experience') + 
  ylab('Salary ($)')


# Visualising the Test set results
