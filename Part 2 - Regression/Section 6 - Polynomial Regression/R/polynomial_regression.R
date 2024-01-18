# Polynomial Regression

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# Splitting the dataset into the Training set and Test set
# como este dataset é pequeno, não vamos separar em treino e teste
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Linear Regression to the dataset
lin_reg = lm(formula = Salary ~ .,
             data = dataset)

# Fitting Polynomial Regression to the dataset
#temos que adicionar os polinomial levels, que criam a nova matriz de features
#criamos uma coluna nova (level) no nosso dataset no nivel ao quadrado
#se quisermos um maior nivel, adicionamos mais colunas com potencias
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
pol_reg = lm(formula = Salary ~ .,
             data = dataset)

# Visualising the Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth of Bluff (Lin. Regression)') + 
  xlab('Level') +
  ylab('Salary ($)')

# Visualising the Polynomial Regression results
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(pol_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth of Bluff (Poly. Regression)') + 
  xlab('Level') +
  ylab('Salary ($)')

# Visualising the Regression Model results (for higher resolution and smoother curve)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)

library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(pol_reg, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Regression Model') + 
  xlab('Level') +
  ylab('Salary ($)')


# Predicting a new result with Linear Regression
y_pred = predict(lin_reg, data.frame(Level = 6.5))
#como o valor 6.5 não existe no nosso dataset temos que criar um novo dataframe com esse valor
#o valor previsto é longe da realidade, e verificamos no modelo pol_reg 


# Predicting a new result with Polynomial Regression
#como o pol_reg necessita de todos os Levels, temos que os descrever no novo dataframe
y_pred = predict(pol_reg, data.frame(Level = 6.5, 
                                     Level2 = 6.5^2,
                                     Level3 = 6.5^3,
                                     Level4 = 6.5^4))
