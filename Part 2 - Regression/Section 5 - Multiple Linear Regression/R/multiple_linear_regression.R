# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')
sum(is.na(dataset))


# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1,2,3))



# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.80)
split

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Feature Scaling
#não fazemos feature sclaing manualmente porque faz na função de multi linear regression


# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit~ . ,
               data = training_set) #o ponto indica todas as variáveis independentes

#regressor = lm(formula = Profit~ R.D.Spend + Administration + Marketing.Spend + State)
summary(regressor)
#percebemos que a variável R.D.Spend tem elevada significancia estatística, e por isso
#deve ser uma das variáveis importantes para os investidores
#Por isso, podemos até tornar isto numa regressão linear simples, pois apenas uma variável é significativa

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred

# Building the optimal model using Backward Elimination
#o principio da backward elimination é eliminar as variáveis menos significantes estatisticamente, e por isso temos que as escrever todas
#poderiamos usar o data = training_set mas vamos usar o dataset todo para não perdermos informação
regressor = lm(formula = Profit~ R.D.Spend + Administration + Marketing.Spend + State ,
               data = dataset) 
summary(regressor)

#Removemos a variável com maior p-value (State): Step 4
regressor = lm(formula = Profit~ R.D.Spend + Administration + Marketing.Spend ,
               data = dataset) 
summary(regressor)

#Removemos a variável com maior p-value (Administration): Step 4
regressor = lm(formula = Profit~ R.D.Spend + Marketing.Spend ,
               data = dataset) 
summary(regressor)

#ao removermos o Administration percebemos queo p-value de Marketing.Spend baixa e fica entre 5-10%
#como estamos a fazer backward vamos remover também esta variável, mas mais tarde vamos perceber se devemos retirar ou não

#Removemos a variável com maior p-value (Marketing.Spend): Step 4
regressor = lm(formula = Profit~ R.D.Spend,
               data = dataset) 
summary(regressor)


#Building an Automatic Backward Elimination 
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)
