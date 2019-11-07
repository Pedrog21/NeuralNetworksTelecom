##Estatística descritiva dos dados
lt1<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
library(psych)
describe(dataT1snap1706[dataT1snap1706[,14]==0,lt1])

describe(dataT1snap1706[dataT1snap1706[,14]==1,lt1])

pairs(dataT1snap1706[,lt1],col=(dataT1snap1706[,14]))

round(cor(dataT1snap1706[dataT1snap1706[,14]== 1,lt1]),3)
round(cor(dataT1snap1706[dataT1snap1706[,14]== 0,lt1]),3)

str(Data1)

##Aplicação de Neural Networks (Caso 1)
Data1 <- dataT1snap1706[,2:14]
smp_size <- floor(0.60 * nrow(Data1))
set.seed(123)
train_ind <- sample(seq_len(nrow(Data1)), size = smp_size)
trainNN1 <- Data1[train_ind, ]
testNN1 <- Data1[-train_ind, ]

library(neuralnet)
n<-names(Data1)
f <- as.formula(paste("anomalyQ ~", paste(n[!n %in% "anomalyQ"], collapse = " + ")))
nn <- neuralnet(f, data = trainNN1, hidden=c(10,6,4), linear.output=FALSE, threshold=0.01)
plot(nn)

nn.results <- compute(nn, testNN1[,1:12])
results <- data.frame(actual = testNN1$anomalyQ, prediction = nn.results$net.result)
roundedresultsdf<-data.frame(sapply(results,round,digits=0))

k=0
for(i in 1:7470) {    
  if(roundedresultsdf[i,1]!=roundedresultsdf[i,2]) k=k+1
  }
print(k) ##Ver em quantos é que a rede neuronal não acertou

plot(testNN1$anomalyQ,results[,2],col='red',main='Real vs predicted NN',pch=18,cex=0.7)
 abline(0,1,lwd=2)
 legend('bottomright',legend='NN',pch=18,col='red', bty='n')

 
 
##kNN para o Caso1
 install.packages("class")
 library(class)
 knn_pred = knn(trainNN1[,1:12],testNN1[,1:12], trainNN1$anomalyQ, k=100)

 table(knn_pred, testNN1$anomalyQ)
 ##Naive Bayes para Caso 1
 
 model = naiveBayes(anomalyQ ~., trainNN1)
 nb_pred = predict(model, testNN1)
 nb_pred
 table(nb_pred, testNN1$anomalyQ)
 


##C5.0
library(recipes)
rule_mod <- C5.0(x = trainNN1[, 1:12], y = trainNN1$anomalyQ, rules = TRUE)
table(predict(rule_mod, newdata = testNN1[, 1:12]), testNN1$anomalyQ)


