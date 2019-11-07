##CHICAGO

Data2Chicago <- dataT1snap1706[,2:4]
Data2Chicago$anomalyQ<- dataT1snap1706[,14]

##Neural Networks
smp_size <- floor(0.60 * nrow(Data2Chicago))
set.seed(123)
train_ind <- sample(seq_len(nrow(Data2Chicago)), size = smp_size)
trainNN2Chicago <- Data2Chicago[train_ind, ]
testNN2Chicago <- Data2Chicago[-train_ind, ]

library(neuralnet)
n<-names(Data2Chicago)
f <- as.formula(paste("anomalyQ ~", paste(n[!n %in% "anomalyQ"], collapse = " + ")))
nn <- neuralnet(f, data = trainNN2Chicago, hidden=c(2,1), linear.output=FALSE, threshold=0.01, act.fct = "tanh", err.fct = "sse")
plot(nn)

nn.results <- compute(nn, testNN2Chicago[,1:3])
results <- data.frame(actual = testNN2Chicago$anomalyQ, prediction = nn.results$net.result)
roundedresultsdf<-data.frame(sapply(results,round,digits=0))

k=0
for(i in 1:7470) {    
  if(roundedresultsdf[i,1]!=roundedresultsdf[i,2]) k=k+1
}
print(k)


#kNN
library(class)
knn_pred = knn(trainNN2Chicago[,1:3],testNN2Chicago[,1:3], trainNN2Chicago$anomalyQ)

table(knn_pred, testNN2Chicago$anomalyQ)

##Naive Bayes
library(e1071)
model = naiveBayes(anomalyQ ~., trainNN2Chicago)
nb_pred = predict(model, testNN2Chicago)
nb_pred
table(nb_pred, testNN2Chicago$anomalyQ)

##C5.0
rule_mod <- C5.0(x = trainNN2Chicago[, 1:3], y = trainNN2Chicago$anomalyQ, rules = TRUE)
table(predict(rule_mod, newdata = testNN2Chicago[, 1:3]), testNN2Chicago$anomalyQ)


##GERMANY


Data2Germany <- dataT1snap1706[,5:7]
Data2Germany$anomalyQ<- dataT1snap1706[,14]

##Neural Networks
smp_size <- floor(0.60 * nrow(Data2Germany))
set.seed(123)
train_ind <- sample(seq_len(nrow(Data2Germany)), size = smp_size)
trainNN2Germany <- Data2Germany[train_ind, ]
testNN2Germany <- Data2Germany[-train_ind, ]

library(neuralnet)
n<-names(Data2Germany)
f <- as.formula(paste("anomalyQ ~", paste(n[!n %in% "anomalyQ"], collapse = " + ")))
nn <- neuralnet(f, data = trainNN2Germany, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
plot(nn)

nn.results <- compute(nn, testNN2Germany[,1:3])
results <- data.frame(actual = testNN2Germany$anomalyQ, prediction = nn.results$net.result)
roundedresultsdf<-data.frame(sapply(results,round,digits=0))

k=0
for(i in 1:7470) {    
  if(roundedresultsdf[i,1]!=roundedresultsdf[i,2]) k=k+1
}
print(k)


#kNN
library(class)
knn_pred = knn(trainNN2Germany[,1:3],testNN2Germany[,1:3], trainNN2Germany$anomalyQ, k=100)

table(knn_pred, testNN2Germany$anomalyQ)

##Naive Bayes
library(e1071)
model = naiveBayes(anomalyQ ~., trainNN2Germany)
nb_pred = predict(model, testNN2Germany)
nb_pred
table(nb_pred, testNN2Germany$anomalyQ)


##C5.0
rule_mod <- C5.0(x = trainNN2Germany[, 1:3], y = trainNN2Germany$anomalyQ, rules = TRUE)
table(predict(rule_mod, newdata = testNN2Germany[, 1:3]), testNN2Germany$anomalyQ)

##HONG KONG



Data2HK <- dataT1snap1706[,8:10]
Data2HK$anomalyQ<- dataT1snap1706[,14]

##Neural Networks
smp_size <- floor(0.60 * nrow(Data2HK))
set.seed(123)
train_ind <- sample(seq_len(nrow(Data2HK)), size = smp_size)
trainNN2HK <- Data2HK[train_ind, ]
testNN2HK <- Data2HK[-train_ind, ]

library(neuralnet)
n<-names(Data2HK)
f <- as.formula(paste("anomalyQ ~", paste(n[!n %in% "anomalyQ"], collapse = " + ")))
nn <- neuralnet(f, data = trainNN2HK, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
plot(nn)

nn.results <- compute(nn, testNN2HK[,1:3])
results <- data.frame(actual = testNN2HK$anomalyQ, prediction = nn.results$net.result)
roundedresultsdf<-data.frame(sapply(results,round,digits=0))

k=0
for(i in 1:7470) {    
  if(roundedresultsdf[i,1]!=roundedresultsdf[i,2]) k=k+1
}
print(k)


#kNN
library(class)
knn_pred = knn(trainNN2HK[,1:3],testNN2HK[,1:3], trainNN2HK$anomalyQ, k=60)

table(knn_pred, testNN2HK$anomalyQ)

##Naive Bayes
library(e1071)
model = naiveBayes(anomalyQ ~., trainNN2HK)
nb_pred = predict(model, testNN2HK)
nb_pred
table(nb_pred, testNN2HK$anomalyQ)

##C5.0
rule_mod <- C5.0(x = trainNN2HK[, 1:3], y = trainNN2HK$anomalyQ, rules = TRUE)
table(predict(rule_mod, newdata = testNN2HK[, 1:3]), testNN2HK$anomalyQ)


##LONDRES


Data2London <- dataT1snap1706[,11:13]
Data2London$anomalyQ<- dataT1snap1706[,14]

##Neural Networks
smp_size <- floor(0.60 * nrow(Data2London))
set.seed(123)
train_ind <- sample(seq_len(nrow(Data2London)), size = smp_size)
trainNN2London <- Data2London[train_ind, ]
testNN2London <- Data2London[-train_ind, ]

library(neuralnet)
n<-names(Data2London)
f <- as.formula(paste("anomalyQ ~", paste(n[!n %in% "anomalyQ"], collapse = " + ")))
nn <- neuralnet(f, data = trainNN2London, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
plot(nn)

nn.results <- compute(nn, testNN2London[,1:3])
results <- data.frame(actual = testNN2London$anomalyQ, prediction = nn.results$net.result)
roundedresultsdf<-data.frame(sapply(results,round,digits=0))

k=0
for(i in 1:7470) {    
  if(roundedresultsdf[i,1]!=roundedresultsdf[i,2]) k=k+1
}
print(k)


#kNN
library(class)
knn_pred = knn(trainNN2London[,1:3],testNN2London[,1:3], trainNN2London$anomalyQ)

table(knn_pred, testNN2London$anomalyQ)

##Naive Bayes
library(e1071)
model = naiveBayes(anomalyQ ~., trainNN2London)
nb_pred = predict(model, testNN2London)
nb_pred
table(nb_pred, testNN2London$anomalyQ)

##C5.0
rule_mod <- C5.0(x = trainNN2London[, 1:3], y = trainNN2London$anomalyQ, rules = TRUE)
table(predict(rule_mod, newdata = testNN2London[, 1:3]), testNN2London$anomalyQ)
