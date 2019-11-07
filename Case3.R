##Caso 3
indices <- c(2,5,8,11,14)
Data3 <- dataT1snap1706[,indices]
smp_size <- floor(0.60 * nrow(Data3))
set.seed(123)
train_ind <- sample(seq_len(nrow(Data3)), size = smp_size)
trainNN3 <- Data3[train_ind, ]
testNN3 <- Data3[-train_ind, ]


#Rede Neuronal
library(neuralnet)
n<-names(Data3)
f <- as.formula(paste("anomalyQ ~", paste(n[!n %in% "anomalyQ"], collapse = " + ")))
nn <- neuralnet(f, data = trainNN3, hidden=c(3,2), linear.output=FALSE, threshold=0.01)
plot(nn)

nn.results <- compute(nn, testNN3[,1:4])
results <- data.frame(actual = testNN3$anomalyQ, prediction = nn.results$net.result)
roundedresultsdf<-data.frame(sapply(results,round,digits=0))

k=0
for(i in 1:7470) {    
  if(roundedresultsdf[i,1]!=roundedresultsdf[i,2]) k=k+1
}
print(k)

#kNN
install.packages("class")
library(class)
knn_pred = knn(trainNN3[,1:4],testNN3[,1:4], trainNN1$anomalyQ)

table(knn_pred, testNN3$anomalyQ)
##Naive Bayes

model = naiveBayes(anomalyQ ~., trainNN3)
nb_pred = predict(model, testNN3)
nb_pred
table(nb_pred, testNN3$anomalyQ)

##C5.0
rule_mod <- C5.0(x = trainNN3[, 1:4], y = trainNN3$anomalyQ, rules = TRUE)
table(predict(rule_mod, newdata = testNN3[, 1:4]), testNN3$anomalyQ)
