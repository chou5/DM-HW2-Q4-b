####### Iris ######
require(rpart)
require(rpart.plot)
require(caret)
attach(iris)
names(iris) = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
set.seed(9850)
g <- runif(nrow(iris))
irisr <- iris[order(g),]
m3 <-rpart(Species ~ .,data=irisr[1:100,], method="class")
print(m3)
rpart.plot(m3, type = 3, extra=101, fallen.leaves=T)

p3 <- predict(m3,irisr[101:150,], type="class")
acc_table1 <- table(irisr[101:150,5], predicted=p3)
acc_table1

#10-fold cross validation


iris<-iris[sample(nrow(iris)),]
folds <- cut(seq(1,nrow(iris)),breaks=10,labels=FALSE)


sum_accuracy = 0

#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = iris[testIndexes, ]
  trainData = iris[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]  
  
  model = rpart(Species ~ .,data=trainDatar, method="class")
  
  p3 = predict(model,testData, type="class")
  acc_table = table(testData[,5], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:3){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc

}

sum_accuracy/10

########  Wine   ############

require(rpart)
require(rpart.plot)
require(caret)
attach(wine)
names(wine) = c("Class","Alcohol","Malic.acid","Ash","Alcalinity.of.ash","Magnesium","Total.phenols","Flavanoids","Nonflavanoid.phenols","Proanthocyanins","Color.intensity","Hue","OD280/OD315.of.diluted.wines","Proline")

set.seed(9850)
g <- runif(nrow(wine))
winer <- wine[order(g),]
m3 <-rpart(Class ~ .,data=winer[1:100,], method="class")
print(m3)
rpart.plot(m3, type = 3, extra=101, fallen.leaves=T)

p3 <- predict(m3,winer[101:150,], type="class")
acc_table1 <- table(winer[101:150,1], predicted=p3)
acc_table1

#10-fold cross validation


wine<-wine[sample(nrow(wine)),]
folds <- cut(seq(1,nrow(wine)),breaks=10,labels=FALSE)


sum_accuracy = 0

#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = wine[testIndexes, ]
  trainData = wine[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]  
  
  model = rpart(Class ~ .,data=trainDatar, method="class")
  
  p3 = predict(model,testData, type="class")
  acc_table = table(testData[,1], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:3){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc
  
}

sum_accuracy/10

######### Donation ###########

require(rpart)
require(rpart.plot)
require(caret)
attach(donation_r)
names(donation_r) = c("Recency","Frequency","Monetary","Time","Donate_Blood")

set.seed(9850)
g <- runif(nrow(donation_r))
donation_rr <- donation_r[order(g),]
m3 <-rpart(Donate_Blood ~ .,data=donation_rr[1:100,], method="class")
#print(m3)
#rpart.plot(m3, type = 3, extra=101, fallen.leaves=T)

p3 <- predict(m3,donation_rr[101:150,], type="class")
acc_table1 <- table(donation_rr[101:150,5], predicted=p3)
acc_table1

#10-fold cross validation


donation_r<-donation_r[sample(nrow(donation_r)),]
folds <- cut(seq(1,nrow(donation_r)),breaks=10,labels=FALSE)


sum_accuracy = 0

#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = donation_r[testIndexes, ]
  trainData = donation_r[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]  
  
  model = rpart(Donate_Blood ~ .,data=trainDatar, method="class")
  
  p3 = predict(model,testData, type="class")
  acc_table = table(testData[,5], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:2){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc
  
}

sum_accuracy/10

######## Breast Cancer########

require(rpart)
require(rpart.plot)
require(caret)
attach(breast)
names(breast) = c("Code_number","Clump_Thickness","Cell_size","Cell_shape","Marginal_adhesion","Single_epithelial_cell_size", "Bare_nuvlei","Bland_chromatin","Normal_nucleoli","Mitoss","Class")
set.seed(9850)
g <- runif(nrow(breast))
breastr <- breast[order(g),]
m3 <-rpart(Class ~ .,data=breastr[1:100,], method="class")
print(m3)
rpart.plot(m3, type = 3, extra=101, fallen.leaves=T)

p3 <- predict(m3,breastr[101:150,], type="class")
acc_table1 <- table(breastr[101:150,11], predicted=p3)
acc_table1

#10-fold cross validation


breast<-breast[sample(nrow(breast)),]
folds <- cut(seq(1,nrow(breast)),breaks=10,labels=FALSE)


sum_accuracy = 0

#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = breast[testIndexes, ]
  trainData = breast[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]  
  
  model = rpart(Class ~ .,data=trainDatar, method="class")
  
  p3 = predict(model,testData, type="class")
  acc_table = table(testData[,11], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:2){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc
  
}

sum_accuracy/10

########## Abalone #########

require(rpart)
require(rpart.plot)
require(caret)
attach(abalone)
names(abalone) = c("Sex","Length","Diameter","Height","Whole_weight","Shucked_weight","Viscera_weight","Shell_weight","Rings")
set.seed(9850)
g <- runif(nrow(abalone))
abaloner <- abalone[order(g),]
m3 <-rpart(Sex ~ .,data=abaloner[1:100,], method="class")
print(m3)
rpart.plot(m3, type = 3, extra=101, fallen.leaves=T)

p3 <- predict(m3,abaloner[101:150,], type="class")
acc_table1 <- table(abaloner[101:150,1], predicted=p3)
acc_table1

#10-fold cross validation


abalone<-abalone[sample(nrow(abalone)),]
folds <- cut(seq(1,nrow(abalone)),breaks=10,labels=FALSE)


sum_accuracy = 0

#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = abalone[testIndexes, ]
  trainData = abalone[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]  
  
  model = rpart(Sex ~ .,data=trainDatar, method="class")
  
  p3 = predict(model,testData, type="class")
  acc_table = table(testData[,1], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:3){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc
  
}

sum_accuracy/10


######### balance scale ##########

require(rpart)
require(rpart.plot)
require(caret)
attach(ballon)
names(ballon) = c("class","left_weight","left_distance","right_weight","right_distance")
set.seed(9850)
g <- runif(nrow(ballon))
ballonr <- ballon[order(g),]
m3 <-rpart(class ~ .,data=ballonr[1:100,], method="class")
print(m3)
rpart.plot(m3, type = 3, extra=101, fallen.leaves=T)

p3 <- predict(m3,ballonr[101:150,], type="class")
acc_table1 <- table(ballonr[101:150,1], predicted=p3)
acc_table1

#10-fold cross validation


ballon<-ballon[sample(nrow(ballon)),]
folds <- cut(seq(1,nrow(ballon)),breaks=10,labels=FALSE)


sum_accuracy = 0

#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = ballon[testIndexes, ]
  trainData = ballon[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]  
  
  model = rpart(class ~ .,data=trainDatar, method="class")
  
  p3 = predict(model,testData, type="class")
  acc_table = table(testData[,1], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:3){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc
  
}

sum_accuracy/10

####### Solar Flare #######

require(rpart)
require(rpart.plot)
require(caret)
attach(solor)
names(solor) = c("class","large.spot.size","spot.distribution","activity","evolution","previous.24.hour.flare.activity","historically-complex","across.the.sun's.disk","area","area.of.the.largest.spot","c-class","m-class","x-class")
set.seed(9850)
g <- runif(nrow(solor))
solorr <- solor[order(g),]
m3 <-rpart(evolution ~ .,data=solorr[1:100,], method="class")
print(m3)
rpart.plot(m3, type = 3, extra=101, fallen.leaves=T)

p3 <- predict(m3,solorr[101:150,], type="class")
acc_table1 <- table(solorr[101:150,5], predicted=p3)
acc_table1

#10-fold cross validation


solor<-solor[sample(nrow(solor)),]
folds <- cut(seq(1,nrow(solor)),breaks=10,labels=FALSE)


sum_accuracy = 0

#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = solor[testIndexes, ]
  trainData = solor[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]  
  
  model = rpart(evolution ~ .,data=trainDatar, method="class")
  
  p3 = predict(model,testData, type="class")
  acc_table = table(testData[,5], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:3){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc
  
}

sum_accuracy/10

####### Teaching Assistant Evaluation ######

require(rpart)
require(rpart.plot)
require(caret)
attach(tae)
names(tae) = c("native.english.speaker","course.instructor","course","summer.or.regular.semester","class.size","class_attribute")
set.seed(9850)
g <- runif(nrow(tae))
taer <- tae[order(g),]
m3 <-rpart(class_attribute ~ .,data=taer[1:100,], method="class")
print(m3)
rpart.plot(m3, type = 3, extra=101, fallen.leaves=T)

p3 <- predict(m3,taer[101:150,], type="class")
acc_table1 <- table(taer[101:150,6], predicted=p3)
acc_table1

#10-fold cross validation


tae<-tae[sample(nrow(tae)),]
folds <- cut(seq(1,nrow(tae)),breaks=10,labels=FALSE)


sum_accuracy = 0

#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = tae[testIndexes, ]
  trainData = tae[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]  
  
  model = rpart(class_attribute ~ .,data=trainDatar, method="class")
  
  p3 = predict(model,testData, type="class")
  acc_table = table(testData[,6], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:3){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc
  
}

sum_accuracy/10


##### heart #####
require(rpart)
require(rpart.plot)
require(caret)
attach(heart)
names(heart) = c("age","Sex","chest.pain.type","resting.bp","serum","fasting.bs","resting.elec","max.heart.rate","exercise.induced.angina","oldpeak","ST","vessel","thal")
set.seed(9850)
g <- runif(nrow(heart))
irisr <- heart[order(g),]
m3 <-rpart(thal ~ .,data=heartr[1:100,], method="class")
print(m3)
rpart.plot(m3, type = 3, extra=101, fallen.leaves=T)

p3 <- predict(m3,heartr[101:150,], type="class")
acc_table1 <- table(heartr[101:150,13], predicted=p3)
acc_table1

#10-fold cross validation


heart<-heart[sample(nrow(heart)),]
folds <- cut(seq(1,nrow(heart)),breaks=10,labels=FALSE)


sum_accuracy = 0

#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = heart[testIndexes, ]
  trainData = heart[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]  
  
  model = rpart(thal ~ .,data=trainDatar, method="class")
  
  p3 = predict(model,testData, type="class")
  acc_table = table(testData[,13], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:3){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc
  
}

sum_accuracy/10

###### poker ######

require(rpart)
require(rpart.plot)
require(caret)
attach(poker)
names(poker) = c("s1","c1","s2","c2","s3","c3","s4","c4","s5","c5","class")
set.seed(9850)
g <- runif(nrow(poker))
pokerr <- poker[order(g),]
m3 <-rpart(class ~ .,data=pokerr[1:100,], method="class")
print(m3)
rpart.plot(m3, type = 3, extra=101, fallen.leaves=T)

p3 <- predict(m3,pokerr[101:150,], type="class")
acc_table1 <- table(pokerr[101:150,11], predicted=p3)
acc_table1

#10-fold cross validation


poker<-poker[sample(nrow(poker)),]
folds <- cut(seq(1,nrow(poker)),breaks=10,labels=FALSE)


sum_accuracy = 0

#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = poker[testIndexes, ]
  trainData = poker[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]  
  
  model = rpart(class ~ .,data=trainDatar, method="class")
  
  p3 = predict(model,testData, type="class")
  acc_table = table(testData[,11], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:4){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc
  
}

sum_accuracy/10
