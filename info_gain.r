require(C50)
attach(iris)
names(iris) = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
set.seed(9850)
g <- runif(nrow(iris))
irisr <- iris[order(g),]
str(irisr)

m1 <- C5.0(irisr[1:100,-5], irisr[1:100,5])
m1
summary(m1)
p1 <- predict(m1, irisr[101:150,])
table(irisr[101:150,5], Predicted=p1)
plot(m1)

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
  model = C5.0(trainDatar[,-5], trainDatar[,5])
  
  
  p3 = predict(model,testData)
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

######### wine #########

require(C50)
attach(wine)
names(wine) = c("Class","Alcohol","Malic.acid","Ash","Alcalinity.of.ash","Magnesium","Total.phenols","Flavanoids","Nonflavanoid.phenols","Proanthocyanins","Color.intensity","Hue","OD280/OD315.of.diluted.wines","Proline")

wine$Class = as.factor(wine$Class)
set.seed(9850)
g <- runif(nrow(wine))
winer <- wine[order(g),]

m1 <- C5.0(winer[1:100,-1], winer[1:100,1])
m1
summary(m1)
p1 <- predict(m1, winer[101:150,])
table(winer[101:150,1], Predicted=p1)
plot(m1)

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
  model = C5.0(trainDatar[,-1], trainDatar[,1])
  
  
  p3 = predict(model,testData)
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

##### breast cancer #####
require(C50)
attach(breast)
names(breast) = c("Code_number","Clump_Thickness","Cell_size","Cell_shape","Marginal_adhesion","Single_epithelial_cell_size", "Bare_nuvlei","Bland_chromatin","Normal_nucleoli","Mitoss","Class")
breast$Class = as.factor(breast$Class)
set.seed(9850)
g <- runif(nrow(breast))
breastr <- breast[order(g),]

m1 <- C5.0(breastr[1:100,-11], breastr[1:100,11])
m1
summary(m1)
p1 <- predict(m1, breastr[101:150,])
table(breastr[101:150,11], Predicted=p1)
plot(m1)

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
  model = C5.0(trainDatar[,-11], trainDatar[,11])
  
  
  p3 = predict(model,testData)
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

#### abalone######

require(C50)
attach(abalone)
names(abalone) = c("Sex","Length","Diameter","Height","Whole_weight","Shucked_weight","Viscera_weight","Shell_weight","Rings")
abalone$Sex = as.factor(abalone$Sex)
set.seed(9850)
g <- runif(nrow(abalone))
abaloner <- abalone[order(g),]

m1 <- C5.0(abaloner[1:100,-1], abaloner[1:100,1])
m1
summary(m1)
p1 <- predict(m1, abaloner[101:150,])
table(abaloner[101:150,1], Predicted=p1)
plot(m1)
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
  model = C5.0(trainDatar[,-1], trainDatar[,1])
  
  
  p3 = predict(model,testData)
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

####### balance scale #####

require(C50)
attach(ballon)
names(ballon) = c("class","left_weight","left_distance","right_weight","right_distance")
set.seed(9850)
ballon$class = as.factor(ballon$class)
set.seed(9850)
g <- runif(nrow(ballon))
ballonr <- ballon[order(g),]

m1 <- C5.0(ballonr[1:100,-1], ballonr[1:100,1])
m1
summary(m1)
p1 <- predict(m1, ballonr[101:150,])
table(ballonr[101:150,1], Predicted=p1)
plot(m1)
ballon<-ballon[sample(nrow(ballon)),]
folds <- cut(seq(1,nrow(ballon)),breaks=10,labels=FALSE)


sum_accuracy = 0


#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = abalone[testIndexes, ]
  trainData = abalone[-testIndexes, ]
  
  g = runif(nrow(trainData))
  trainDatar = trainData[order(g),]
  model = C5.0(trainDatar[,-1], trainDatar[,1])
  
  
  p3 = predict(model,testData)
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

###### solar flare ######

require(C50)
attach(solor)
names(solor) = c("class","large.spot.size","spot.distribution","activity","evolution","previous.24.hour.flare.activity","historically-complex","across.the.sun's.disk","area","area.of.the.largest.spot","c-class","m-class","x-class")
set.seed(9850)
solor$evolution = as.factor(solor$evolution)
set.seed(9850)
g <- runif(nrow(solor))
solorr <- solor[order(g),]

m1 <- C5.0(solorr[1:100,-5], solorr[1:100,5])
m1
summary(m1)
p1 <- predict(m1, solorr[101:150,])
table(solorr[101:150,5], Predicted=p1)
plot(m1)
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
  model = C5.0(trainDatar[,-5], trainDatar[,5])
  
  
  p3 = predict(model,testData)
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

###### Teaching Assistant Evaluation ######

require(C50)
attach(tae)
names(tae) = c("native.english.speaker","course.instructor","course","summer.or.regular.semester","class.size","class_attribute")
set.seed(9850)
tae$class_attribute = as.factor(tae$class_attribute)
set.seed(9850)
g <- runif(nrow(tae))
taer <- tae[order(g),]

m1 <- C5.0(taer[1:100,-6], taer[1:100,6])
m1
summary(m1)
p1 <- predict(m1, taer[101:150,])
table(taer[101:150,6], Predicted=p1)
plot(m1)
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
  model = C5.0(trainDatar[,-6], trainDatar[,6])
  
  
  p3 = predict(model,testData)
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

require(C50)
attach(heart)
names(heart) = c("age","Sex","chest.pain.type","resting.bp","serum","fasting.bs","resting.elec","max.heart.rate","exercise.induced.angina","oldpeak","ST","vessel","thal")
set.seed(9850)
heart$thal = as.factor(heart$thal)
set.seed(9850)
g <- runif(nrow(heart))
heartr <- heart[order(g),]

m1 <- C5.0(heartr[1:100,-13], heartr[1:100,13])
m1
summary(m1)
p1 <- predict(m1, heartr[101:150,])
table(heartr[101:150,13], Predicted=p1)
plot(m1)
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
  model = C5.0(trainDatar[,-13], trainDatar[,13])
  
  
  p3 = predict(model,testData)
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

require(C50)
attach(poker)
names(poker) = c("s1","c1","s2","c2","s3","c3","s4","c4","s5","c5","class")
set.seed(9850)
poker$class = as.factor(poker$class)
set.seed(9850)
g <- runif(nrow(poker))
pokerr <- poker[order(g),]

m1 <- C5.0(pokerr[1:100,-11], pokerr[1:100,11])
m1
summary(m1)
p1 <- predict(m1, pokerr[101:150,])
table(pokerr[101:150,11], Predicted=p1)
plot(m1)
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
  model = C5.0(trainDatar[,-11], trainDatar[,11])
  
  
  p3 = predict(model,testData)
  acc_table = table(testData[,11], predicted=p3)
  #print(acc_table)
  
  count = 0
  for (i in 1:9){
    count=count+acc_table[i,i]
    #print(count)
  }
  get_acc = count/sum(acc_table)
  sum_accuracy = sum_accuracy+get_acc
  
}

sum_accuracy/10




