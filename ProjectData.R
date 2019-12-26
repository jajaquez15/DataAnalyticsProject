##install.packages("cluster")
install.packages("RWeka")
install.packages("infotheo")
install.packages("class")
install.packages("randomForest")
library(randomForest)
library(class)
library(arules)
library(arulesViz)
library(ggplot2)
library("cluster")
require(caTools)
library(e1071)
library("klaR")
library("caret")
require(rpart)
require(rpart.plot)
library("RWeka")
library(infotheo)



#-----------------------PREPARING THE DATA-----------------------------
#view(data)
#dataTrans<-read.csv("Macintosh HD/Users/jamylaneudy/Desktop/military.csv⁩")
#dataTrans<-read.transactions("MacintoshHD/⁩⁨Users/⁨jamylaneudy/⁨Desktop/military.csv⁩⁩",format="basket",sep=",")
dataLabel<-militaryHeaders[,c(1:3,53:63)]
dataNoLabel<-military[c(2:265),c(1:3,53:63)]
data<-militaryHeaders[,c(1:3,53:63)]
df<-militaryHeaders[,c(1:3,53:63)]
#select country only
data<-data[(data$Type == "Country"),]
#takes away the unnecessary column
dataNoLabel<- dataNoLabel[,c(4:14)]
dataLabel<-dataLabel[,c(3:14)]
data<-data[,c(1,4:14)]
#take out NA's
df<-na.omit(df)
data<-na.omit(data)
dataLabel<-na.omit(dataLabel)
dataNoLabel<-na.omit(dataNoLabel)

#clean the infinite numbers from dataset
#dataLabel <- dataLabel[is.finite(rowSums(dataLabel)),]
dataNoLabel <- dataNoLabel[is.finite(rowSums(dataNoLabel)),]
str(dataNoLabel)

#-----------------------Clustered------------------
View(dataLabel)
summary(dataNoLabel)
clust<-kmeans(dataNoLabel,3)
clust
#differetiated the amount of countries into to sections
model1<-kmeans(dataNoLabel,3)
#didnt really see any changes
model2<-kmeans(dataNoLabel,2)
#broke down more countries
model3<-kmeans(dataNoLabel,4)
#get Clustered 
cluste<-data.frame(dataLabel, clust$cluster)
clusteredView<-data.frame(dataLabel,model1$cluster)
clusteredView2<-data.frame(dataLabel,model2$cluster)
clusteredView3<-data.frame(dataLabel,model3$cluster)


#top 5 countries that have high military expenditures
#US,china,germany,FRance, UK(2008-2018)
#visualize
#plot(fedData$author ~jitter(fModel$cluster,1), pch=21,col=fedData$filename)
#plot(dataLabel$V1 ~jitter(model1$cluster,1), pch=21,col=dataLabel)

#show connections among the countries
d = dist(as.matrix(cluste))
hc=hclust(d)
plot(hc)
#---------------------------------Naive bayes-----------------------------------------
#determine whether or not is a country or region
nrows <- nrow(df)
rand <- sample(1:nrows)
cutPoint2_3<- floor(2*nrows/3)
trainData<-df[rand[1:cutPoint2_3],]
testData <- df[rand[cutPoint2_3+1:nrows],]

str(trainData)
str(testData)

nb=naiveBayes(Type~., data = trainData, laplace = 1)
nb2=naiveBayes(Type~., data = trainData, laplace = 4)
nb3=naiveBayes(Type~., data = trainData, laplace = 10)
pred=predict(nb, newdata=testData)
pred2=predict(nb2, newdata=testData)
pred3=predict(nb3, newdata=testData)

cType=c("Type")
id_col=testData[cType]
newpred=cbind(id_col,pred)
head(cbind(id_col,pred))
#---------------------------------------Decision Tree--------------------------------
#numeric to nominal
NN <- make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal") 
trainset <- NN(data=trainData, control= Weka_control(R="1-3"), na.action = NULL) 
testset <- NN(data=testData, control= Weka_control(R="1,3"), na.action = NULL)
#Replace all the missin1`1` values
MS <- make_Weka_filter("weka/filters/unsupervised/attribute/ReplaceMissingValues") #
trainset <-MS(data=trainset, na.action = NULL)
testset <-MS(data=testset, na.action = NULL)

m=J48(Type~., data = trainset)
m=J48(Type~., data = trainset, control=Weka_control(U=FALSE, M=2, C=0.5))

WOW("J48")
#best decision tree gaves an accuracy of 83.3%
e <- evaluate_Weka_classifier(m,numFolds = 10,seed = 1, class = TRUE)
e2 <- evaluate_Weka_classifier(m,numFolds = 10,seed = 3, class = TRUE)
e4 <- evaluate_Weka_classifier(m,numFolds = 9,seed = 3, class = TRUE)

pre2=predict(m, newdata=testset, type = c("class"))
#-----------------------------------Support Vector Machine---------------------------
#numeric to nominal
NN <- make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal") 
trainset <- NN(data=trainData, control= Weka_control(R="1-3"), na.action = NULL) 
testset <- NN(data=testData, control= Weka_control(R="1,3"), na.action = NULL)
#Replace all the missing values
MS <- make_Weka_filter("weka/filters/unsupervised/attribute/ReplaceMissingValues") #
trainset <-MS(data=trainset, na.action = NULL)
testset <-MS(data=testset, na.action = NULL)

svm<-svm(Type~., data=trainset)
predSVM=predict(svm, newdata=testset, type=c("class"))
Name=c("Type")
id_col=testset[Name]
newpred=cbind(id_col, predSVM)
colnames(newpred)=c("Type", "Pre")

#-------------------------------------Random Forest--------------------------------------------
#Random forest could handle the dataset
smallerTrain<-trainset[c(1:40),c(1:10)]
smallerTest<-

rfm<-randomForest(formula = Type ~., data = smallerTrain, ntree=50)
#not applicable

#---------------------------------------KNN--------------------------------------------
dTrain<-df$Type
dtrainset<-trainset[,(2:14)]
dTestset<-testset[,(2:14)]
predKNN<-knn(train=trainData, test=testData, cl=trainData$Type, k=3)
#not applicable
#-------------------------Associative Rule---------------


dfA<-df[,c(1,3)]
rules<-apriori(dfA, parameter = list(supp = 0.001, conf = 0.8))
rules2<-apriori(dfA, parameter = list(supp = 0.06, conf = 0.8))
rules3<-apriori(dfA, parameter = list(supp = 0.001, conf = 0.5))
options(digits=2)
inspect(rules[1:5])
options(digits=2)
inspect(rules2)
options(digits=2)
inspect(rules3[1:5])



