
library(naivebayes)

placement<- read.csv("C:\\Program Files\\R\\R-3.3.2\\datasets\\Placement.csv", header = TRUE)
print(placement)


dim(placement)
length(placement)
placement[6,]
placement[,c(10,11,12,13)]
placement[1:10,]
placement[,9]
str(placement)
# DATA PREPROCESSING
summary(placement)
placement$gender=as.factor(placement$gender)
placement$ssc_b=as.factor(placement$ssc_b)
placement$hsc_b=as.factor(placement$hsc_b)
placement$hsc_s=as.factor(placement$hsc_s)
placement$degree_t=as.factor(placement$degree_t)
placement$workex=as.factor(placement$workex)
placement$specialisation=as.factor(placement$specialisation)
placement$status =as.factor(placement$status)
summary(placement)


barplot(table(placement$status),
        main="Student Placement", col="pink")

y<-placement$status 
print(y)
x1<- placement$gender
barplot(table(placement$gender),
        main="Students Gender", col="Yellow")


x2<- placement$ssc_p 
hist(x2)
x3<- placement$ssc_b
barplot(table(x3))
x4<- placement$hsc_p 
hist(x4, main = "SSC Pass Percentage",xlab="Pass Percentage", ylab="No.of Students",col = "red")
x5<- placement$hsc_b 
barplot(table(x5))

x6<- placement$hsc_s
barplot(table(x6))

x7<- placement$degree_p
x8<- placement$degree_t 
barplot(table(x8))

x9<- placement$workex
barplot(table(x9))

x10<- placement$etest_p
x12<- placement$specialisation
x13<- placement$mba_p 
x14<- placement$salary 

hist(x14, main = "Students Salary",xlab="Salary Assigned", ylab="No.of Students",col = "brown")

new_data <- cbind(x3,x4,x5,x6,x7,x8,y)
print(placement)

# Sampling of Datasets

ind <- sample(2, nrow(placement), replace=TRUE, prob=c(0.8, 0.2))
trainData <- placement[ind==1,]
testData <- placement[ind==2,]


print(trainData)
print(testData)

naive_model <- naive_bayes(status ~ gender+ssc_p+ssc_b+hsc_p+hsc_b+hsc_s+degree_p+degree_t+
                             workex+etest_p+specialisation+mba_p, data=trainData, type="C-classification")
print(naive_model)
plot(naive_model)

#predict on test data
testData1 = testData[,-c(13,14)]
testData1
testPred <- predict(naive_model, newdata = testData1)
print(testPred)
print(testData$status)


a<-data.frame(gender= "M",ssc_p =76.50, ssc_b = "Others", hsc_p =97.70, hsc_b = "Others", hsc_s ="Science",
              degree_p = 78.86, degree_t ="Sci&Tech", workex= "No",etest_p= 97.40, specialisation ="Mkt&Fin",
              mba_p =74.01)

result <- predict(naive_model,a)
print(result)

