setwd("L:/Data Analysis using R/Internship Project")
 #step 1 Import Iris dataset
iris <- read.csv("iris_Dataset/iris.csv")
#step 2 Exploratory Analysis
# A)Print first 3 records from dataset
head(iris,n=3)

# B)Find Dimensions of the dataset
dim(iris)

# C)Find names and class of the dataset
names(iris)
  
class(iris)

# D)Find missing vlaues in the dataset and make data consistent.
colSums(is.na(iris))

# E)FInd structure of the dataset
str(iris) 

# F) Find mean, median, quartile, max, min data for every feature.
summary(iris)

# G)Plot a Boxplot Graph, Pie chart respective to their Species.
png(file = "iris_species_box1.jpg")
boxplot(iris$sepal_length~iris$species,col="red",xlab="Species",ylab="sepal_length",main = "Box plot for classification of Species based on Sepal Length ")
dev.off()
png(file = "iris_species_box2.jpg")
boxplot(iris$sepal_width~iris$species,col="red",xlab="Species",ylab="sepal_width",main = "Box plot for classification of Species based on Sepal Width ")
dev.off()
png(file = "iris_species_box3.jpg")
boxplot(iris$petal_length~iris$species,col="red",xlab="Species",ylab="petal_length", main = "Box plot for classification of Species based on Petal Length ")
dev.off()
png(file = "iris_species_box4.jpg")
boxplot(iris$petal_width~iris$species,col="red",xlab="Species",ylab="petal_width", main = "Box plot for classification of Species based on Sepal Width ")
dev.off()

png(file = "iris_species_pie.jpg")
pie(table(iris$species),col = c("Red","Green","Yellow"),main = "Classification of Iris species")
dev.off()

###
par(mfrow=c(2,2))
boxplot(sepal_length  ~ species, iris, main = "Sepal Length wrt Species", col = "lightpink3")
boxplot(sepal_width   ~ species, iris, main = "Sepal Width wrt Species", col = "antiquewhite1")
boxplot(petal_length  ~ species, iris, main = "Petal Length wrt Species", col = "lightskyblue4")
boxplot(petal_width  ~ species, iris, main = "Petal Width wrt Species", col = "orange1")

###


# H)Subset tuples based on their Species in different R-Object.
iris_setosa <- subset(iris, species == "setosa")
iris_versicolor <- subset(iris, species == "versicolor")
iris_virginica <- subset(iris, species == "virginica")

# I)Box plot for individual R objects
png(file = "iris_2I_1.jpg")
boxplot(iris_setosa,xlab="Species",main = "Box plot for Setosa species for Iris dataset")
dev.off()

str(iris)

#or
boxplot(iris_setosa$sepal_length,xlab="sepal_length wrt setosa",col = "lightpink3")
boxplot(iris_setosa$sepal_width,xlab="sepal_width wrt setosa", col = "antiquewhite1")
boxplot(iris_setosa$petal_length,xlab="pedal_length wrt setosa", col = "lightskyblue4")
boxplot(iris_setosa$petal_width,xlab="pedal_width wrt setosa", col = "orange1")

png(file = "iris_2I_2.jpg")
boxplot(iris_versicolor,xlab="Species",main = "Box plot for Versicolor species for Iris dataset")
dev.off()
#or
boxplot(iris_versicolor$sepal_length,xlab="sepal_length wrt versicolor", col = "lightpink3")
boxplot(iris_versicolor$sepal_width,xlab="sepal_width wrt versicolor", col = "antiquewhite1")
boxplot(iris_versicolor$petal_length,xlab="pedal_length wrt versicolor", col = "lightskyblue4")
boxplot(iris_versicolor$petal_width,xlab="pedal_width wrt versicolor", col = "orange1")

png(file = "iris_2I_3.jpg")
boxplot(iris_virginica,xlab="Species",main = "Box plot for virginica species for Iris dataset")
dev.off()
#or
boxplot(iris_virginica$sepal_length,xlab="sepal_length wrt virginica",col = "lightpink3")
boxplot(iris_virginica$sepal_width,xlab="sepal_width wrt virginica", col = "antiquewhite1")
boxplot(iris_virginica$petal_length,xlab="pedal_length wrt virginica", col = "lightskyblue4")
boxplot(iris_virginica$petal_width,xlab="pedal_width wrt virginica", col = "orange1")

# J)Plot a Histogram on feature Petal lengths of iris dataset
png(file = "iris_2J.jpg")
hist(iris$petal_length,ylim = c(0,40),xlab = "Petal Length",ylab = "Frequency",main = "Histogram for Petal Length of Iris Data Set", col = "red")
dev.off()

# K)Plot a Histogram for Petal Lengths of Different Species on different Graph.
png(file = "iris_2K_1.jpg")
hist(iris_setosa$petal_length,ylim = c(0,20),xlim = c(1,2),xlab = "Petal Length of Setosa",main = "Histogram for Petal Length wrt Setosa", col = "lightpink3")
dev.off()
png(file = "iris_2K_2.jpg")
hist(iris_versicolor$petal_length,ylim = c(0,25),xlab = "Petal Length of Versicolor",main = "Histogram for Petal Length wrt Versicolor", col = "antiquewhite1")
dev.off()
png(file = "iris_2K_3.jpg")
hist(iris_virginica$petal_length,ylim = c(0,20), xlab = "Petal Length of Virginica",main = "Histogram for Petal Length wrt Virginica", col = "lightskyblue4")
dev.off()

# L. Find correlation between multiple features also plot a scatter plot for correlation.
iris$species = factor(iris$species,
                        levels = c('setosa', 'versicolor', 'virginica'),
                        labels = c(1, 2, 3))
iris$species <- as.numeric(iris$species)

pairs(iris[,1:4])
x <- cor(iris[,1:4])
print(x)
corrplot::corrplot(x)

# 4. Classify Data based on iris Species and plot a Decision Tree.
library("caTools")
library("rpart.plot")
library("rpart")
library("RColorBrewer")
library("rattle")
sam <- sample.split(iris$sepal_length,SplitRatio = 0.70)
print(sam)
training = subset(iris , sam==TRUE )
testing = subset(iris , sam==FALSE )
length(training)
length(testing$species)

model = rpart(species~.,data = training)
png(file="iris_decision_tree.png")
fancyRpartPlot(model)
dev.off()
model$variable.importance
rm(result)
result = predict(model,testing,type='class')

table(result,testing$species)
