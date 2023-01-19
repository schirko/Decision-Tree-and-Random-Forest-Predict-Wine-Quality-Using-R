library(corrplot)
library(corrgram)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gmodels)

##Collecting data/load CSV
rwine.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
rwine <- read.csv(red.url, header = TRUE, sep = ";")
rwine

##Data Exploration
str(rwine) 
summary(rwine) 
names(rwine)
table(rwine$quality)

Red_Wine <- as.numeric(rwine$quality)
hist(Red_Wine) #view histogram on quality

corrplot(cor(rwine)) #Correlation Matrix
corrgram(rwine, type="data", lower.panel=panel.conf, upper.panel=panel.shade, main= "Corrgram for wine quality dataset", order=T, cex.labels=1.2)

##Data Transformation / Preparation
#Check for NA values
is.na(rwine)
df <- data.frame(rwine)
which(is.na(df))
sum(is.na(df))
df_nonas <- na.omit(df)
sum(is.na(df_nonas))

#Modify the preidictor to a factor
rwine$quality <- as.factor(rwine$quality) 
str(rwine$quality)

##Train and Test data 4,898 objects
set.seed(123)
train_sample <- sample(4408, 490)
rwine_train <- rwine[train_sample, ]
rwine_test <- rwine[-train_sample, ]
prop.table(table(rwine_train$quality))
prop.table(table(rwine_test$quality))
dim(rwine_train)

# Evaluate the model performance
library(gmodels)
CrossTable(x=quality, y=rwine_test, prop.chisq=FALSE)

#rpart to grow tree
fit <- rpart(quality~alcohol+density+pH, method="class", data=rwine_train) # grow tree
summary(fit) #detailed summary of splits
plot(fit,uniform=TRUE,main="Classfication Tree for Red Wine Quality") # plot tree
text (fit, use.n=TRUE, all=TRUE, cex=0.8) # give label text
rpart.plot(fit, tweak = 1.6)

#Prune the tree
prunefit <- prune(fit,cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]) #prune the tree
plot(prunefit,uniform=TRUE,main="Prune Classification Tree for ") #plot pruned tree
text(prunefit,use.n=TRUE,all=TRUE,cex=0.8) # give label text

#Random Forest prediction of red wine data
fit <- randomForest(quality~alcohol+density+pH, data=rwine_train)
print(fit) # view results


