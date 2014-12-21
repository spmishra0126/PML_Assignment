library(ElemStatLearn)
library(caret)
library(rpart)
library(randomForest)
set.seed(357)

check_file_exist <- function(file_path) 
{
        if (!file.exists(file_path))
                stop("The ", file_path, " not found!") else TRUE 
}

load_data <- function(data_dir , fileURL, fileSource) 
{
        # Dataset check and load 
        
        source_path <- paste(data_dir, "/", fileSource , sep="")
        txt_file <- paste(data_dir, "/","activity.csv", sep="")
        
        if (!file.exists(txt_file)) {
                message(paste("Please Wait! Download...", fileURL, "..."));
                download.file(fileURL, destfile=source_path);
        } 
        data <- read.csv(txt_file,
                         header=TRUE,  na.strings=c("NA",""))
        data$interval <- factor(data$interval)
        data$date <- as.Date(data$date, format="%Y-%m-%d")
        data        
        
}

data_dir <- "F:/Data Science_Coursera/Machine Learning/Course Project/My Project/Data";

if (!file.exists(data_dir)){
        # data_dir <- readline(prompt = "Please, inform your data directory path: ")
        data_dir <-"./PML_PA/Data" ## simulate a valid data entry just because we use a Rmd
        if (!file.exists(data_dir)){
                stop("You inform a invalid directory path")
        }
}


fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv" 
fileSource <-"pml-training.csv"
source_path <- paste(data_dir, "/", fileSource , sep="")
txt_file <- paste(data_dir, "/", fileSource, sep="")

if (!file.exists(txt_file)) {
        message(paste("Please Wait! Download...", fileURL, "..."));
        download.file(fileURL, destfile=source_path);
}
pml_CSV <- read.csv(txt_file, header=TRUE, sep=",", na.strings=c("NA",""))

pml_CSV <- pml_CSV[,-1] # Remove the first column that represents a ID Row

inTrain = createDataPartition(pml_CSV$classe, p=0.60, list=FALSE)
training = pml_CSV[inTrain,]
validating = pml_CSV[-inTrain,]

sum((colSums(!is.na(training[,-ncol(training)])) < 0.6*nrow(training)))

Keep <- c((colSums(!is.na(training[,-ncol(training)])) >= 0.6*nrow(training)))
training   <-  training[,Keep]
validating <- validating[,Keep]

model <- randomForest(classe~.,data=training)
print(model)

importance(model)

confusionMatrix(predict(model,newdata=validating[,-ncol(validating)]),validating$classe)

accuracy<-c(as.numeric(predict(model,newdata=validating[,-ncol(validating)])==validating$classe))
accuracy<-sum(accuracy)*100/nrow(validating)

accurancy

fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv" 
fileSource <-"pml-testing.csv"
source_path <- paste(data_dir, "/", fileSource , sep="")
txt_file <- paste(data_dir, "/", fileSource, sep="")

if (!file.exists(txt_file)) {
        message(paste("Please Wait! Download...", fileURL, "..."));
        download.file(fileURL, destfile=source_path);
}
pml_CSV <- read.csv(txt_file, header=TRUE, sep=",", na.strings=c("NA",""))

pml_CSV <- pml_CSV[,-1] # Remove the first column that represents a ID Row
pml_CSV <- pml_CSV[,-ncol(pml_CSV)] # Remove the problem ID

pml_CSV <- pml_CSV[,Keep] # Keep the same columns of testing dataset

# class_check <- (sapply(pml_CSV, class) == sapply(training, class))
# pml_CSV[, !class_check] <- sapply(pml_CSV[, !class_check], as.numeric)

testing <- rbind(training[1, -59] , pml_CSV) # Coerce testing dataset to same class and strucuture of training dataset 

predictions <- predict(model,newdata=testing[-1,])
print(predictions)



