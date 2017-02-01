setwd("C:/seagate/Loktra/")
#train<-read.table("trainingdata.txt", sep="\t", col.names=c("Category", "Document"), fill=FALSE, strip.white=TRUE)
fileName <- "trainingdata.txt"
conn <- file(fileName,open="r")
linn <-readLines(conn)
for (i in 2:length(linn)){
  print(linn[i])
  n<-as.numeric(linn[1])
}
close(conn)
a<-gsub('([0-9]+)([^0-9]+)', '\\1 \\2', linn[2])
for(i in 2:length(linn))
{
  id[i]<-as.numeric(gsub("[^0-9]+", " ", linn[i], fixed = FALSE))
}
gsub("[^0-9]+", " ", linn[2], fixed = FALSE)
id<-numeric(n)
nrow(id)
id
library(qdap)
library(tm)
library(class)
library(plyr)
description<-linn
descsource<-VectorSource(description)
desccorpus<-VCorpus(descsource)
desccorpus<-tm_map(desccorpus,removePunctuation)
desccorpus<-tm_map(desccorpus,removeNumbers)
desccorpus<-tm_map(desccorpus,tolower)
desccorpus<-tm_map(desccorpus,stripWhitespace)
desccorpus<-tm_map(desccorpus,removeWords,stopwords("english"))
desccorpus<-tm_map(desccorpus,content_transformer(replace_abbreviation))
desc_dtm<-DocumentTermMatrix(desccorpus)
desccorpus[[15]][1]
desccorpus <- tm_map(desccorpus, PlainTextDocument)
desc_dtm<-removeSparseTerms(desc_dtm,0.7)
mat.df <- as.data.frame(data.matrix(desc_dtm), stringsAsfactors = FALSE)
mat.df<-mat.df[2:5486,]
id<-id[2:5486]
mat.df <- cbind(mat.df, id)
#head(mat.df)
#head(linn)
#levels(id)
#head(mat.df)
training <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .7))
testing <- (1:nrow(mat.df))[- training]
category <- mat.df[, "id"]
modeldata <- mat.df[,!colnames(mat.df) %in% "id"]
pred<-knn(modeldata[training,],modeldata[testing,],category[training])
confusionmatrix <- table("Predictions" = pred, Actual = category[testing])
(accuracy <- sum(diag(confusionmatrix))/length(testing) * 100)
head(confusionmatrix)
confusionmatrix
length(testing)
sum(diag(confusionmatrix))
head(linn)
