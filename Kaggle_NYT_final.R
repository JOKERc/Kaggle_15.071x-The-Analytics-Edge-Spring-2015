Train = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
Test = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
ID = Test$UniqueID
popular = as.factor(Train$Popular)
Train$Popular = NULL
end = nrow(Train)
full = rbind(Train,Test)
full$PubDate = strptime(full$PubDate, "%Y-%m-%d %H:%M:%S")
full$Weekday = weekdays(full$PubDate)
full$Hour = full$PubDate$hour
full$WordCount = log(50+full$WordCount)
full = select(full,NewsDesk,SectionName,SubsectionName,WordCount,Weekday,Hour)


full$SectionName = as.factor(full$SectionName)
full$SubsectionName = as.factor(full$SubsectionName)
full$NewsDesk = as.factor(full$NewsDesk)
full$Weekday = as.factor(full$Weekday)
full$Hour = as.factor(full$Hour)

Train = full[1:end,]
Train$Popular = popular
Test = full[-(1:end),]


##sparse  single word
sparse = removeSparseTerms(dtm, 0.998)
HeadlineWords = as.data.frame(as.matrix(sparse))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
HeadlineWordsTrain = head(HeadlineWords, nrow(Train))
HeadlineWordsTest = tail(HeadlineWords, nrow(Test))

# ngram sparse
tdm = removeSparseTerms(tdmH,0.999)
terms = as.data.frame(as.matrix(tdm))
terms =data.frame(t(terms)) 
HeadtermsTrain =  head(terms,nrow(Train))
HeadtermsTest = tail(terms,nrow(Test))


HeadlineWordsTrain = cbind(HeadlineWordsTrain,HeadtermsTrain)
HeadlineWordsTest = cbind(HeadlineWordsTest,HeadtermsTest)

HeadlineWordsTrain = cbind(HeadlineWordsTrain,Train)
HeadlineWordsTest = cbind(HeadlineWordsTest,Test)

#######  lasso
x=model.matrix(Popular~.,HeadlineWordsTrain) 
y=HeadlineWordsTrain$Popular
fit.lasso=glmnet(x,y,family = "binomial")  #bridge
plot(fit.lasso,xvar="lambda",label=TRUE)

cv.lasso=cv.glmnet(x,y,family = "binomial",type.measure="auc")  #lasso
plot(cv.lasso)
bestlam=cv.lasso$lambda.min
bestlam
coef(cv.lasso,bestlam)
findFreqTerms(tdmT,lowfreq = 5)
findFreqTerms(dtmH,lowfreq = 16)

#####
############

# rf cv
train = HeadlineWordsTrain
k = 5
n = floor(nrow(train)/k)
err.vect = rep(NA,k)
for (i in 1:k ){
  s1 = ((i-1)*n+1)
  s2 = i*n
  subset = s1:s2
  cv.train = train[-subset,]
  cv.test = train[subset,]
  indexpop = which(colnames(train)== "Popular")
  fit = randomForest(Popular~NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour
                     +reader.respond+friday.night
                     +dont+can+today
                     ,cv.train)
  pred = predict(fit,cv.test[,-indexpop],type = "prob")[,2]
  err.vect[i] = roc.area(as.numeric(as.character(cv.test[,indexpop])),pred)$A
  print(paste("AUC for fold", i,":",err.vect[i]))
}
print(paste("Average Auc:", mean(err.vect)))


rf2 = randomForest(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour
                   +reader.respond+friday.night
                   +dont+can+today
                   ,HeadlineWordsTrain)
importance(rf2)




# nnet cv

train = HeadlineWordsTrain
k = 10
n = floor(nrow(train)/k)
err.vect = rep(NA,k)
for (i in 1:k ){
  s1 = ((i-1)*n+1)
  s2 = i*n
  subset = s1:s2
  cv.train = train[-subset,]
  cv.test = train[subset,]
  indexpop = which(colnames(train)== "Popular")
  fit = nnet(Popular~NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour
             +reader.respond+friday.night
             +dont+can+today
             ,cv.train,size = 6, rang = 0.5, decay = 0.4 , maxit = 400)
  pred = predict(fit,cv.test[,-indexpop])
  err.vect[i] = roc.area(as.numeric(as.character(cv.test[,indexpop])),pred)$A
  print(paste("AUC for fold", i,":",err.vect[i]))
}
print(paste("Average Auc:", mean(err.vect)))







################
rf = randomForest(Popular ~ .,Train,mtry =2)
predrf = predict(rf,Test,type = "prob")[,2]


rf2 = randomForest(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour
                   +reader.respond+friday.night
                   +dont+can+today
                   ,HeadlineWordsTrain)
predrf2 = predict(rf2,HeadlineWordsTest,type = "prob")[,2]
importance(rf2)



nne = nnet(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour
           +reader.respond+friday.night
           +dont+can+today
           ,HeadlineWordsTrain,size = 6, rang = 0.5, decay = 0.4 , maxit = 600)
prednne = predict(nne,HeadlineWordsTest)

##
gbmGrid <-  expand.grid(interaction.depth = 13, n.trees = 2000, shrinkage = 0.005)
nf  <- trainControl(method="cv", number=10, classProbs = TRUE, summaryFunction = twoClassSummary)
HeadlineWordsTrain$Popular = as.factor(ifelse(HeadlineWordsTrain$Popular==1, 'yes', 'no'))
gbmtr <- train(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour
               +reader.respond+friday.night
               +dont+can+today
               , HeadlineWordsTrain, method = "gbm",trControl = nf, tuneGrid=gbmGrid, metric ="ROC",verbose = T)

predgbmtr <- predict(gbmtr, HeadlineWordsTest, type="prob")[,2]

#~~~~~~~~~~~~~~~~
# submisson



predfinal = (predrf2+predgbmtr+prednne)/3
summary(predfinal)
MySubmission = data.frame(UniqueID = ID, Probability1 = predfinal)
write.csv(MySubmission, "SubmissionFINAL!.csv", row.names=FALSE)

