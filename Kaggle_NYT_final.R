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
# ... feature select
agenda+amazon+anoth+appl+allergan+among+attack+ban+bond+british+busi+can+case+center+child+close+colleg+compani+cook+corpor+critic+daili+david+dead+deal+digit+doctor+econom+energi+expect+friday+game+get+girl+group+isi+jpmorgan+join+lawsuit+lesson+comment+dont+expect+facebook+fashion+fear+ferguson+game+goldman+good+health+morn+parent+rais+seek+love+market++mean+microsoft+miss+movi+notebook++now+one+parent+person+phone+polic+power+prepar+problem+push+realli+river+seek+sign+startup+star+swift+target+tell+varieti+voter++weigh+thanksgiv+turn+uber+wall+want+can+like+music+move+read+recap+report+today+war+NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour

first.draft.focus+friday.night+hong.kong.protest+insid.trade+reader.respond+taylor.swift++social.media+wall.street+today.polit+wall.st+morn.agenda+
  agenda+amazon+anoth+appl+busi++comment+dont+expect+facebook+fashion+fear++ferguson+game+goldman+good+health+morn+parent+rais+seek+thanksgiv+turn+uber+wall+want+can+like+music+move+read+recap+report+today+war+NewsDesk+SectionName+SubsectionName+WordCount+Weekday+Hour


+taylor.swift+wall.st+wall.street+hong.kong.protest+vegetarian.thanksgiv
+social.media+report.notebook+reader.respond+open.comment+joan.river
+joe.wnyc+homeland.recap+good.wife+goldman.sach+friday.night+data.breach
+comment.necessari+daili.report+big.bank+affair.recap+today.polit
+morn.agenda
+appl+amazon+anoth+agenda+can+dont+health+game+facebook
+today.polit+today+read+vote+uber+trade+thanksgiv+seek+read+war+what+want
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
## LB Best 2
# pred1 = read.csv("pre455.csv")
# pre1 = pred1[,2]
# pred2 = read.csv("pre451.csv")
# pre2 = pred2[,2] 


predfinal = (predrf2+predgbmtr+prednne)/3
summary(predfinal)
MySubmission = data.frame(UniqueID = ID, Probability1 = predfinal)
write.csv(MySubmission, "SubmissionFINAL!.csv", row.names=FALSE)

