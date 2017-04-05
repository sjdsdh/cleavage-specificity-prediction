#install.packages('e1071')
library("e1071")
#install.packages('pROC')
library("pROC")
#install.packages('ROCR')
library("ROCR")

#mapping strings to orthogonal binary features
amin_code=diag(1,20,20)

alphabet="ARNDCQEGHILKMFPSTWYV"
v=strsplit(alphabet, "")
v=v[[1]]

mapp= function(data){
  
  data_trans=rep(NA,8*20)  
  
  for (n in 1:length(data)){
    
    current_sample=c()
    current_sample_code=as.character(data[n]) 
    current_sample_code=strsplit(current_sample_code, "")
    current_sample_code=current_sample_code[[1]]
    for (j in 1:8){
      index=(1:20)[v==current_sample_code[j]]
      current_sample=c(current_sample, t(as.vector(amin_code[index,]))) 
    }
    data_trans=rbind(data_trans, t(current_sample))
  }
  #remove first row: NA s
  data_trans=data_trans[-1,]
  return (data_trans)
}

##################################main

setwd("C:/Users/sadeh/Google Drive/2015 Fall/ecen689/hw/final project/")

#representing data in orthogonal vector format
dtrain=read.csv("Train-746Data.txt", header=F)
apply(dtrain,2,anyNA)
names(dtrain)=c("oct", "label")
x_train=mapp(dtrain$oct)

#representing data in orthogonal vector format
dtest=read.csv("Test-impensData.txt", header=F)
apply(dtest,2,anyNA)
names(dtest)=c("oct", "label")
x_test=mapp(dtest$oct)

y_train=factor(dtrain$label) 
y_test=factor(dtest$label)





#################
setwd("C:/Users/sadeh/Google Drive/2015 Fall/ecen689/hw/final project/")
dt=read.delim("dist_train_b62_n0.txt", sep = " ", header=F)
#heatmap(as.matrix(dt))


dt=read.delim("b62.txt", sep = " ", header=F)
#heatmap(as.matrix(dt))





######################Linear svm
  
  
  obj <- tune.svm(x_train, y_train, kernel= "linear", 
           cost = 2^(1:9),
           tunecontrol = tune.control(sampling = "cross", cross = 10)
           )

  summary(obj)
  plot(obj)
  
  #probability
  model=svm(x_train,y_train, probability = TRUE,kernel = "linear",  cost = 2)
  # compute probabilites
  pred=predict(model, x_test, probability= TRUE)
  ypredscore=attr(pred, "probabilities")
  pred = prediction(ypredscore[,2], y_test)
  # Plot ROC curve
  perf = performance(pred,  "tpr", "fpr")
  plot(perf,colorize=TRUE, lwd=2, main="Linear cost:2")
  lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

  # Plot precision/recall curve
  perf <- performance(pred, measure = "prec", x.measure = "rec")
  plot(perf, main="precision/recall curve; Linear cost:2")
  # Plot accuracy as function of threshold
  perf <- performance(pred, measure = "acc")
  plot(perf, main="Accuracy; Linear cost:2")
  
  
  require('pROC')
  #PROC
  proc_obj=roc(response= y_test , predictor= ypredscore[,2])
  summary(proc_obj)
  print(auc(proc_obj))


  