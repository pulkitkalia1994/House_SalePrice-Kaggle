train<-read.csv("C:\\R-Programming\\house rate prediction\\train.csv")
test<-read.csv("C:\\R-Programming\\house rate prediction\\test.csv")

##Making NAs in some variables as missing
library(dplyr)
train$Alley<-as.character(train$Alley)
train$hasAlley<-ifelse(is.na(train$Alley),0,1)
train$hasAlley<-as.factor(train$hasAlley)
train$Alley[is.na(train$Alley)]<-"NoAlley"
train$Alley<-as.factor(train$Alley)
levels(train$Alley)<-c("NoAlley","Grvl","Pave")

train$BsmtCond<-as.character(train$BsmtCond)
train$hasBsmt<-ifelse(is.na(train$BsmtCond),0,1)
train$hasBsmt<-as.factor(train$hasBsmt)
train$BsmtCond[is.na(train$BsmtCond)]<-"NoBsmt"
train$BsmtCond<-as.factor(train$BsmtCond)
levels(train$BsmtCond)<-c("NoBsmt","Po","Fa","TA","Gd","Ex")

train$BsmtQual<-as.character(train$BsmtQual)
train$BsmtQual[is.na(train$BsmtQual)]<-"NoBsmt"
train$BsmtQual<-as.factor(train$BsmtQual)
levels(train$BsmtQual)<-c("NoBsmt","Po","Fa","TA","Gd","Ex")

train$BsmtExposure<-as.character(train$BsmtExposure)
train$BsmtExposure[is.na(train$BsmtExposure)]<-"NoBsmt"
train$BsmtExposure<-as.factor(train$BsmtExposure)
levels(train$BsmtExposure)<-c("NoBsmt","No","Mn","Av","Gd")

train$BsmtFinType1<-as.character(train$BsmtFinType1)
train$BsmtFinType1[is.na(train$BsmtFinType1)]<-"NoBsmt"
train$BsmtFinType1<-as.factor(train$BsmtFinType1)
levels(train$BsmtFinType1)<-c("NoBsmt","Unf","LwQ","Rec","BLQ","ALQ","GLQ")

train$BsmtFinType2<-as.character(train$BsmtFinType2)
train$BsmtFinType2[is.na(train$BsmtFinType2)]<-"NoBsmt"
train$BsmtFinType2<-as.factor(train$BsmtFinType2)
levels(train$BsmtFinType2)<-c("NoBsmt","Unf","LwQ","Rec","BLQ","ALQ","GLQ")

train$GarageType<-as.character(train$GarageType)
train$hasGarage<-ifelse(is.na(train$GarageType),0,1)
train$hasGarage<-as.factor(train$hasGarage)
train$GarageType[is.na(train$GarageType)]<-"NoGarage"
train$GarageType<-as.factor(train$GarageType)
levels(train$GarageType)<-c("NoGarage","Detchd","CarPort","BuiltIn","Basment","Attchd","2Types")

train$GarageFinish<-as.character(train$GarageFinish)
train$GarageFinish[is.na(train$GarageFinish)]<-"NoGarage"
train$GarageFinish<-as.factor(train$GarageFinish)
levels(train$GarageFinish)<-c("NoGarage","Unf","RFn","Fin")

train$GarageQual<-as.character(train$GarageQual)
train$GarageQual[is.na(train$GarageQual)]<-"NoGarage"
train$GarageQual<-as.factor(train$GarageQual)
levels(train$GarageQual)<-c("NoGarage","Po","Fa","TA","Gd","Ex")

train$GarageCond<-as.character(train$GarageCond)
train$GarageCond[is.na(train$GarageCond)]<-"NoGarage"
train$GarageCond<-as.factor(train$GarageCond)
levels(train$GarageCond)<-c("NoGarage","Po","Fa","TA","Gd","Ex")

train$PoolQC<-as.character(train$PoolQC)
train$hasPool<-ifelse(is.na(train$PoolQC),0,1)
train$hasPool<-as.factor(train$hasPool)
train$PoolQC[is.na(train$PoolQC)]<-"NoPool"
train$PoolQC<-as.factor(train$PoolQC)
levels(train$PoolQC)<-c("NoPool","Fa","TA","Gd","Ex")

train$Fence<-as.character(train$Fence)
train$hasFence<-ifelse(is.na(train$Fence),0,1)
train$hasFence<-as.factor(train$hasFence)
train$Fence[is.na(train$Fence)]<-"NoFence"
train$Fence<-as.factor(train$Fence)
levels(train$Fence)<-c("NoFence","MnWw","GdWo","MnPrv","GdPrv")

train$MiscFeature<-as.character(train$MiscFeature)
train$hasMisc<-ifelse(is.na(train$MiscFeature),0,1)
train$hasMisc<-as.factor(train$hasMisc)
train$MiscFeature[is.na(train$MiscFeature)]<-"NoMisc"
train$MiscFeature<-as.factor(train$MiscFeature)
levels(train$MiscFeature)<-c("NoMisc","TenC","Shed","Othr","Gar2","Elev")

train$MSSubClass<-ifelse(is.na(train$MSSubClass),-1,train$MSSubClass)

train$LotFrontage[is.na(train$LotFrontage)]<--1

train$MasVnrArea[is.na(train$MasVnrArea)]<--1

train$GarageYrBlt[is.na(train$GarageYrBlt)]<--1

train$MasVnrType<-as.character(train$MasVnrType)
train$MasVnrType[is.na(train$MasVnrType)]<-"Missing"
train$MasVnrType<-as.factor(train$MasVnrType)

train$FireplaceQu<-as.character(train$FireplaceQu)
train$hasFireplace<-ifelse(is.na(train$FireplaceQu),0,1)
train$hasFireplace<-as.factor(train$hasFireplace)
train$FireplaceQu[is.na(train$FireplaceQu)]<-"Missing"
train$FireplaceQu<-as.factor(train$FireplaceQu)
levels(train$FireplaceQu)<-c("Missing","TA","Po","Fa","Gd","Ex")

train$Electrical<-as.character(train$Electrical)
train$Electrical[is.na(train$Electrical)]<-"Missing"
train$Electrical<-as.factor(train$Electrical)

train$specialCond1<-ifelse(grepl("^RR",as.character(train$Condition1)),1,0)
train$specialCond1<-as.factor(train$specialCond1)
train$specialCond2<-ifelse(grepl("^Pos",as.character(train$Condition2)),1,0)
train$specialCond2<-as.factor(train$specialCond2)
train$bothConditions<-ifelse(as.numeric(train$specialCond1)+as.numeric(train$specialCond2)==2,1,0)
train$bothConditions<-as.factor(train$bothConditions)
train$specialStyle<-ifelse(as.character(train$HouseStyle) %in% c("2.5Fin","2Story","SLvl"),1,0)
train$specialStyle<-as.factor(train$specialStyle)
train$remodellingDone<-ifelse(train$YearBuilt!=train$YearRemodAdd,1,0)
train$remodellingDone<-as.factor(train$remodellingDone)
train$specialMaterial<-ifelse(as.character(train$Foundation) %in% c("PConc","Wood"),1,0)
train$specialMaterial<-as.factor(train$specialMaterial)
train$Area1<-train$LotArea-train$BsmtFinSF1
train$Area2<-train$LotArea-train$BsmtFinSF2
train$totalFoorArea<-train$X1stFlrSF+train$X2ndFlrSF
train$AvgBedroomPerFlr<-train$Bedroom/train$totalFoorArea
train$AvgRoomsPerLot<-train$TotRmsAbvGrd/train$LotArea
train$garageRatio<-train$GarageArea/train$LotArea
train$poolAreaRatio<-train$PoolArea/train$LotArea
train$X2ndMiscgarage<-ifelse(as.character(train$MiscFeature) %in% c("Gar2"),1,0)
train$X2ndMiscgarage<-as.factor(train$X2ndMiscgarage)
train$houseAge<-train$YrSold-train$YearBuilt

complete<-complete.cases(train)
complete<-subset(train,complete==TRUE)
library(Boruta)
boruta.train <- suppressMessages(Boruta(SalePrice~.-Id, data = complete, doTrace = 2,maxRuns=100))

##Total of 39 columns selected out of 81 original columns
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)

order<-order(boruta.df$meanImp,decreasing=TRUE)
boruta.df<-boruta.df[order,]
print(boruta.df)

totalcols<-getSelectedAttributes(final.boruta, withTentative = F)
trimmedtrain<-train[,names(train) %in% totalcols]
trimmedtrain$SalePrice<-train$SalePrice

library("ggplot2")
library("dplyr")
library("caret")


##Adding these variables as they had good mean importance rating
trimmedtrain$LotShape<-train$LotShape
trimmedtrain$Condition1<-train$Condition1
trimmedtrain$LandSlope<-train$LandSlope
trimmedtrain$LotFrontage<-train$LotFrontage
trimmedtrain$ScreenPorch<-train$ScreenPorch
trimmedtrain$ExterCond<-train$ExterCond
trimmedtrain$BsmtFinType2<-train$BsmtFinType2
trimmedtrain$RoofMatl<-train$RoofMatl
trimmedtrain$SaleType<-train$SaleType
trimmedtrain$Heating<-train$Heating
trimmedtrain$Street<-train$Street
trimmedtrain$LotConfig<-train$LotConfig
trimmedtrain$X3SsnPorch<-train$X3SsnPorch
trimmedtrain$MiscFeature<-train$MiscFeature
trimmedtrain$YrSold<-train$YrSold

library(dplyr)

categoricalData<-trimmedtrain %>% select_if(is.factor)
categoricalData$SalePrice<-trimmedtrain$SalePrice

for(i in names(categoricalData)){
  col.name<-i
  if(i!="SalePrice"){
  title<-paste("SalePrice Vs",col.name)
  explore<-categoricalData %>% group_by_(col.name) %>% summarise(point=median(SalePrice))
  explore<-as.data.frame(explore)
  g<-ggplot(data=explore, aes(x=explore[,1], y=point,fill=get(col.name))) +
    geom_bar(stat="identity") +xlab(col.name)+ylab("salePrice") + labs(title=title)
  suppressMessages(print(g))
  }
}

numericData<-trimmedtrain %>% select_if(is.numeric)
numericData$SalePrice<-trimmedtrain$SalePrice

for(i in names(numericData)){
  col.name<-i
  if(i!="SalePrice"){
    title<-paste("SalePrice Vs",col.name)
    explore<-data.frame(numericData[,i],point=numericData$SalePrice)
    explore<-explore[order(numericData[,i]),]
    g<-ggplot(data=explore, aes(x=explore[,1], y=point)) +
      geom_point() +xlab(col.name)+ylab("salePrice") + labs(title=title)
    suppressMessages(print(g))
  }
}

model<-lm(SalePrice~.,data=trimmedtrain)
##R2 is 0.9167 which is good 

library("xgboost")
library("Matrix")
library(e1071)
library("caret")
d<-trimmedtrain %>% mutate_if(is.factor,as.numeric)

data_variables <- as.matrix(d[,-(ncol(d)-14)])
data_label <- trimmedtrain[,"SalePrice"]
data_matrix <- xgb.DMatrix(data = data_variables, label = data_label)

xgb_params <- list("objective" = "reg:linear",eta=0.04,gamma=0.6,max_depth=10,eval_metric="mae")
xgbcv <- xgb.cv( params = xgb_params, data = data_matrix, nrounds = 300, nfold = 10, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

nround    <- xgbcv$best_iteration # number of XGBoost rounds
cv.nfold  <- 10

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
bst_model <- xgb.train(params = xgb_params,
                       data = data_matrix,
                       nrounds = nround)


##Transforming the test data in the same way
test$Alley<-as.character(test$Alley)
test$hasAlley<-ifelse(is.na(test$Alley),0,1)
test$hasAlley<-as.factor(test$hasAlley)
test$Alley[is.na(test$Alley)]<-"NoAlley"
test$Alley<-as.factor(test$Alley)
levels(test$Alley)<-c("NoAlley","Grvl","Pave")

test$BsmtCond<-as.character(test$BsmtCond)
test$hasBsmt<-ifelse(is.na(test$BsmtCond),0,1)
test$hasBsmt<-as.factor(test$hasBsmt)
test$BsmtCond[is.na(test$BsmtCond)]<-"NoBsmt"
test$BsmtCond<-as.factor(test$BsmtCond)
levels(test$BsmtCond)<-c("NoBsmt","Po","Fa","TA","Gd","Ex")

test$BsmtQual<-as.character(test$BsmtQual)
test$BsmtQual[is.na(test$BsmtQual)]<-"NoBsmt"
test$BsmtQual<-as.factor(test$BsmtQual)
levels(test$BsmtQual)<-c("NoBsmt","Po","Fa","TA","Gd","Ex")

test$BsmtExposure<-as.character(test$BsmtExposure)
test$BsmtExposure[is.na(test$BsmtExposure)]<-"NoBsmt"
test$BsmtExposure<-as.factor(test$BsmtExposure)
levels(test$BsmtExposure)<-c("NoBsmt","No","Mn","Av","Gd")

test$BsmtFinType1<-as.character(test$BsmtFinType1)
test$BsmtFinType1[is.na(test$BsmtFinType1)]<-"NoBsmt"
test$BsmtFinType1<-as.factor(test$BsmtFinType1)
levels(test$BsmtFinType1)<-c("NoBsmt","Unf","LwQ","Rec","BLQ","ALQ","GLQ")

test$BsmtFinType2<-as.character(test$BsmtFinType2)
test$BsmtFinType2[is.na(test$BsmtFinType2)]<-"NoBsmt"
test$BsmtFinType2<-as.factor(test$BsmtFinType2)
levels(test$BsmtFinType2)<-c("NoBsmt","Unf","LwQ","Rec","BLQ","ALQ","GLQ")

test$GarageType<-as.character(test$GarageType)
test$hasGarage<-ifelse(is.na(test$GarageType),0,1)
test$hasGarage<-as.factor(test$hasGarage)
test$GarageType[is.na(test$GarageType)]<-"NoGarage"
test$GarageType<-as.factor(test$GarageType)
levels(test$GarageType)<-c("NoGarage","Detchd","CarPort","BuiltIn","Basment","Attchd","2Types")

test$GarageFinish<-as.character(test$GarageFinish)
test$GarageFinish[is.na(test$GarageFinish)]<-"NoGarage"
test$GarageFinish<-as.factor(test$GarageFinish)
levels(test$GarageFinish)<-c("NoGarage","Unf","RFn","Fin")

test$GarageQual<-as.character(test$GarageQual)
test$GarageQual[is.na(test$GarageQual)]<-"NoGarage"
test$GarageQual<-as.factor(test$GarageQual)
levels(test$GarageQual)<-c("NoGarage","Po","Fa","TA","Gd","Ex")

test$GarageCond<-as.character(test$GarageCond)
test$GarageCond[is.na(test$GarageCond)]<-"NoGarage"
test$GarageCond<-as.factor(test$GarageCond)
levels(test$GarageCond)<-c("NoGarage","Po","Fa","TA","Gd","Ex")

test$PoolQC<-as.character(test$PoolQC)
test$hasPool<-ifelse(is.na(test$PoolQC),0,1)
test$hasPool<-as.factor(test$hasPool)
test$PoolQC[is.na(test$PoolQC)]<-"NoPool"
test$PoolQC<-as.factor(test$PoolQC)
levels(test$PoolQC)<-c("NoPool","Fa","TA","Gd","Ex")

test$Fence<-as.character(test$Fence)
test$hasFence<-ifelse(is.na(test$Fence),0,1)
test$hasFence<-as.factor(test$hasFence)
test$Fence[is.na(test$Fence)]<-"NoFence"
test$Fence<-as.factor(test$Fence)
levels(test$Fence)<-c("NoFence","MnWw","GdWo","MnPrv","GdPrv")

test$MiscFeature<-as.character(test$MiscFeature)
test$hasMisc<-ifelse(is.na(test$MiscFeature),0,1)
test$hasMisc<-as.factor(test$hasMisc)
test$MiscFeature[is.na(test$MiscFeature)]<-"NoMisc"
test$MiscFeature<-as.factor(test$MiscFeature)
levels(test$MiscFeature)<-c("NoMisc","TenC","Shed","Othr","Gar2","Elev")

test$MSSubClass<-ifelse(is.na(test$MSSubClass),-1,test$MSSubClass)

test$LotFrontage[is.na(test$LotFrontage)]<--1

test$MasVnrArea[is.na(test$MasVnrArea)]<--1

test$GarageYrBlt[is.na(test$GarageYrBlt)]<--1

test$MasVnrType<-as.character(test$MasVnrType)
test$MasVnrType[is.na(test$MasVnrType)]<-"Missing"
test$MasVnrType<-as.factor(test$MasVnrType)

test$FireplaceQu<-as.character(test$FireplaceQu)
test$hasFireplace<-ifelse(is.na(test$FireplaceQu),0,1)
test$hasFireplace<-as.factor(test$hasFireplace)
test$FireplaceQu[is.na(test$FireplaceQu)]<-"Missing"
test$FireplaceQu<-as.factor(test$FireplaceQu)
levels(test$FireplaceQu)<-c("Missing","TA","Po","Fa","Gd","Ex")

test$Electrical<-as.character(test$Electrical)
test$Electrical[is.na(test$Electrical)]<-"Missing"
test$Electrical<-as.factor(test$Electrical)

test$specialCond1<-ifelse(grepl("^RR",as.character(test$Condition1)),1,0)
test$specialCond1<-as.factor(test$specialCond1)
test$specialCond2<-ifelse(grepl("^Pos",as.character(test$Condition2)),1,0)
test$specialCond2<-as.factor(test$specialCond2)
test$bothConditions<-ifelse(as.numeric(test$specialCond1)+as.numeric(test$specialCond2)==2,1,0)
test$bothConditions<-as.factor(test$bothConditions)
test$specialStyle<-ifelse(as.character(test$HouseStyle) %in% c("2.5Fin","2Story","SLvl"),1,0)
test$specialStyle<-as.factor(test$specialStyle)
test$remodellingDone<-ifelse(test$YearBuilt!=test$YearRemodAdd,1,0)
test$remodellingDone<-as.factor(test$remodellingDone)
test$specialMaterial<-ifelse(as.character(test$Foundation) %in% c("PConc","Wood"),1,0)
test$specialMaterial<-as.factor(test$specialMaterial)
test$Area1<-test$LotArea-test$BsmtFinSF1
test$Area2<-test$LotArea-test$BsmtFinSF2
test$totalFoorArea<-test$X1stFlrSF+test$X2ndFlrSF
test$AvgBedroomPerFlr<-test$Bedroom/test$totalFoorArea
test$AvgRoomsPerLot<-test$TotRmsAbvGrd/test$LotArea
test$garageRatio<-test$GarageArea/test$LotArea
test$poolAreaRatio<-test$PoolArea/test$LotArea
test$X2ndMiscgarage<-ifelse(as.character(test$MiscFeature) %in% c("Gar2"),1,0)
test$X2ndMiscgarage<-as.factor(test$X2ndMiscgarage)
test$houseAge<-test$YrSold-test$YearBuilt

trimmedtest<-test[,names(test) %in% totalcols]

trimmedtest$LotShape<-test$LotShape
trimmedtest$Condition1<-test$Condition1
trimmedtest$LandSlope<-test$LandSlope
trimmedtest$LotFrontage<-test$LotFrontage
trimmedtest$ScreenPorch<-test$ScreenPorch
trimmedtest$ExterCond<-test$ExterCond
trimmedtest$BsmtFinType2<-test$BsmtFinType2
trimmedtest$RoofMatl<-test$RoofMatl
trimmedtest$SaleType<-test$SaleType
trimmedtest$Heating<-test$Heating
trimmedtest$Street<-test$Street
trimmedtest$LotConfig<-test$LotConfig
trimmedtest$X3SsnPorch<-test$X3SsnPorch
trimmedtest$MiscFeature<-test$MiscFeature
trimmedtest$YrSold<-test$YrSold

data_test<-trimmedtest
data_test<-data_test%>%mutate_if(is.factor,as.numeric)

test_matrix <- xgb.DMatrix(data = as.matrix(data_test))
predictionsXGBoost<-predict(bst_model,newdata=test_matrix)

dfXG<-data.frame(Id=test$Id,SalePrice=predictionsXGBoost)

write.csv(dfXG,"XG.csv",row.names = FALSE)