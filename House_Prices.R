#House Prices: Advanced Regression Techniques
#Junyan Shao

# Load data
tr <- read.table("W:\\MGMT 473\\train.csv", header=T, sep=",", quote="",
                 colClasses=c("numeric",rep("factor",2),rep("numeric",2),rep("factor",12)
                              ,rep("numeric",4),rep("factor",5),"numeric",rep("factor",7)
                              ,"numeric","factor",rep("numeric",3),rep("factor",4)
                              ,rep("numeric",10),"factor","numeric","factor"
                              ,"numeric",rep("factor",2),"numeric","factor"
                              ,rep("numeric",2),rep("factor",3),rep("numeric",6)
                              ,rep("factor",3),rep("numeric",3),rep("factor",2)
                              ,"numeric")
)

te <- read.table("W:\\MGMT 473\\test.csv", header=T, sep=",", quote="",
                 colClasses=c("numeric",rep("factor",2),rep("numeric",2),rep("factor",12)
                              ,rep("numeric",4),rep("factor",5),"numeric",rep("factor",7)
                              ,"numeric","factor",rep("numeric",3),rep("factor",4)
                              ,rep("numeric",10),"factor","numeric","factor"
                              ,"numeric",rep("factor",2),"numeric","factor"
                              ,rep("numeric",2),rep("factor",3),rep("numeric",6)
                              ,rep("factor",3),rep("numeric",3),rep("factor",2))
)

##1##
DataQualityReportOverall = function(dataSetName) {
  
  n = dim(dataSetName)[[1]]       # Number of observations/records
  p = dim(dataSetName)[[2]]       # Number of attributes
  
  # Data Quality Overall
  CompleteCases = c(1:1)     # Total Complete cases
  IncompleteCases = c(1:1)   # Total Incomplete cases
  CompleteCasePct = c(1:1)   # Percent of Complete cases
  y = data.frame(CompleteCases, IncompleteCases, CompleteCasePct) # Dataframe of attribute's statistics
  
  y[1] = sum(complete.cases(dataSetName)) # Count of complete cases in a data frame
  y[2] = sum(!complete.cases(dataSetName)) # Count of incomplete cases
  y[3] = round(sum(complete.cases(dataSetName))/n,4)*100 # Count of complete cases in a data frame
  
  # Return Data Quality Reports
  return(y)
}
DataQualityReportOverall(tr)

##2##
DataQualityReport = function(dataSetName) {
  
  n = dim(dataSetName)[[1]]       # Number of observations/records
  p = dim(dataSetName)[[2]]       # Number of attributes
  Attributes = names(dataSetName) # Attribute names
  Type = c(1:p)                   # Attributes data type
  NumberMissing = c(1:p)          # Number of missing values
  PercentComplete = c(1:p)        # Percent of missing values
  Min = c(1:p)                    # Min value for numeric attributes
  Avg = c(1:p)                    # Average value for numeric attributes
  Median = c(1:p)                 # Median value for numeric attributes
  Max = c(1:p)                    # Max value for numeric attributes
  NumberLevels = c(1:p)     # Number of Levels for factor attributes
  x = data.frame(Attributes, Type, NumberMissing, PercentComplete, Min, Avg, Median, Max, NumberLevels) # Dataframe of attribute's statistics
  
  # Determine attribute type and calculate relevant statistical measures
  for (i in 1:p) {
    if (is.numeric(dataSetName[,i])) {
      x[i,2] = "numeric"
      x[i,5] = round(min(dataSetName[,i], na.rm=TRUE),2)
      x[i,6] = round(mean(dataSetName[,i], na.rm=TRUE),2)
      x[i,7] = round(median(dataSetName[,i], na.rm=TRUE),2)
      x[i,8] = round(max(dataSetName[,i], na.rm=TRUE),2)
      x[i,9] = "-"
    } else if (is.factor(dataSetName[,i])) {
      x[i,2] = "factor"
      x[i,5] = "-"
      x[i,6] = "-"
      x[i,7] = "-"
      x[i,8] = "-"
      x[i,9] = length(levels(dataSetName[,i]))
    } else if (is.integer(dataSetName[,i])) {
      x[i,2] = "integer"
      x[i,5] = "-"
      x[i,6] = "-"
      x[i,7] = "-"
      x[i,8] = "-"
      x[i,9] = "-"
    } else {
      x[i,2] = "UNKNOWN"
      x[i,5] = "-"
      x[i,6] = "-"
      x[i,7] = "-"
      x[i,8] = "-"
      x[i,9] = "-"
    }
  }
  
  # Determine Number and Percentage of Missing values
  for (i in 1:p) {
    x[i,3] = sum(is.na(dataSetName[i])) #Count
    x[i,4] = round(1-sum(is.na(dataSetName[i]))/n,4)*100 #Percentage
  }  
  
  # Data Quality Overall
  CompleteCases = c(1:1)     # Total Complete cases
  IncompleteCases = c(1:1)   # Total Incomplete cases
  CompleteCasePct = c(1:1)   # Percent of Complete cases
  y = data.frame(CompleteCases, IncompleteCases, CompleteCasePct) # Dataframe of attribute's statistics
  
  y[1] = sum(complete.cases(dataSetName)) # Count of complete cases in a data frame
  y[2] = sum(!complete.cases(dataSetName)) # Count of incomplete cases
  y[3] = round(sum(complete.cases(dataSetName))/n,3)*100 # Count of complete cases in a data frame
  
  # Return Data Quality Reports
  return(x)
}

tr_all<-DataQualityReport(tr)
tr_missing<- subset(tr_all,tr_all$NumberMissing>0)
tr_missing

##3##
tr_80 <- subset(tr_missing,tr_missing$PercentComplete<20)
tr_80
tr$Alley<-NULL
tr$PoolQC<-NULL
tr$Fence<-NULL
tr$MiscFeature<-NULL
tr$Id<-NULL

te_all<-DataQualityReport(te)
te_80<- subset(te_all,te_all$PercentComplete<20)
te_80
te$Alley<-NULL
te$PoolQC<-NULL
te$Fence<-NULL
te$MiscFeature<-NULL

##4##
install.packages("mice")
library(mice)
imputedValues <- mice(data=tr, method="cart", seed=2016, maxint=1, printFlag=F)
tri <- complete(imputedValues, action=1)
imputedValues2 <- mice(data=te, method="cart", seed=2016, maxint=1, printFlag=F)
tei <- complete(imputedValues2, action=1)
setwd("W:\\MGMT 473")
save.image(file="exam2.RData")
load(file="exam2.RData")
dim(tri)
dim(tei)

##5##
install.packages("caret")
library(caret)
dim(tri)
nzv <- nearZeroVar(tri[,2:ncol(tri)], uniqueCut=3) 
tri_filtered <- tri[,2:ncol(tri)][, -nzv]
dim(tri_filtered) 
tri <- cbind(tri$SalePrice, tri_filtered)
names(tri)[1] <- "SalePrice"
rm(tri_filtered)

tei <- tei[,c("Id",names(tri)[2:(ncol(tri)-1)])]

##6##
dim(train1)
dim(train2)
dim(train3)
library(caret)
tri<-tri[,1:(ncol(tri)-1)]
dim(tri)
dummies1 <- dummyVars(~ ., data = tri)
ex1 <- data.frame(predict(dummies1, newdata = tri))
names(ex1) <- gsub("\\.", "", names(ex1))
tri <- ex1
rm(dummies1, ex1)
dummies <- dummyVars(~ ., data = tei)
ex <- data.frame(predict(dummies, newdata = tei))
names(ex) <- gsub("\\.", "", names(ex))
tei <- ex
rm(dummies, ex)
tri <- tri[,c("SalePrice", Reduce(intersect, list(names(tri), names(tei))))]
tei <- tei[,c("Id", Reduce(intersect, list(names(tri), names(tei))))]
dim(tri)
dim(tei)

##7##
descrCor <-  cor(tri[,2:ncol(tri)])  
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999) 
summary(descrCor[upper.tri(descrCor)]) 
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.50)
filteredDescr <- tri[,2:ncol(tri)][,-highlyCorDescr] 
descrCor2 <- cor(filteredDescr) 
summary(descrCor2[upper.tri(descrCor2)])
tri <- cbind(tri$SalePrice, filteredDescr)
names(tri)[1] <- "SalePrice"
rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr, nzv)
tei <- tei[,c("Id", Reduce(intersect, list(names(tri), names(tei))))]
dim(tri)
dim(tei)

##8##
comboInfo <- findLinearCombos(tri)
comboInfo
tri <- tri[, -comboInfo$remove]
dim(tri)
rm(comboInfo)
tei <- tei[,c("Id", Reduce(intersect, list(names(tri), names(tei))))]

##9##
str(tri)
names(tri)
cols <- c("Id","MSSubClass120","MSSubClass160","MSSubClass180","MSSubClass190",
          "MSSubClass20","MSSubClass30","MSSubClass40","MSSubClass45","MSSubClass50",
          "MSSubClass60","MSSubClass70","MSSubClass75","MSSubClass80","MSSubClass85",
          "MSSubClass90","MSZoningCall","MSZoningFV","MSZoningRH","MSZoningRL","MSZoningRM",
          "StreetGrvl","StreetPave","AlleyGrvl","AlleyPave","LotShapeIR1","LotShapeIR2",
          "LotShapeIR3","LotShapeReg","LandContourBnk","LandContourHLS","LandContourLow",
          "LandContourLvl","UtilitiesAllPub","UtilitiesNoSeWa","LotConfigCorner","LotConfigCulDSac",
          "LotConfigFR2","LotConfigFR3","LotConfigInside","LandSlopeGtl","LandSlopeMod",
          "LandSlopeSev","NeighborhoodBlmngtn","NeighborhoodBlueste","NeighborhoodBrDale",
          "NeighborhoodBrkSide","NeighborhoodClearCr","NeighborhoodCollgCr","NeighborhoodCrawfor",
          "NeighborhoodEdwards","NeighborhoodGilbert","NeighborhoodIDOTRR","NeighborhoodMeadowV",
          "NeighborhoodMitchel","NeighborhoodNAmes","NeighborhoodNoRidge","NeighborhoodNPkVill",
          "NeighborhoodNridgHt","NeighborhoodNWAmes","NeighborhoodOldTown","NeighborhoodSawyer",
          "NeighborhoodSawyerW","NeighborhoodSomerst","NeighborhoodStoneBr","NeighborhoodSWISU",
          "NeighborhoodTimber","NeighborhoodVeenker","Condition1Artery","Condition1Feedr",
          "Condition1Norm","Condition1PosA","Condition1PosN","Condition1RRAe","Condition1RRAn",
          "Condition1RRNe","Condition1RRNn","Condition2Artery","Condition2Feedr","Condition2Norm",
          "Condition2PosA","Condition2PosN","Condition2RRAe","Condition2RRAn","Condition2RRNn",
          "BldgType1Fam","BldgType2fmCon","BldgTypeDuplex","BldgTypeTwnhs","BldgTypeTwnhsE",
          "HouseStyle15Fin","HouseStyle15Unf","HouseStyle1Story","HouseStyle25Fin","HouseStyle25Unf",
          "HouseStyle2Story","HouseStyleSFoyer","HouseStyleSLvl","RoofStyleFlat","RoofStyleGable",
          "RoofStyleGambrel","RoofStyleHip","RoofStyleMansard","RoofStyleShed","RoofMatlClyTile",
          "RoofMatlCompShg","RoofMatlMembran","RoofMatlMetal","RoofMatlRoll","RoofMatlTarGrv",
          "RoofMatlWdShake","RoofMatlWdShngl","Exterior1stAsbShng","Exterior1stAsphShn",
          "Exterior1stBrkComm","Exterior1stBrkFace","Exterior1stCBlock","Exterior1stCemntBd",
          "Exterior1stHdBoard","Exterior1stImStucc","Exterior1stMetalSd","Exterior1stPlywood",
          "Exterior1stStone","Exterior1stStucco","Exterior1stVinylSd","Exterior1stWdSdng",
          "Exterior1stWdShing","Exterior2ndAsbShng","Exterior2ndAsphShn","Exterior2ndBrkCmn",
          "Exterior2ndBrkFace","Exterior2ndCBlock","Exterior2ndCmentBd","Exterior2ndHdBoard",
          "Exterior2ndImStucc","Exterior2ndMetalSd","Exterior2ndOther","Exterior2ndPlywood",
          "Exterior2ndStone","Exterior2ndStucco","Exterior2ndVinylSd","Exterior2ndWdSdng",
          "Exterior2ndWdShng","MasVnrTypeBrkCmn","MasVnrTypeBrkFace","MasVnrTypeNone",
          "MasVnrTypeStone","ExterQualEx","ExterQualFa","ExterQualGd","ExterQualTA",
          "ExterCondEx","ExterCondFa","ExterCondGd","ExterCondPo","ExterCondTA","FoundationBrkTil",
          "FoundationCBlock","FoundationPConc","FoundationSlab","FoundationStone","FoundationWood",
          "BsmtQualEx","BsmtQualFa","BsmtQualGd","BsmtQualTA","BsmtCondFa","BsmtCondGd",
          "BsmtCondPo","BsmtCondTA","BsmtExposureAv","BsmtExposureGd","BsmtExposureMn",
          "BsmtExposureNo","BsmtFinType1ALQ","BsmtFinType1BLQ","BsmtFinType1GLQ","BsmtFinType1LwQ",
          "BsmtFinType1Rec","BsmtFinType1Unf","BsmtFinType2ALQ","BsmtFinType2BLQ","BsmtFinType2GLQ",
          "BsmtFinType2LwQ","BsmtFinType2Rec","BsmtFinType2Unf","HeatingGasA","HeatingGasW",
          "HeatingGrav","HeatingOthW","HeatingWall","HeatingQCEx","HeatingQCFa","HeatingQCGd",
          "HeatingQCPo","HeatingQCTA","CentralAirN","CentralAirY","ElectricalFuseA",
          "ElectricalFuseF","ElectricalFuseP","ElectricalMix","ElectricalSBrkr","KitchenQualEx",
          "KitchenQualFa","KitchenQualGd","KitchenQualTA","FunctionalMaj1","FunctionalMaj2",
          "FunctionalMin1","FunctionalMin2","FunctionalMod","FunctionalSev","FunctionalTyp",
          "FireplaceQuEx","FireplaceQuFa","FireplaceQuGd","FireplaceQuPo","FireplaceQuTA",
          "GarageType2Types","GarageTypeAttchd","GarageTypeBasment","GarageTypeBuiltIn",
          "GarageTypeCarPort","GarageTypeDetchd","GarageFinishFin","GarageFinishRFn",
          "GarageFinishUnf","GarageQualEx","GarageQualFa","GarageQualGd","GarageQualPo",
          "GarageQualTA","GarageCondEx","GarageCondFa","GarageCondGd","GarageCondPo",
          "GarageCondTA","PavedDriveN","PavedDriveP","PavedDriveY","PoolQCEx","PoolQCFa",
          "PoolQCGd","FenceGdPrv","FenceGdWo","FenceMnPrv","FenceMnWw","MiscFeatureGar2",
          "MiscFeatureOthr","MiscFeatureShed","MiscFeatureTenC","SaleTypeCOD","SaleTypeCon",
          "SaleTypeConLD","SaleTypeConLI","SaleTypeConLw","SaleTypeCWD","SaleTypeNew",
          "SaleTypeOth","SaleTypeWD","SaleConditionAbnorml","SaleConditionAdjLand",
          "SaleConditionAlloca","SaleConditionFamily","SaleConditionNormal","SaleConditionPartial")
cols <- Reduce(intersect, list(names(tri), cols))
tri[cols] <- lapply(tri[cols], factor)
tei[cols] <- lapply(tei[cols], factor)
dim(tri)
dim(tei)

tri

##10##
library(caret)
preProc<-preProcess(tri[2:ncol(tri)],method=c("range","YeoJohnson"))
trit<- predict(preProc, tri)
preProc1<-preProcess(tei[2:ncol(tei)],method=c("range","YeoJohnson"))
teit<-predict(preProc1, tei)
trit
teit
dim(trit)

##11##
dim(tei)
dim(tri)

##12##
nums <- sapply(trit, is.numeric)
trit_num <- trit[ ,nums]
nums2 <- sapply(teit, is.numeric)
teit_num <- teit[ ,nums2]
trit_num
summary(trit_num)
dim(trit_num)
str(trit_num)
trit

##13##
library(caret)
trainIndex <- createDataPartition(trit_num$SalePrice 
                                  , p = 0.50    
                                  , times = 1   
                                  , list = F    
)
train <- trit_num[ trainIndex,]
test  <- trit_num[-trainIndex,]
train$SalePrice=NULL
test$SalePrice=NULL
library(psych)
principal(train)

##14##

R<-cor(trit_num)
e<-eigen(R)
e

##15##
pc<-princomp(trit_num)
pc
screeplot(pc, npcs = 17,
          type = c("lines")
)

##16##
library(psych)
pca2 <- principal(test
                  , nfactors = 11     
                  , rotate = "none"  
                  , scores = T       
)
pca2$loadings


setwd("C:\\Users\\yan_m\\Desktop\\473data")
save.image(file="exam2_2.RData")
load(file="exam2_2.RData")

##17##
pca_tr <- principal(trit_num[,2:ncol(trit_num)]
                    , nfactors = 11    
                    , rotate = "none"  
                    , scores = T      
)
pca_te <- principal(teit_num[,2:ncol(teit_num)]
                    , nfactors = 11    
                    , rotate = "none" 
                    , scores = T       
)

tr_pcscores <- data.frame(predict(pca_tr, data=trit_num[,2:ncol(trit_num)]))
te_pcscores <- data.frame(predict(pca_te, data=teit_num[,2:ncol(teit_num)]))
tr_pcscores <- tr_pcscores[,1:6]
te_pcscores <- te_pcscores[,1:6]

##18##
library(caret)
preProcValues <- preProcess(tr_pcscores[,1:ncol(tr_pcscores)], method = c("range","YeoJohnson"))
tr_pcscores <- predict(preProcValues, tr_pcscores)
preProcValues <- preProcess(te_pcscores[,1:ncol(te_pcscores)], method = c("range","YeoJohnson"))
te_pcscores <- predict(preProcValues, te_pcscores)

facs <- sapply(trit, is.factor)
trit_facs <- trit[ ,facs]
tr_scoresNfactors <- data.frame(tr_pcscores, trit_facs)
facs2 <- sapply(teit, is.factor)
teit_facs <- teit[ ,facs2]
te_scoresNfactors <- data.frame(te_pcscores, teit_facs)

tr_pcscores <- data.frame(trit$SalePrice, tr_pcscores); names(tr_pcscores)[1] <- "SalePrice"
tr_scoresNfactors <- data.frame(trit$SalePrice, tr_scoresNfactors); names(tr_scoresNfactors)[1] <- "SalePrice"
dim(tr_pcscores)
dim(tr_scoresNfactors)
str(tr_pcscores)
str(tr_scoresNfactors)

##19##
set.seed(1234)
library(caret)
trainIndex1 <- createDataPartition(trit$SalePrice 
                                  , p = 0.80    
                                  , times = 1   
                                  , list = F    
)
train1 <- trit[ trainIndex1,]
test1  <- trit[-trainIndex1,]

trainIndex2 <- createDataPartition(tr_pcscores$SalePrice 
                                   , p = 0.80    
                                   , times = 1   
                                   , list = F    
)
train2 <- tr_pcscores[ trainIndex1,]
test2  <- tr_pcscores[-trainIndex1,]

trainIndex3 <- createDataPartition(tr_scoresNfactors$SalePrice 
                                   , p = 0.80    
                                   , times = 1   
                                   , list = F    
)
train3 <- tr_scoresNfactors[ trainIndex1,]
test3  <-  tr_scoresNfactors[-trainIndex1,]

##20##
m1f <- lm(SalePrice ~ ., data=train1)
summary(m1f)
library(leaps)
mlf <- regsubsets(SalePrice ~ ., data=train1, nbest=1, intercept=T, method='forward')
vars2keep <- data.frame(summary(mlf)$which[which.max(summary(mlf)$adjr2),])
names(vars2keep) <- c("keep")  
head(vars2keep)
library(data.table)
vars2keep <- setDT(vars2keep, keep.rownames=T)[]
vars2keep <- c(vars2keep[which(vars2keep$keep==T & vars2keep$rn!="(Intercept)"),"rn"])[[1]]
vars2keep

modelFormula <- paste("SalePrice ~ LotArea+NeighborhoodNoRidge+NeighborhoodNridgHt+NeighborhoodStoneBr+ExterQualEx+FoundationCBlock+
                      BsmtFinType1GLQ+Fireplaces+GarageArea")
m1f <- lm(modelFormula, data=train1)
summary(m1f)
mlf

# backward selection
library(leaps)
mlb <- regsubsets(SalePrice ~ ., data=train1, nbest=1, intercept=T, method='backward') 
vars2keep <- data.frame(summary(mlb)$which[which.max(summary(mlb)$adjr2),])
names(vars2keep) <- c("keep")  
head(vars2keep)
library(data.table)
vars2keep <- setDT(vars2keep, keep.rownames=T)[]
vars2keep <- c(vars2keep[which(vars2keep$keep==T & vars2keep$rn!="(Intercept)"),"rn"])[[1]]
vars2keep

modelFormula <- paste("SalePrice ~ NeighborhoodNoRidge+NeighborhoodNridgHt+NeighborhoodStoneBr+ExterQualEx+
                      BsmtExposureGd+BedroomAbvGr+Fireplaces+GarageFinishFin+GarageArea") 
m1b <- lm(modelFormula, data=train1)
summary(m1b)
str(train1)

##21##
myDiag <- function(lmfit) {
  colors<-colors() # save vector of colors for custom plots
  library(faraway) # library needed for half-normalplot
  library(lmtest) # library need for bptest
  library(lawstat) # library needed for Brown-Forsythe, and Run Tests
  library(nortest) # library needed for Anderson-Darling Test
  library(MASS) # library needed for Box-Cox transformation
  
  par(mfcol=c(2,3), fg=colors[461],col.lab="black")
  # cooks distance - check for influential points
  cook<-cooks.distance(lmfit) 
  halfnorm(cook,3,ylab="Cooks distance", main="Influences", col="blue") 
  boxplot(cook, col="blue", ylab="Cooks distance", main="Boxplot Cooks Distances")
  # constant variance
  plot(fitted(lmfit),residuals(lmfit),xlab="Fitted",ylab="Residuals", col="red"
       , pch=19,type='p', main="Resid vs. Fitted") 
  abline(h=0) 
  plot(fitted(lmfit),abs(residuals(lmfit)),xlab="Fitted",ylab="Abs(Residuals)"
       , main="Abs(Resid) vs. Fitted", col="red", pch=19)
  # normality
  qqnorm(residuals(lmfit),ylab="Residuals", pch=19, col=colors[610]) 
  qqline(residuals(lmfit)) 
  hist(residuals(lmfit), col=colors[610], main="Historgram of Residuals")
}
source("C:\\Users\\yan_m\\Desktop\\473data\\myDiag.R")
myDiag(m1f)
myDiag(m1b)

# plot predicted vs actual
par(mfrow=c(1,2))
yhat_m1f <- predict(m1f, newdata=train1); plot(train1$SalePrice, yhat_m1f)
yhat_m1b <- predict(m1b, newdata=train1); plot(train1$SalePrice, yhat_m1b)

##22##
m2 <- lm(SalePrice ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data=train2)
myDiag(m2)
par(mfrow=c(1,1))
yhat_m2 <- predict(m2, newdata=train2); plot(train2$SalePrice, yhat_m2)

##23##
str(train3)
m3 <- lm(SalePrice ~. ,data=train3)
myDiag(m3)

##24##
# backward selection##
library(leaps)
m3b <- regsubsets(SalePrice ~ ., data=train3, nbest=1, intercept=T, method='backward') 
vars2keep1 <- data.frame(summary(mlb1)$which[which.max(summary(mlb1)$adjr2),])
names(vars2keep1) <- c("keep")  
head(vars2keep1)
library(data.table)
vars2keep1 <- setDT(vars2keep1, keep.rownames=T)[]
vars2keep1 <- c(vars2keep1[which(vars2keep1$keep==T & vars2keep1$rn!="(Intercept)"),"rn"])[[1]]
vars2keep1

modelFormula1 <- paste("SalePrice ~ PC1+NeighborhoodNAmes+NeighborhoodNoRidge+NeighborhoodNridgHt+
                      NeighborhoodStoneBr+ExterQualEx+BsmtExposureGd+BsmtFinType1GLQ+HeatingQCTA") 
m3b <- lm(modelFormula1, data=train3)
summary(m3b)

myDiag(m3)
par(mfrow=c(1,2))
yhat_m3 <- predict(m3, newdata=train3); plot(train3$SalePrice, yhat_m3)
yhat_m3b <- predict(m3b, newdata=train3); plot(train3$SalePrice, yhat_m3b)

##25##
ctrl <- trainControl(method = "cv", number=3)

##26##
(maxvalue <- summary(trit$SalePrice)["Max."][[1]])

nnet1 <- train(SalePrice/755000 ~ LotArea+NeighborhoodNoRidge+NeighborhoodNridgHt+
               NeighborhoodStoneBr+ExterQualEx+FoundationCBlock+
               BsmtFinType1GLQ+Fireplaces+GarageArea+BsmtExposureGd+BedroomAbvGr+
               GarageFinishFin,
               data = train1,    
               method = "nnet",     
               trControl = ctrl,    
               tuneLength = 15,
               maxit = 100,
               metric = "RMSE"     
)
str(train1)

##27##
nnet1$finalModel$tuneValue

##28##
myGrid <-  expand.grid(size = c(1,2,3,4,5)    
                       , decay = c(0
                                   ,0.002424462
                                   ,0.012424462))  
nnet1b <- train(SalePrice/755000 ~ LotArea+NeighborhoodNoRidge+NeighborhoodNridgHt+
                  NeighborhoodStoneBr+ExterQualEx+FoundationCBlock+
                  BsmtFinType1GLQ+Fireplaces+GarageArea+BsmtExposureGd+BedroomAbvGr+
                  GarageFinishFin,
                data = train1, 
                method = "nnet",     
                trControl = ctrl,    
                tuneGrid = myGrid,
                maxit = 500,
                metric = "RMSE"
)

par(mfrow=c(1,2))
yhat_nn1 <- predict(nnet1, newdata=train1)*maxvalue; plot(train1$SalePrice, yhat_nn1)
yhat_nn1b <- predict(nnet1b, newdata=train1)*maxvalue; plot(train1$SalePrice, yhat_nn1b)

##29##
nnet1b$finalModel$tuneValue

##30##
(maxvalue <- summary(trit$SalePrice)["Max."][[1]])

nnet2 <- train(SalePrice/755000 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6,
               data = train2,    
               method = "nnet",     
               trControl = ctrl,    
               tuneLength = 15,
               maxit = 100,
               metric = "RMSE"     
)
nnet2$finalModel$tuneValue

##31##
myGrid2 <-  expand.grid(size = c(7,8,9,10,11)    
                       , decay = c(0
                                   ,0.001425103
                                   ,0.011425103))  
nnet2b <- train(SalePrice/755000 ~  PC1 + PC2 + PC3 + PC4 + PC5 + PC6,
                data = train2, 
                method = "nnet",     
                trControl = ctrl,    
                tuneGrid = myGrid2,
                maxit = 500,
                metric = "RMSE"
)
par(mfrow=c(1,2))
yhat_nn2 <- predict(nnet2, newdata=train2)*maxvalue; plot(train2$SalePrice, yhat_nn2)
yhat_nn2b <- predict(nnet2b, newdata=train2)*maxvalue; plot(train2$SalePrice, yhat_nn2b)

##32##
(maxvalue <- summary(trit$SalePrice)["Max."][[1]])
nnet3 <- train(SalePrice/755000 ~ PC1+NeighborhoodNAmes+NeighborhoodNoRidge+NeighborhoodNridgHt+
               NeighborhoodStoneBr+ExterQualEx+BsmtExposureGd+BsmtFinType1GLQ+HeatingQCTA,
               data = train3,    
               method = "nnet",     
               trControl = ctrl,    
               tuneLength = 15,
               maxit = 100,
               metric = "RMSE"     
)
nnet3$finalModel$tuneValue

##33##
myGrid3 <-  expand.grid(size = c(-1,0,1,2,3)    
                        , decay = c(0
                                    ,0.007017038
                                    ,0.017017038))  
nnet3b <- train(SalePrice/755000 ~ PC1+NeighborhoodNAmes+NeighborhoodNoRidge+NeighborhoodNridgHt+
                  NeighborhoodStoneBr+ExterQualEx+BsmtExposureGd+BsmtFinType1GLQ+HeatingQCTA,
                data = train3, 
                method = "nnet",     
                trControl = ctrl,    
                tuneGrid = myGrid3,
                maxit = 500,
                metric = "RMSE"
)
par(mfrow=c(1,2))
yhat_nn3 <- predict(nnet3, newdata=train3)*maxvalue; plot(train3$SalePrice, yhat_nn3)
yhat_nn3b <- predict(nnet3b, newdata=train3)*maxvalue; plot(train3$SalePrice, yhat_nn3b)


##34##
install.packages("tree")
library(tree)
tree1 = tree(SalePrice ~ .
             , control = tree.control(nobs=nrow(train1)[[1]]
                                      , mincut = 0
                                      , minsize = 1
                                      , mindev = 0.01)
             , data = train1)
summary(tree1)
plot(tree1); text(tree1, pretty=0) # plot the tree
cv.tree1 = cv.tree(tree1)
par(mfrow=c(1,1))
plot(cv.tree1$size
     , cv.tree1$dev
     , type = 'b')
prunefit<-prune.tree(tree1,best=6)
summary(prunefit)
plot(prunefit); text(prunefit,pretty=0)

##35##
yhat_tree1 <- predict(tree1, newdata=train1); plot(train1$SalePrice, yhat_tree1)

##36##
tree1b <- train(SalePrice ~ .,
                data = train1,     
                method = "treebag",     
                trControl = ctrl,   
                metric = "RMSE"     
)
tree2 <- train(SalePrice ~ .,
               data = train2,     
               method = "treebag",     
               trControl = ctrl,   
               metric = "RMSE"     
)
tree3 <- train(SalePrice ~ .,
               data = train3,     
               method = "treebag",   
               trControl = ctrl,   
               metric = "RMSE"    
)
par(mfrow=c(2,2))
yhat_dt1 <- predict(tree1, newdata=train1); plot(train1$SalePrice, yhat_dt1)
yhat_dt1b <- predict(tree1b, newdata=train1); plot(train1$SalePrice, yhat_dt1b)
yhat_dt2 <- predict(tree2, newdata=train2); plot(train2$SalePrice, yhat_dt2)
yhat_dt3 <- predict(tree3, newdata=train3); plot(train3$SalePrice, yhat_dt3)

##37##
yhat_m1f_te <- predict(m1f, newdata=test1)
yhat_m1b_te <- predict(m1b, newdata=test1)
yhat_m2_te <- predict(m2, newdata=test2)
yhat_m3_te <- predict(m3, newdata=test3)
yhat_m3b_te <- predict(m3b, newdata=test3)

yhat_nn1_te <- predict(nnet1, newdata=test1)*maxvalue
yhat_nn1b_te <- predict(nnet1b, newdata=test1)*maxvalue
yhat_nn2_te <- predict(nnet2, newdata=test2)*maxvalue
yhat_nn2b_te <- predict(nnet2b, newdata=test2)*maxvalue
yhat_nn3_te <- predict(nnet3, newdata=test3)*maxvalue
yhat_nn3b_te <- predict(nnet3b, newdata=test3)*maxvalue

yhat_dt1_te <- predict(tree1, newdata=test1) 
yhat_dt1b_te <- predict(tree1b, newdata=test1)
yhat_dt2_te <- predict(tree2, newdata=test2)
yhat_dt3_te <- predict(tree3, newdata=test3)

results <- matrix(rbind(
  cbind(t(postResample(pred=yhat_m1f, obs=train1$SalePrice)), t(postResample(pred=yhat_m1f_te, obs=test1$SalePrice))),
  cbind(t(postResample(pred=yhat_m1b, obs=train1$SalePrice)), t(postResample(pred=yhat_m1b_te, obs=test1$SalePrice))),
  cbind(t(postResample(pred=yhat_m2, obs=train2$SalePrice)), t(postResample(pred=yhat_m2_te, obs=test2$SalePrice))),
  cbind(t(postResample(pred=yhat_m3, obs=train3$SalePrice)), t(postResample(pred=yhat_m3_te, obs=test3$SalePrice))),
  cbind(t(postResample(pred=yhat_m3b, obs=train3$SalePrice)), t(postResample(pred=yhat_m3b_te, obs=test3$SalePrice))),
  
  cbind(t(postResample(pred=yhat_nn1, obs=train1$SalePrice)), t(postResample(pred=yhat_nn1_te, obs=test1$SalePrice))),
  cbind(t(postResample(pred=yhat_nn1b, obs=train1$SalePrice)), t(postResample(pred=yhat_nn1b_te, obs=test1$SalePrice))),
  cbind(t(postResample(pred=yhat_nn2, obs=train2$SalePrice)), t(postResample(pred=yhat_nn2_te, obs=test2$SalePrice))),
  cbind(t(postResample(pred=yhat_nn2b, obs=train2$SalePrice)), t(postResample(pred=yhat_nn2b_te, obs=test2$SalePrice))),
  cbind(t(postResample(pred=yhat_nn3, obs=train3$SalePrice)), t(postResample(pred=yhat_nn3_te, obs=test3$SalePrice))),
  cbind(t(postResample(pred=yhat_nn3b, obs=train3$SalePrice)), t(postResample(pred=yhat_nn3b_te, obs=test3$SalePrice))),
  
  cbind(t(postResample(pred=yhat_dt1, obs=train1$SalePrice)), t(postResample(pred=yhat_dt1_te, obs=test1$SalePrice))),
  cbind(t(postResample(pred=yhat_dt1b, obs=train1$SalePrice)), t(postResample(pred=yhat_dt1b_te, obs=test1$SalePrice))),
  cbind(t(postResample(pred=yhat_dt2, obs=train2$SalePrice)), t(postResample(pred=yhat_dt2_te, obs=test2$SalePrice))),
  cbind(t(postResample(pred=yhat_dt3, obs=train3$SalePrice)), t(postResample(pred=yhat_dt3_te, obs=test3$SalePrice)))
), nrow=15)
results<-results[,c(1,2,4,5)]
colnames(results) <- c("Train_RMSE", "Train_R2","Test_RMSE", "Test_R2")
rownames(results) <- c("MLR_Forward","MLR_Backward","MLR_PCs","MLR_PCs+Factors",
                       "MLR_Backward_PCs+Factors","NN_ForBackFeatures","NN_ForBackFeatures_Optimized",
                       "NN_PCs","NN_PCs_Optimized","NN_BackFeatures","NN_BackFeatures_Optimized",
                       "Tree_Numerics+Factors","BaggedTree_Numerics+Factors",
                       "BaggedTree_PCs","BaggedTree_PCs+Factors")
results
library(reshape2)
results <- melt(results)
names(results) <- c("Model","Stat","Values")

##38##
library(ggplot2)
# RMSE
p1 <- ggplot(data=results[which(results$Stat=="Train_RMSE" | results$Stat=="Test_RMSE"),]
             , aes(x=Model, y=Values, fill=Stat)) 
p1 <- p1 + geom_bar(stat="identity", color="black", position=position_dodge()) + theme_minimal()
p1 <- p1 + facet_grid(~Model, scale='free_x', drop = TRUE)
p1 <- p1 + scale_fill_manual(values=c('#FF6666','blue'))
p1 <- p1 + xlab(NULL) + theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=.5))

p1 <- p1 + theme(strip.text.x = element_text(size=0, angle=0, colour="white"),
                 strip.text.y = element_text(size=0, face="bold"),
                 strip.background = element_rect(colour="white", fill="white"))
p1 <- p1 + ggtitle("RMSE Performance")
p1

##R2##
p2 <- ggplot(data=results[which(results$Stat=="Train_R2" | results$Stat=="Test_R2"),]
             , aes(x=Model, y=Values, fill=Stat)) 
p2 <- p2 + geom_bar(stat="identity", color="black", position=position_dodge()) + theme_minimal()
p2 <- p2 + facet_grid(~Model, scale='free_x', drop = TRUE)
p2 <- p2 + scale_fill_manual(values=c('#FF6666','blue'))
p2 <- p2 + xlab(NULL) + theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=.5))

p2 <- p2 + theme(strip.text.x = element_text(size=0, angle=0, colour="white"),
                 strip.text.y = element_text(size=0, face="bold"),
                 strip.background = element_rect(colour="white", fill="white"))
p2 <- p2 + ggtitle("R2 Performance")
p2

##38##
yhat_m3_teit <- predict(m3, newdata=teit)
submission<-as.data.frame(yhat_m3_teit)
names(teit)[names(teit) == "LotFrontage"] <- "PC1"
names(teit)[names(teit) == "LotArea"] <- "PC2"
names(teit)[names(teit)== "OverallCond"] <- "PC3"
names(teit)[names(teit) == "BsmtFinSF2"] <- "PC4"
names(teit)[names(teit) == "BsmtFullBath"] <- "PC5"
names(teit)[names(teit) == "BsmtHalfBath"] <- "PC6"
write.csv(submission
            , file=("C:/Users/yan_m/Desktop/473data/submission1.csv") 
)

