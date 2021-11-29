#-----------------------------------------------------------------
#  Package Loading
library(rpart)
library(rpart.plot)
install.packages("randomForest")
library(randomForest)
##-----------------------------------------------------------------
#loading the database

tele_churn <- read.csv("C:\\users\\tk\\desktop\\DataScience\\DataSets\\Telecom_Churn.csv",stringsAsFactors = TRUE)
tele_churn

#view the database

View(tele_churn)

#seeing full summary of data

summary(tele_churn)

#to check NULL values-Method 1

sum(is.na(tele_churn))
#sum(is.na(tele_churn$gender))

# To check NULL values-Method-2-colSums(is.na(x)) is the function to know NULL values of all columns

colSums(is.na(tele_churn))

#------------------------------------------------------------------------------
#Id has not use in model building.So making it NULL

tele_churn$customerID <- NULL

#---------Data Sampling into train & test--------------------------
#------------------------------------------------------------------
set.seed(1234)

RowNumbers <- sample(x=1:nrow(tele_churn),size=0.7*nrow(tele_churn))

train <- tele_churn[RowNumbers,]
test <- tele_churn[-c(RowNumbers),]

dim(train)
dim(test)

# To check the yes or no in split database train-event rate
 
table(tele_churn$Churn)/nrow(tele_churn)

# No- 54% & Yes- 45%

#----------------------------------------------------------------------
#---------CART model with default parameter settings-----------------

tele_model1 <- rpart(Churn~.,data=train)
tele_model1
summary(tele_model1)

#-----------------------------------------------------------------
windows()
rpart.plot(tele_model1,cex=0.65)#cex stands for character expansion

#---------------------------------------------------------------------
#---------Prediction on testing test---------------------------------
# type attributes converts output in the form of "Yes" and "No"
#---------------------------------------------------------------------
pred_test <- predict(tele_model1,test,type="class")
pred_test

#--------------------------------------------------------------------
# confusion matrix-------------------------------------------------

table(test$Churn,pred_test)
(1737+1151)/(1737+1151+538+275)
#---------------------------------------------------------------------------
#---------Accuracy--------------------------------------------------
sum(diag(table(test$Churn,pred_test)))/nrow(test)#Accuracy is 78%

#---------------------------------------------------------------------
#--------tree growth with split-------------------------------------

tele_model2 <- rpart(Churn~.,data=train,minsplit=100,maxdepth=4) 

#------plot-----------------------------------------------------------

rpart.plot(tele_model2,cex=0.65)    

#----------------------------------------------------------------------
pred_test1 <- predict(tele_model2,test,type="class")
length(pred_test1)

#-confusion matrix-----------------------------------------------------

table(test$Churn,pred_test1)

#---Accuracy---------------------------------------------------
sum(diag(table(test$Churn,pred_test1)))/nrow(test)# Accuray is 73%

#---Overfitting check--------------------------------------------
#--predict on train and check it on test.Accuracy should not vary a lot

#-------prediction on train data

pred_train <- predict(tele_model2,train,type="class")

#--Accuracy on train data
sum(diag(table(train$Churn,pred_train)))/nrow(train)#Accuracy is 73% .
# It's similar to test data.So we can conclude that data is not overfitting

# In case of overfitting in DT, you should try the following:
# * Reduce the tree depth even further
# * Increase the minsplit number


#------------------------------------------------------------------------
#---------------------------------------------------------------------
#-----Random forest with default parameter settings-----------------------
#----------------------------------------------------------------------------
#------------------------------------------------------------------------

#load library
library(randomForest)
set.seed(3456)

rf_model <- randomForest(Churn~.,data=train)
rf_model
summary(rf_model)

#-------------------------------------------------------------------------
# predict on test data---------------------------------------------------

rf_pred <- predict(rf_model,test,type="class")

#------confusion matrix-------------------------------------------------

table(test$Churn,rf_pred)

#-----------------------------------------------------------------------
#-----Accuracy--------------------------------------------------------

sum(diag(table(test$Churn,rf_pred)))/nrow(test)# Accuracy is 88%------

#Variable Importance plot---------------------------------------------

varImp <- varImpPlot(rf_model)
View(varImp)

#------------------------------------------------------------------------

#####################
# RF Model with some tuning parameters
# Tuning parameters: nodesize, ntree, mtry
#####################

# mtry = Total variables to be considered at each node 
# while splitting (Default: sqrt(Total indep variables))

# ntree = Total trees in RF model (Default: 500)

# nodesize = Minimum number of rows in a LEAF NODE. 
# LARGER nodesize will result in SMALLER TREES. (Default: 1)
# So if you have nodesize = 100, and if a intermediate node has 102 rows, 
# then it will NOT be split
# any further, if the individual child nodes are going to have rows 
# lesser than 100 (nodesize), resulting in SMALLER TREE. 
# On the other hand, if you have nodesize = 10, and if a intermediate node 
# has 50 rows, then it will be split
# further, if the individual child nodes are going to have rows 
# more than 10 (nodesize), resulting in a LARGER TREE.

#-------------------------------------------------------------------------

set.seed(2345)
rf_model2 <- randomForest(Churn~.,data=train,ntree=100,nodesize=50,mtry=7)

#-------predict on Test set--------------------------------------------

pred_test_rf <- predict(rf_model2,test,type="class")

#------------confusion matrix--------------------------------------

table(test$Churn,pred_test_rf)

#-----------Accuracy-----------------------------------------------

sum(diag(table(test$Churn,pred_test_rf)))/nrow(test)## Accuracy is 84%

#####################
# Grid Search on Random Forest 
# nodesize, ntree, mtry
#####################

# 
# 
# 
# 
# 
# 



my_nodesize <- c(10,20,30)
my_ntree <- c(50,100)
my_mtry <- c(5,6,7)

Model_Comparison_df <- NULL
model_counter <- 0

set.seed(4567)

for(i in my_nodesize){
  for(j in my_ntree){
    for (k in my_mtry){
      
      #Step 1:Model building,prediction & Accuracy check
      
      temp_model <- randomForest(Churn~., data=train, nodesize = i,ntree = j, mtry = k)
      temp_pred <- predict(temp_model,test,type="class")
      temp_accuracy <- sum(diag(table(test$Churn,temp_pred)))/nrow(test)
      
      #Step 2:storing in a dataframe
      
      temp_row <- cbind.data.frame(i,j,k,temp_accuracy)
      Model_Comparison_df <- rbind.data.frame(Model_Comparison_df,temp_row)
      
      #step 3:(optional) printing on console
      
      model_counter <- model_counter + 1
      cat("Model : ", model_counter, "Nodesize :" ,i,"Ntree :" ,j, "Mtry :" , k, "Accuracy :",temp_accuracy, "\n")
    }
  }
}


colnames(Model_Comparison_df) <- c("Nodesize","Ntree","Mtry","Accuracy")
View(Model_Comparison_df)
