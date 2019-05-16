##
## CBW R EDA WORKSHOP
##
## DIMENSIONALITY REDUCTION AND RANDOM FOREST
##
## LAUREN ERDMAN, MAY 16, 2019
##

##########****************#############
####
####  DIMENSIONALITY REDUCTION
####
##########****************#############

####
####  Load crabs data 
####
  ## "crabs" is from the MASS package
library(MASS)

  ## import crabs data
data(crabs)

  ## investigate crabs data 

  ## create two dataframes for the data
  ## (1) one with numeric data called "crabs_meas"
  ## (2) one with details about the crabs, "sp","sex","index"
  ##  called "crabs_details"

## Set yourself up for easier plotting parameter assigment
## by making added a variable called "color" to the 
## "crabs_details" dataframe with sp = "B" taking the value of "blue" 
## and sp != "B" taking the value of "darkorange1"

## add another variable called "shape" to "crabs_details" 
## taking the value of 15 if sex = "M" and 17 if sex = "F"

####
####  PCA -- find low dimensional representation of data explaining maximum variance
####

  ## RUN PCA -- the function to run PCA (prcomp) is already included in base R by default

  ## INVESTIAGATE OUTPUT using str()

  ## PLOT RESULT using "pairs"

####
####  tSNE -- find low dimensional representation preserving neighborhood distances
####
  ## LOAD tSNE LIBRARY

  ## RUN TSNE IN CRABS DATA

  ## INVESTIGATE OUTPUT using str()

  ## PLOT TSNE OUTPUT

####
####  UMAP -- find a low dimensional representation of the data maintaining topological structure 
#### topology = can be thought of as the "shape" of our data 
###     a cool introduction to manifolds and topology: https://existenceproofblog.wordpress.com/2017/11/03/manifolds-1/
  ## LOAD UMAP LIBRARY

  ## GET UMAP OUTPUT

  ## INVESTIGATE UMAP OUTPUT

  ## PLOT UMAP OUTPUT 


####
####  PLOT PCA, tSNE, and UMAP OUTPUT IN THE SAME PLOT WINDOW
####

  ## TELL R TO CREATE 1 ROW AND 3 COLUMNS OF "SLOTS" FOR R PLOT
  ## using par(mfrow)

  ## PLOT YOUR DATA ONE BY ONE 
  
  ## FIRST, PCA: 

  ## NEXT, tSNE: 

  ## LAST, UMAP: 


##########****************#############
####
####  RANDOM FOREST CLASSIFIERS
####
##########****************#############

# Let's keep using our crabs data from above. 

  ## create "crabs_pred" (predictors) and 
  ## "crabs_lab" (labels) for the crabs data 

  ## predictors are just the numeric measurements from above
crabs_pred = crabs_meas # this can be used directly

  ## labels are 'color:sex'
crabs_lab = paste0(crabs_details$sp,":",crabs_details$sex)

# Now, since we want to build a classifier, 
# we need to generate a held out test set to 
# evaluate how our classifiers perform on unseen data
#    (p-values aren't very relevant for classifiers -- why?)

  ## create sample index
n_samples = length(crabs_lab)
test.per = 0.10 ## 10% test set
n.test = round(n_samples*test.per) ## number of observations = 10% of your data
set.seed(1234) # make reproducible
test.idx = sample(x = 1:n_samples,n.test) ## indices to 

  ## create test set: 
crabs_pred_test = crabs_pred[test.idx,]
crabs_lab_test = crabs_lab[test.idx]

  ## create training set: 
crabs_pred_train = crabs_pred[-test.idx,]
crabs_lab_train = crabs_lab[-test.idx]
str(crabs_pred_train)

  ###
  ### RANDOM FOREST
  ###
library("randomForest")

  ## CREATE YOUR TRAINING AND TEST DATAFRAMES 
rf_train_dat = data.frame(cbind(crabs_lab_train,crabs_pred_train))
rf_test_dat = data.frame((cbind(crabs_lab_test,crabs_pred_test)))

  ## SET YOUR SEED TO MAKE YOUR ANALYSIS REPRODUCIBLE
set.seed(4321)
  ## TRAIN YOUR RANDOM FOREST CLASSIFIER
train_rf = randomForest(crabs_lab_train ~ .,data = rf_train_dat)
str(train_rf)

  ## FIND PREDICTED VALUES
train_rf_pred = train_rf$predicted 
test_rf_pred = predict(train_rf,rf_test_dat)

  ## GET CONFUSION MATRIX
(rf.train.conf.mat <- table(train_rf_pred,crabs_lab_train))
(rf.test.conf.mat <- table(test_rf_pred,crabs_lab_test))

  ## FIND TRAIN AND TEST ACCURACY
# training acc
sum(diag(rf.train.conf.mat))/sum(rf.train.conf.mat)
# test acc
sum(diag(rf.test.conf.mat))/sum(rf.test.conf.mat)

  ## PLOTTING CONFUSION MATRIX
library(reshape)

rf.train.conf.mlt <- melt(rf.train.conf.mat)
rf.test.conf.mlt <- melt(rf.test.conf.mat)

library(ggplot2)

ggplot(rf.train.conf.mlt,aes(y = train_rf_pred, x = crabs_lab_train)) + 
  geom_tile(aes(fill = value)) + 
  xlab("Ground Truth") + 
  ylab("Predicted")

ggplot(rf.test.conf.mlt,aes(y = test_rf_pred, x = crabs_lab_test)) + 
  geom_tile(aes(fill = value)) + 
  xlab("Ground Truth") + 
  ylab("Predicted")


####
####  CHANGE THE RANDOM FOREST CLASSIFIER 
####

## classify species ONLY (group males and females together)
## fewer trees (change the "ntree" argument)
## set maxnodes (try different values e.g. 3, 5, 10)
  ## how does the accuracy change?
## get the importance of variables -- which is most important?
