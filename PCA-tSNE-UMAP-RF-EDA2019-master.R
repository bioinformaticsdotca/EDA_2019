
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

  ## investigate crabs data (two ways)
head(crabs)
str(crabs)

  ## create two dataframes for the data 
crabs_meas = crabs[,c("FL","RW","CL","CW","BD")]
crabs_details = crabs[,c("sp","sex","index")]

## Set yourself up for easier plotting parameter assigment
crabs_details$color <- ifelse(test = (crabs_details$sp == "B"),
                              yes = "blue",
                              no = "darkorange1")

crabs_details$shape <- ifelse(test = (crabs_details$sex == "M"),
                              yes = 15,
                              no = 17)

####
####  PCA
####

  ## RUN PCA -- the function to run PCA is already included in base R by default
crabs_pca = prcomp(crabs_meas)

  ## INVESTIAGATE OUTPUT
str(crabs_pca)

  ## PLOT RESULT
pairs(crabs_pca$x,
      col = crabs_details$color,
      pch = crabs_details$shape)

####
####  tSNE
####
  ## LOAD tSNE LIBRARY
library(tsne)

  ## RUN TSNE IN CRABS DATA
crabs_tsne = tsne(crabs_meas)

  ## INVESTIGATE OUTPUT
str(crabs_tsne)

  ## PLOT TSNE OUTPUT
plot(x = crabs_tsne[,1],
     y = crabs_tsne[,2],
     xlab = "Dim 1",
     ylab = "Dim 2",
     col = crabs_details$color,
     pch = crabs_details$shape)

####
####  UMAP 
####
  ## LOAD UMAP LIBRARY
library(umap)

  ## GET UMAP OUTPUT
crabs_umap = umap(crabs_meas)
  
  ## INVESTIGATE UMAP OUTPUT
str(crabs_umap)

  ## PLOT UMAP OUTPUT 
plot(x = crabs_umap$layout[,1],
     y = crabs_umap$layout[,2],
     xlab = "Dim 1",
     ylab = "Dim 2",
     col = crabs_details$color,
     pch = crabs_details$shape)


####
####  PLOT PCA, tSNE, and UMAP OUTPUT IN THE SAME PLOT WINDOW
####

  ## TELL R TO CREATE 1 ROW AND 3 COLUMNS OF "SLOTS" FOR R PLOT
par(mfrow=c(1,3))

  ## PLOT YOUR DATA ONE BY ONE 
  
  ## FIRST PCA: 
pairs(crabs_pca$x, ## doesn't work! Why?
      col = crabs_details$color,
      pch = crabs_details$shape,
      main = "PCA")

  ## FIRST alternative PCA: 
plot(crabs_pca$x[,1],
     crabs_pca$x[,2],
     xlab = "Dim 1",
     ylab = "Dim 2",
      col = crabs_details$color,
      pch = crabs_details$shape,
      main = "PCA")

  ## NEXT tSNE: 
plot(x = crabs_tsne[,1],
     y = crabs_tsne[,2],
     xlab = "Dim 1",
     ylab = "Dim 2",
     col = crabs_details$color,
     pch = crabs_details$shape, 
     main = "tSNE")


  ## LAST UMAP: 
plot(x = crabs_umap$layout[,1],
     y = crabs_umap$layout[,2],
     xlab = "Dim 1",
     ylab = "Dim 2",
     col = crabs_details$color,
     pch = crabs_details$shape,
     main = "UMAP")


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
