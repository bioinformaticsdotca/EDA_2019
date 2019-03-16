## First set your working directory
setwd("C:/Users/larun/Desktop/CBW/Intro to R/Data/")

## Next, read in your data "let7-3cancer.rds"
## This data contains all the normalized expression of 
## the let-7 miRNAs from 
## TCGA's HNSC, LUAD, and BLCA data sets 
mirna.3can.df <- readRDS("mirna-3cancer.rds")

## Use the head(), View(), str() or point and click to investigate this data
head(<WHAT DATAFRAME DO YOU WANT TO LOOK AT?>)
# View(mirna.3can.df)

## Now use prcomp() to generate PCs based on our nomralized miRNA expression
##  you can center and/or scale the data if you like though it is already 
##  centered and scaled within each cancer
##        Note: you'll need to subset the columns of your dataframe either 
##        within the prcomp function or beforehand

mirna.pcs <- prcomp(<WHAT DATAFRAME DO YOU WANT TO DO PCA ON?>) 

## Check out what's inside your pca object using the str() function 
str(<WHICH OBJECT ARE YOU CHECKING OUT?>)

## We can use the "summary" function to investigate the importance 
##  of the 1st 10 PCS based on how much variance they explain
##  You'll need to use the summary() function with mirna.pcs
##    and then, within that object, access the "importance" variable
##    Finally, within the "importance", you only want to look at the first 10 columns

summary(mirna.pcs)$importance[,1:10]

## Let's plot the share of variance explained by each of the first 10 PCs
plot(<WHAT VECTOR ARE YOU PLOTTING?>,ylim=c(0,1))

## It looks like the first 4 PCs are the most important, 
## let's use the pairs() function to  check out how our data 
## is distributed among the first 4 PCs
## (i.e. are there outliers? are there groups? do our data separate by cancer?)

## pairs plots of our samples' PC coordinates
pairs(<ENTER THE DATAFRAME YOU WANT A PAIRS PLOT OF>)

## The samples in our PCA plot (surprisingly) seem uniform with the exception of a few outliers
## Re-create the pairs plot of our samples' PC coordinates but with the individual outliers colored 
## Using one possible approach, you'll need to create a vector of colors where each element represents 
##  the individual in the same position of the color for example: 
y.vec <- c(1,4,6,7,8,2,3)
x.vec <- 1:length(x.vec)
col.vec <- c("red","green","pink","yellow","black","blue","purple")
plot(x = x.vec,y = y.vec,pch = 19,cex = 2,
     col = col.vec)

## I'm setting pink to be te color of all my non-outlier points
outliers.col.vec <- rep("pink",nrow(mirna.pcs$x))
outliers.col.vec[mirna.pcs$x[,"PC1"] < -32] <- "red"
outliers.col.vec[mirna.pcs$x[,"PC2"] < -30] <- "red"
outliers.col.vec[mirna.pcs$x[,"PC3"] > 30] <- "red"

pairs(<WHAT DATAFRAME DO YOU WANT TO PLOT?>,col=<WHAT COLORS ARE YOU USING?>)

## What cancers are these outliers from? 


## Now, let's remove these outliers, refit the PCs and plot the first 
## 4 PCs in a pairs() plot again
##    Note that order is maintained 

  ## First, subset

  ## Next do the pca (recall, you only want to do PCA on the miRNA columns)

  ## Check out the PC importance as you did before

  ## Last, plot the first 4 PCs

## Finally, let's see if our PCs are dividing the cancers or 
##  if they are genuninely homogenous.
##  We can do this using a visual and a statistical investigation 

  ## First let's do a visual inspection (color your PCA by cancer)

  ## Next, use kruskal.test() check the first 4 PCs for 
  ## how they differ by cancer 
  ## (try using a loop with the cat function to output what you'd like reported)



### Now that we're satisfied with our individual PCs 
##  we're going to investigate the loadings of our miRNA
## You can do this by using the pairs() function with pcs$rotation provides loadings on the PCs for the miRNAs

## pairs plots of miRNA rotations


## Interesting! It looks like there are is a cluster in the miRNA
## Choose boundaries for your miRNA cluster and create a column in 
## the mirna.3can.df identifying these clusters


## CHALLENGE QUESTION
## Now you know your mirna clusters, let's see if membership in a given cluster 
##  confers with a pattern of miRNA differential expression in cancer

  ## First, import the dbDEMC2.0 database
demc.df <- read.table("http://www.picb.ac.cn/dbDEMC/download/miRExpAll.txt",header = TRUE,sep = "\t",as.is = TRUE)
head(demc.df)

  ## What are the unique cancer types being investigated and how many are there? 
    ## hint: use the table() function
table(<WHAT VECTOR DO YOU WANT TO GET THE COUNTS OF UNIQUE VALUES FROM?>)

   ## Note that this database has miRNA from bladder cancer, lung cancer, and head and neck cancer
  ##  all of which are in our data. Let's see if our miRNA clusters confer with differential 
  ##  expression in each of these cancers
      ## Try doing this in a loop



## It seems like there may be some association with the lung cancer miRNAs
##  investigate it further: what does the table of db membership vs cluster membership look like? 
    ## should be able to use portion of your loop above: 



## seemed potentially interesting, however it looks like our small group has 
## no miRNAs in the differential expression database for any of our cancers of interest