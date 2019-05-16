## First set your working directory
setwd("C:/Users/larun/Desktop/CBW/Intro to R/Data/")

## Next, read in your data "let7-3cancer.rds"
## This data contains all the normalized expression of 
## the let-7 miRNAs from 
## TCGA's HNSC, LUAD, and BLCA data sets 
mirna.3can.df <- readRDS("mirna-3cancer.rds")

## Use the head(), View(), str() or point and click to investigate this data
head(mirna.3can.df)
# View(mirna.3can.df)

## Now use prcomp() to generate PCs based on our nomralized miRNA expression
##  you can center and/or scale the data if you like though it is already 
##  centered and scaled within each cancer
##        Note: you'll need to subset the columns of your dataframe either 
##        within the prcomp function or beforehand

mirna.pcs <- prcomp(mirna.3can.df[,grep("hsa",colnames(mirna.3can.df))]) 

## Check out what's inside your pca object using the str() function 
str(mirna.pcs)

## We can use the "summary" function to investigate the importance 
##  of the 1st 10 PCS based on how much variance they explain
##  You'll need to use the summary() function with mirna.pcs
##    and then, within that object, access the "importance" variable
##    Finally, within the "importance", you only want to look at the first 10 columns

summary(mirna.pcs)$importance[,1:10]

## Let's plot the share of variance explained by each of the first 10 PCs
plot(summary(mirna.pcs)$importance[2,1:10],ylim=c(0,1),type = "l")

## It looks like the first 4 PCs are the most important, 
## let's use the pairs() function to  check out how our data 
## is distributed among the first 4 PCs
## (i.e. are there outliers? are there groups? do our data separate by cancer?)

## pairs plots of our samples' PC coordinates
pairs(mirna.pcs$x[,paste0('PC',1:4)])

## The samples in our PCA plot (surprisingly) seem uniform with the exception of a few outliers
## Re-create the pairs plot of our samples' PC coordinates but with the individual outliers colored 
## One way to do this, you'll need to create a vector of colors where each element represents 
##  the individual in the same position of the color for example: 
y.vec <- c(1,4,6,7,8,2,3)
x.vec <- 1:length(y.vec)
col.vec <- c("red","green","pink","yellow","black","blue","purple")
plot(x = x.vec,y = y.vec,pch = 19,cex = 2,
     col = col.vec)

  ## I'm setting pink to be the color of all my non-outlier points
outliers.col.vec <- rep("pink",nrow(mirna.pcs$x))
outliers.col.vec[mirna.pcs$x[,"PC1"] < -32] <- "red"
outliers.col.vec[mirna.pcs$x[,"PC2"] < -30] <- "red"
outliers.col.vec[mirna.pcs$x[,"PC3"] > 30] <- "red"

  ## and then I plot 
pairs(mirna.pcs$x[,paste0('PC',1:4)],col=outliers.col.vec)

## Another way to do this is to create a factor that identifies the 
##    outliers and build our color vector based on that

  ## creating an outlier factor vector
    ## first create a character vector
outliers.vec <- rep("non-outlier",nrow(mirna.pcs$x))
outliers.vec[mirna.pcs$x[,"PC1"] < -32] <- "outlier"
outliers.vec[mirna.pcs$x[,"PC2"] < -30] <- "outlier"
outliers.vec[mirna.pcs$x[,"PC3"] > 30] <- "outlier"
    ## then create a factor vector
outliers.fac.vec <- factor(outliers.vec,levels = c("non-outlier","outlier"))

    ## and then plot
pairs(mirna.pcs$x[,paste0('PC',1:4)],col=c("pink","red")[outliers.fac.vec])
    ## note that c("pink","red")[outliers.fac.vec] produces a vectors of colors like we made above

## What cancers are these outliers from? 
names(outliers.col.vec) <- mirna.3can.df$cancer
table(names(outliers.col.vec[outliers.col.vec == "red"]))

## Now, let's remove these outliers, refit the PCs and plot the first 
## 4 PCs in a pairs() plot again
##    Note that order is maintained 

  ## First, subset
mirna.3can.df.sub <- mirna.3can.df[which(outliers.col.vec != "red"),]

  ## Next do the pca (recall, you only want to do PCA on the miRNA columns)
mirna.sub.pcs <- prcomp(mirna.3can.df.sub[,grep("hsa",colnames(mirna.3can.df.sub))]) 

  ## Check out the PC importance as you did before
plot(summary(mirna.sub.pcs)$importance[2,1:10],ylim=c(0,1))

  ## Last, plot the first 4 PCs
pairs(mirna.sub.pcs$x[,paste0('PC',1:4)])

## Finally, let's see if our PCs are dividing the cancers or 
##  if they are genuninely homogenous.
##  We can do this using a visual and a statistical investigation 

  ## First let's do a visual inspection
cancer.col.vec <- rep("red",nrow(mirna.3can.df.sub))
cancer.col.vec[mirna.3can.df.sub$cancer == "HNSC"] <- "blue"
cancer.col.vec[mirna.3can.df.sub$cancer == "BLCA"] <- "orange"
cancer.col.vec[mirna.3can.df.sub$cancer == "LUAD"] <- "seagreen"

pairs(mirna.sub.pcs$x[,paste0('PC',1:4)],col = cancer.col.vec)

  ## Next, use kruskal.test() check the first 4 PCs for 
  ## how they differ by cancer 
  ## (try using a loop with the cat function to output what you'd like reported)

for(i in 1:4){
  my.k.test <- kruskal.test(x = mirna.sub.pcs$x[,paste0('PC',i)],g = factor(mirna.3can.df.sub$cancer))  
  cat(paste0("PC",i," p-value: ",my.k.test$p.value,"\n"))
}


### Now that we're satisfied with our individual PCs 
##  we're going to investigate the loadings of our miRNA
## You can do this by using the pairs() function with pcs$rotation provides loadings on the PCs for the miRNAs

## pairs plots of miRNA rotations
pairs(mirna.sub.pcs$rotation[,paste0('PC',1:4)])

## Interesting! It looks like there are is a cluster in the miRNA
## Choose boundaries for your miRNA cluster and create a column in 
## the mirna.3can.df identifying these clusters
mirna.col.vec <- rep("blue",nrow(mirna.sub.pcs$rotation))
mirna.col.vec[mirna.sub.pcs$rotation[,"PC1"] < 0.02 & mirna.sub.pcs$rotation[,"PC2"] > 0.05] <- "red"

pairs(mirna.sub.pcs$rotation[,paste0('PC',1:4)],col = mirna.col.vec)

## CHALLENGE QUESTION
## Now you know your mirna clusters, let's see if membership in a given cluster 
##  confers with a pattern of miRNA differential expression in cancer

  ## First, import the dbDEMC2.0 database
demc.df <- read.table("http://www.picb.ac.cn/dbDEMC/download/miRExpAll.txt",header = TRUE,sep = "\t",as.is = TRUE)
head(demc.df)

  ## What are the unique cancer types being investigated and how many are there? 
    ## hint: use the table() function
table(demc.df$Cancer.Type)

   ## Note that this database has miRNA from bladder cancer, lung cancer, and head and neck cancer
  ##  all of which are in our data. Let's see if our miRNA clusters confer with differential 
  ##  expression in each of these cancers
      ## Try doing this in a loop

my.mirna.col.df <- data.frame(cbind(rownames(mirna.sub.pcs$rotation),mirna.col.vec))
names(my.mirna.col.df) <- c("mirna.name","color")
my.mirna.col.df$mirna.name <- as.character(my.mirna.col.df$mirna.name)
my.mirna.col.df$group.mem <- NA
my.mirna.col.df$group.mem[my.mirna.col.df$color == "blue"] <- "big.group"
my.mirna.col.df$group.mem[my.mirna.col.df$color == "red"] <- "small.group"

my.cancers.vec <- c("bladder cancer","lung cancer","head and neck cancer")

i = 1
for(i in 1:length(my.cancers.vec)){
  demc.sub <- demc.df[demc.df$Cancer.Type == my.cancers.vec[i],]
  demc.sub$mirna.name <- tolower(demc.sub$miRBase.Update.ID)
  demc.sub$mirna.name <- gsub(x = demc.sub$mirna.name,pattern = "-",replacement = "\\.")

  merged.df <- merge(my.mirna.col.df,demc.sub,by = "mirna.name",all.x = TRUE)
  merged.df$in.df <- "no"
  merged.df$in.df[is.na(merged.df$Cancer.Type) == FALSE] <- "yes"

  cat(paste0(toupper(my.cancers.vec[i])," chi-square p-value for \n membership in the database vs. membership in our clusters: \n"))  
  cat(paste0(chisq.test(table(merged.df$group.mem,merged.df$in.df))$p.val,"\n"))
  
}

## It seems like there may be some association with the lung cancer miRNAs
##  investigate it further: what does the table of db membership vs cluster membership look like? 
    ## should be able to use portion of your loop above: 

i = 2 ## lung cancer is the second in our my.cancers.vec vector
demc.sub <- demc.df[demc.df$Cancer.Type == my.cancers.vec[i],]
demc.sub$mirna.name <- tolower(demc.sub$miRBase.Update.ID)
demc.sub$mirna.name <- gsub(x = demc.sub$mirna.name,pattern = "-",replacement = "\\.")

merged.df <- merge(my.mirna.col.df,demc.sub,by = "mirna.name",all.x = TRUE)
merged.df$in.df <- "no"
merged.df$in.df[is.na(merged.df$Cancer.Type) == FALSE] <- "yes"

## double chedck with a fisher's exact test
fisher.test(merged.df$group.mem,merged.df$in.df)

## but is it interesting? 
table(merged.df$group.mem,merged.df$in.df)

## seemed potentially interesting, however it looks like our small group has 
## no miRNAs in the differential expression database for any of our cancers of interest

