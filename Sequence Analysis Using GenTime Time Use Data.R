
# Load packages we are going to use.
if (!require("pacman")) install.packages("pacman")
library(pacman)

# load and install (if necessary) required packages for this course

pacman::p_load(TraMineR, TraMineRextras, cluster, rio, plotrix, 
               haven, Hmisc, gganimate, RColorBrewer, colorspace, 
               knitr, kableExtra, reshape2, summarytools, vegan, MCMCpack, 
               corrplot, ade4, cssTools, WeightedCluster, factoextra,
               tidyverse, effects, margins, psych, devtools, 
               broom, nnet, descr, here, magrittr)


## load dta dataset
## remember that in r, it's forward slashes
## unlike read.dta read_dta reads all versions of stata
data<-read_dta("/Users/kamilakolpashnikova//OneDrive - Nexus365/Data files - Copy/TWtimeuse/Taiwan Sequence Files/Taiwan 2004 sequences.dta")

## create id if id is not present in the dataset
data$id <- as.numeric(paste(data$HLDID, data$PERSID, sep = ""))

## specify the names for the activity variables
activities<-c()
for(i in 1:96) {
  activities<-c(activities, paste("act_h", i, sep = ""))
}

activities

# I create an object with intervals' labels. Sequences start at 00:00 AM:

###########
########### Sequence analysis #####
###########

t_intervals_labels <-  c("00:00", "00:15","00:30","00:45",
                         "01:00", "01:15","01:30","01:45",
                         "02:00", "02:15","02:30","02:45",
                         "03:00", "03:15","03:30","03:45",
                         "04:00", "04:15","04:30","04:45",
                         "05:00", "05:15","05:30","05:45",
                         "06:00", "06:15","06:30","06:45",
                         "07:00", "07:15","07:30","07:45",
                         "08:00", "08:15","08:30","08:45",
                         "09:00", "09:15","09:30","09:45",
                         "10:00", "10:15","10:30","10:45",
                         "11:00", "11:15","11:30","11:45",
                         "12:00", "12:15","12:30","12:45",
                         "13:00", "13:15","13:30","13:45",
                         "14:00", "14:15","14:30","14:45",
                         "15:00", "15:15","15:30","15:45",
                         "16:00", "16:15","16:30","16:45",
                         "17:00", "17:15","17:30","17:45",
                         "18:00", "18:15","18:30","18:45",
                         "19:00", "19:15","19:30","19:45",
                         "20:00", "20:15","20:30","20:45",
                         "21:00", "21:15","21:30","21:45",
                         "22:00", "22:15","22:30","22:45",
                         "23:00", "23:15","23:30","23:45")

## Setting a user defined color palette

## let's brew some colours first
## number of colours is the number of states (in the alphabet)
colourCount = 13
getPalette = colorRampPalette(brewer.pal(9, "Set3"))

# Now we can define the sequence object
# 
# subset the data if you need to
# data <- data[which(data$v19==1),]
MyData <- as_tibble(data)


## you want to use the full categories of states:
## (you need to change if you only focus on specific activities)
gentime_seq <- seqdef(MyData,
                        var = activities,
                        cnames = t_intervals_labels,
                        alphabet = c("1", "2", "3", "4", "5",
                                     "6", "7", "8","9", "10",
                                     "11", "12", "13"), ## notice that I don't have 8 ) you might have it check the data
                        labels = c("sleep", "selfcare", 
                                   "eatdrink", "commute",
                                   "paidwork", "educatn", "housework",
                                   "shopserv", "TVradio", "leisure", 
                                   "sportex",
                                   "volorgwk",
                                   "other activity"),
                        cpal = getPalette(colourCount),
                        xtstep = 18, ##step between displayed tick-marks and labels on the time x-axis
                        id = MyData$id)
## if you have weights then add ===>  weights = MyData$Weight)

## check how the sequence looks like
print(gentime_seq[1:5, ], format = "SPS")

#### PLOTTING SEQUENCES ####

#### sequence index plots ####
## tlim used to show the sequences to plot
## now they use idxs 
## the default is 1:10 plotting the first 10 sequences
## if you set it to 0, it plots all
seqiplot(gentime_seq, border = NA, with.legend = "right", legend.prop=0.4)

##also to plot all you can use seqIplot
seqIplot(gentime_seq, border = NA, with.legend = "right", legend.prop=0.4, idxs = 1:4)

##
## tabulate 4 frequent sequences:
## because there are 96 steps there are barely any
## this is more useful for shorter sequences with many commonalities (as in life-course research)
seqtab(gentime_seq, idxs = 1:4)

## Plot of the 10 most frequent sequences
seqplot(gentime_seq, type="f", with.legend = "right", legend.prop=0.4)

##also can plot frequencies using seqfplot
seqfplot(caregiver_seq, border = NA, with.legend = "right", legend.prop=0.4)
##again, frequencies is not very useful for TU seqs
##because very few of them repeat themselves with 96 steps

## transitions from state to state
trate <- seqtrate(gentime_seq)
round(trate, 2)

## state distribution plots (aka tempogram)
## this is an easy way to plot a tempogram 
seqdplot(gentime_seq, border = NA, with.legend = "right", legend.prop=0.4)


#### changing granularity (number of steps in a sequence) ####
## chaning the number of steps
## to the first method = "first", to the last = "last", or most frequent = "mostfreq"
## 4 means every hour instead of 15 min
gentime4_seq <- seqgranularity(gentime_seq,
                                  tspan=4, method="mostfreq")

seqdplot(gentime4_seq, border = NA, with.legend = "right", legend.prop=0.4)

## =====================
#### Modal states sequence ####
## =====================
seqplot(gentime_seq, type="ms")
## same as
seqmsplot(gentime4_seq, with.legend = "right", legend.prop=0.4, main="Modal Sequences")

#transversal enthropy of state distributions
#the number of valid states and the Shannon entropy of the transversal state
#distribution.
seqHtplot(gentime_seq, with.legend = "right", legend.prop=0.4)



#### calculating dissimilarities #####

## let's subset our sequences b/c it's too big and will take a long time otherwise
data4om<-seqdef(MyData[1:2000,],
                var = activities,
                cnames = t_intervals_labels,
                alphabet = c("1", "2", "3", "4", "5",
                             "6", "7", "8","9", "10",
                             "11", "12", "13"), ## notice that I don't have 8 ) you might have it check the data
                labels = c("sleep", "selfcare", 
                           "eatdrink", "commute",
                           "paidwork", "educatn", "housework",
                           "shopserv", "TVradio", "leisure", 
                           "sportex",
                           "volorgwk",
                           "other activity"),
                cpal = getPalette(colourCount),
                xtstep = 18, ##step between displayed tick-marks and labels on the time x-axis
                id = MyData[1:2000,]$id)

# seqdist() = for pairwise dissimilarities
# seqsubm() = to compute own substitution matrix
#"TRATE", the costs are determined from the estimated transition rates
scost <- seqsubm(data4om, method = "TRATE")
round(scost, 3)
## calculated in this way, all are close to 2 anyway (for this dataset) 2 is default

##optimal matching include both substitutions and indels
##The cost minimization is achieved through dynamic programming, the algorithm
##implemented in TraMineR being essentially that of Needleman and Wunsch (1970) with
##standard optimizations.


## if heavy, calculate only the upper part of the matrix by full.matrix = FALSE
## remember that With a
##constant substitution cost of 2 and an indel cost equal to 1, OM is just LCS (longest common subsequence)
## default is that substitution cost is twice the indel cost, and default indel cost is 1


om_gentime <- seqdist(data4om, method = "OM", indel = 1, sm = scost)

#### cluster analysis ####

## let's run cluster analysis on our dissimilarity matrix
clusterward <- agnes(om_gentime, diss = TRUE, method = "ward")

# Convert hclust into a dendrogram and plot
hcd <- as.dendrogram(clusterward)

# Default plot
plot(hcd, type = "rectangle", ylab = "Height")

## Triangle plot
# plot(hcd, type = "triangle", ylab = "Height")

#### testing cluster solution ####

#inspect the splitting steps
ward.tree <- as.seqtree(clusterward, seqdata = data4om, 
                            diss = om_gentime, 
                            ncluster = 6)
seqtreedisplay(ward.tree, type = "d", border = NA, show.depth = TRUE) 

#test cluster solution quality
wardtest <- as.clustrange(clusterward,
                         diss = om_gentime, 
                          ncluster = 6)

#plot the quality criteria
plot(wardtest,  lwd = 4)
plot(wardtest, norm = "zscore", lwd = 4)
plot(wardtest, stat = c("ASW", "HC", "PBC"), norm = "zscore", lwd = 4)


#### cluster solution ####

#cut tree
MyData<-MyData[1:2000,]
c6 <- cutree(clusterward, k = 6)
MyData<-cbind(MyData, c6)

#plot cluster solution
seqdplot(data4om, group = c6, border = NA)

# subset data by cluster
cl1<-(data4om[MyData$c6 ==  "1",])

# plot the selected cluster 

seqdplot(cl1, main = "",
         cex.main = 1.7, 
         with.legend = FALSE, 
         yaxis = FALSE, 
         cpal = getPalette(colourCount), 
         ylab = "",
         border = NA)

#### multinomial regression #####

# Format categorical variables
MyData$c6 <- factor(MyData$c6)

# Set the reference group for c5 to be 1 (workers)
MyData$c6 <- relevel(MyData$c6, ref=1)

# Run the model
model <- multinom(c6~URBAN+SEX+CARER+factor(INCOME), data=MyData, na.action=na.omit)

summary(model)

z <- summary(model)$coefficients/summary(model)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

##Multinomial logit model: relative risk ratios
# Relative risk ratios allow an easier interpretation of the logit coefficients. 
#They are the exponentiatedvalue of the logit coefficients.

multi1.rrr = exp(coef(model))
multi1.rrr

## number of observations in the total model
length(residuals(model)[,5])

