# Cardiotocography Data Set

############################################################
# Data taken from                                          #
# https://archive.ics.uci.edu/ml/datasets/Cardiotocography #
############################################################
# Project Location:                                        #
# https://github.com/jholland5/CTG_ML                      #
############################################################

# Required Packages:

if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

# This is the URL for the data that will be analyzed.
urlfile <- "https://raw.githubusercontent.com/jholland5/CTG_ML/master/CTG.csv"
  
# Read the data into R and name it liver.
ctg <- read_csv(url(urlfile))

############################# Data PreProcessing ###################

# Any NA's?
any(is.na(ctg))  # Yes, how many rows have NA's?
temp <- na.omit(ctg)
length(ctg$LB) - length(temp$LB)

# There are three rows so we omit these records.

ctg <- na.omit(ctg)

######################## Visualizations/Summaries ##################
# This is what our data looks like
ctg %>% head()
summary(ctg)                # Statistical Summary
############################ Visualize Predictors
ctg %>%
  gather(-NSP, -DS, key = "var", value = "value") %>% 
  ggplot(aes(x = value, color = NSP)) +
  geom_histogram(position = "identity",bins=18,fill = "plum") +
  facet_wrap(~ var, scales = "free") + 
  labs(title = "Distributions of Predictors") + 
  theme(axis.title.x=element_blank(),axis.title.y = element_blank())

########################################
# We remove predictors with very little variability.

ctg <- ctg %>% select(-DP,-DS,-FM,-Nzeros,-Tendency)


################### We scale the columns
ctgs <- ctg[,1:16] # subset columns to be scaled
ctgs <- as.data.frame(scale(ctgs)) # convert to data frame and scale
ctgs$NSP <- ctg$NSP                # add the response variable NSP

#############################################################33

# How does the diagnosis break down in the data?

mean(ctg$NSP == 1) # about 22 % receive a diagnosis of not normal

# We look at scatter-plots to see if the normal ctg's can be 
# separated from the abnormal ones.

#############################ASTV vs MIN###############
splot <- ctg %>% ggplot() 
splot + geom_jitter(aes(ASTV,Min,col = as.factor(NSP)),
                    width = 5,height = 5) + 
  stat_ellipse(aes(ASTV,Min,col=as.factor(NSP)),
               type = "norm",lwd =1.5) +
  scale_color_manual(values = c("plum", "brown")) +
  labs(col = "Diagnosis", title = "ASTV vs Min",
       subtitle = "1 indicates possible abnormality")
########################################################
#############################Width vs ASTV###############
splot <- ctg %>% ggplot() 
splot + geom_jitter(aes(Width,ASTV,col = as.factor(NSP)),
                    width = 5,height = 5) + 
  stat_ellipse(aes(Width,ASTV,col=as.factor(NSP)),
               type = "norm",lwd =1.5) +
  scale_color_manual(values = c("plum", "brown")) +
  labs(col = "Diagnosis", title = "Width vs ASTV",
       subtitle = "1 indicates possible abnormality")
########################################################


# Before modeling, we split the data into test and train sets.
set.seed(1965,sample.kind = "Rounding") # need to be able to reproduce res.
testIndex <- createDataPartition(ctgs$NSP,times = 1,p=.2,list = FALSE)
ctgTrain <- ctgs %>% slice(-testIndex)
ctgTest <- ctgs %>% slice(testIndex)
##################################### 

head(ctgTrain)
############### Logistic model with 2 predictors; baseline.
glm_fit <- ctgTrain %>% 
  glm(NSP ~ ASTV + Min, data =.,family = "binomial")

p_hat_glm <- predict(glm_fit,ctgTest, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > .5,1,0),levels = c("0","1"))
# create a data frame for viewing three metrics
c1 <-confusionMatrix(y_hat_glm, factor(ctgTest$NSP),positive = "0")$byClass[1:2]
c2<- confusionMatrix(y_hat_glm, factor(ctgTest$NSP),positive = "0")$overall["Accuracy"]
df <- data.frame(Result=c(c1[1],c1[2],c2))
df

# Produces 79% accuracy; 95% specificity, 31% sensitivity
####################################### Try all predictors.
glm_fit2 <- ctgTrain %>% 
  glm(NSP ~ ., data =.,family = "binomial")

p_hat_glm <- predict(glm_fit2,ctgTest, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > .5,1,0),levels = c("0","1"))
confusionMatrix(y_hat_glm, factor(ctgTest$NSP),positive = "0")
# create a data frame for viewing three metrics
c1 <-confusionMatrix(y_hat_glm, factor(ctgTest$NSP),positive = "0")$byClass[1:2]
c2<- confusionMatrix(y_hat_glm, factor(ctgTest$NSP),positive = "0")$overall["Accuracy"]
df <- data.frame(Result=c(c1[1],c1[2],c2))
df
# Best model so far: accuracy = .89, spec = .74, sens = .93

########################### KNN



# We try knn with all predictors. We tune the parameter k. 
# We use 5 fold cross validation.
control <- trainControl(method = "cv", number = 5, p = .8)
knn_fit_all <- train(as.factor(NSP) ~ ., 
                     method = "knn",
                     data = ctgTrain,
                     trControl = control,
                     tuneGrid = data.frame(k = seq(1,51,2)))
ggplot(knn_fit_all,highlight = TRUE) + 
  labs(title = "The Best Tune is with k=1")
knn_fit_all$bestTune
y_hat_knn_all <- predict(knn_fit_all,ctgTest)
# create a data frame for viewing three metrics
c1 <-confusionMatrix(y_hat_knn_all, factor(ctgTest$NSP),positive = "0")$byClass[1:2]
c2<- confusionMatrix(y_hat_knn_all, factor(ctgTest$NSP),positive = "0")$overall["Accuracy"]
df <- data.frame(Result=c(c1[1],c1[2],c2))
df
# Accuracy .93, spec = .77, sens = .98


##################################################################
# Table of Predictors
Predictor <- c("LB","AC","FM","UC","DL","DS","DP","ASTV","MSTV",
               "ALTV","MLTV","Width","Min","Max","Nmax","Nzeros",
               "Mode","Mean","Median","Variance","Tendency")
Description <- c("Beats per Min.","Accel. per sec.","Fetal Movement per sec.",
                 "Uterine contrac. per sec.","Light Decel. per sec.",
                 "Severe Decel. per sec.","Prolonged Decel. per sec.",
                 "Abn. Short Term Var.","Mean Short Term Var.",
                 "Abn. Long Term Var.","Mean Long Term Var.",
                 "Hist. Width","Hist. Min","Hist. Max","Hist. Peaks",
                 "Hist. Zeros","Hist. Mode","Hist. Mean","Hist. Median",
                 "Hist. Var.","Hist. Tendency")





