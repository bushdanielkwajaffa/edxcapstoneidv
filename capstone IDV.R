#University Admission Prediction using Linear Regression, Logistic Regression and RandomForest

#Load source data
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


urlfile <- "https://raw.github.com/bushdanielkwajaffa/edxcapstone2/master/Admission_Predict.csv"
Admission <- read.csv(urlfile)
names(Admission)

# Looking at the summary
summary(Admission)

# Data visualization
#From the plot below we see the effect of research on each of component in relation to chance of admittance
research <- table(Admission$Research)
barplot(research,
        col=rainbow(2),
        legend=rownames(research),
        main="Barplot of Research",
        xlab="Research",
        ylab="Count")

p1 <- ggplot(Admission, aes(Chance.of.Admit, TOEFL.Score)) +geom_point(aes(color=factor(Research), alpha=0.5, size=2.0)) + theme_bw()
p2 <- ggplot(Admission, aes(Chance.of.Admit, GRE.Score)) + geom_point(aes(color=factor(Research), alpha=0.5, size=2.0)) + theme_bw()
p3 <- ggplot(Admission, aes(Chance.of.Admit, CGPA)) + geom_point(aes(color=factor(Research), alpha=0.5, size=2.0)) + theme_bw()
p4 <- ggplot(Admission, aes(Chance.of.Admit, University.Rating)) + geom_point(aes(color=factor(Research), alpha=0.5, size=2.0)) + theme_bw()
p5 <- ggplot(Admission, aes(Chance.of.Admit, SOP)) + geom_point(aes(color=factor(Research), alpha=0.5, size=2.0)) + theme_bw()
p6 <- ggplot(Admission, aes(Chance.of.Admit, LOR)) + geom_point(aes(color=factor(Research), alpha=0.5, size=2.0)) + theme_bw()

grid.arrange(p1,p2,p3,p4,p5,p6)

#studying corrolation
num.cols <- sapply(Admission, is.numeric)
cors <-cor(Admission[,num.cols])
corrplot(cors)

#Train and test set
set.seed(222)
test_index <- createDataPartition(Admission$GRE.Score, times = 1, p= 0.7, list = F)
train_set <- Admission %>% slice(-test_index)
test_set <- Admission %>% slice(test_index)

#Modelling using linear regression
fit_lm <- lm(Chance.of.Admit ~ ., data = train_set)
summary(fit_lm)
y_hat_lm <- predict(fit_lm, test_set)

#calculating root mean square error for linear regression
rmse_lm <- sqrt(mean((y_hat_lm-test_set$Chance.of.Admit)^2))
rmse_lm


#Modelling using logistic regression
fit_glm <- glm(Chance.of.Admit ~ ., data = train_set)
summary(fit_glm)
y_hat_glm <- predict(fit_glm, test_set, type= "response")

#Calculating root mean square error for logistic regression
rmse_glm <- sqrt(mean((y_hat_glm-test_set$Chance.of.Admit)^2))
rmse_glm

#Modelling using randomForest
fit_rf <- randomForest(Chance.of.Admit~., train_set)
y_hat_rf <- predict(fit_rf, test_set)

#Calculating root mean square error for randomForest
rmse_rf <- sqrt(mean(y_hat_rf-test_set$Chance.of.Admit)^2)
rmse_rf

#Looking at the table below, randomForest has the lowest root mean square error making it the best model
rmse <- matrix(c(rmse_lm, rmse_glm, rmse_rf),ncol=1,byrow=TRUE)
colnames(rmse) <- c("RMSE")
rownames(rmse) <- c("rmse_lm","rmse_glm","rmse_rf")
rmse <- as.table(rmse)
rmse
