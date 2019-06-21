
# Syntax
# var_name <- lm(Quality to be predicted ~ Var to use for pred, 
# data=dtrain(df))

# pred_name <- predict(var_name, test data)

df <- read.csv("student-mat.csv", sep = ";")

head(df)

summary(df)

any(is.na(df)) #Checking for NA values

str(df)

library(ggplot2)

#install.packages("ggthemes")
library(ggthemes)

library(dplyr)

# Num only

num.cols <- sapply(df,is.numeric)

#Filter
cor.data <- cor(df[,num.cols])

cor.data

install.packages("corrgram")
install.packages("corrplot")

library(corrgram)
library(corrplot)

corrplot(cor.data, method = 'color')

corrgram(df)

# help("corrgram")

corrgram(df, order = T, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

ggplot(df,aes(x=G3)) + geom_histogram(bins=20, alpha = 0.5, fill = 'blue')

library(caTools)

set.seed(101) 

sample <- sample.split(df$G3, SplitRatio = 0.7) #70% as training data

train <- subset(df,sample == T)

test <- subset(df, sample == F)

model <- lm( G3 ~ ., train)

summary(model)

res <- residuals(model)
class(res)
res <- as.data.frame(res)
ggplot(res,aes(res)) + geom_histogram(fill = 'blue', alpha = 0.5)

# Shows negative values even when not possible on scoring negative on test

G3.predictions <- predict(model, test)

results <- cbind(G3.predictions, test$G3)

colnames(results) <- c("Predicted", "Real")
results <- as.data.frame(results)

results

# Take care of negative values

to_zero <- function(x){
    if(x<0){
        return(0)
    }else{
        return(x)
    }
}

results$Predicted <- sapply(results$Predicted, to_zero)

# Mean Squared Error

mse <- mean((results$Real-results$Predicted)^2)

mse

mse ^ 0.5

SSE <- sum((results$Predicted-results$Real)^2)
SST <- sum((mean(df$G3)-results$Real)^2)
R2 <- 1- SSE/SST
R2
