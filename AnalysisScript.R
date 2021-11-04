
library(tidyverse)
data <- read.csv("data.csv")

numeric_data <- data[,3:32 ]
summary(numeric_data)

mean_Vec <- apply(numeric_data, 2, mean)

# pairs plot, looking at mean variables only
data %>% filter(diagnosis == "M") %>% select_if(is.numeric) %>% select(contains("mean")) %>% pairs()
data %>% filter(diagnosis == "B") %>% select_if(is.numeric) %>% select(contains("mean")) %>% pairs()
# also look at the se and worst too

# look at how the variables are differently distributed between the two groups
data %>% group_by(diagnosis) %>% ggplot(aes(x = smoothness_mean, col = diagnosis))  + geom_density()


#########
# Logistic Regression

library(ISLR)
library(tidyverse)

data(Default)
glimpse(Default)

## start with one variable
## function: glm (stands for general linear model)
## "formula" y ~ x (dependent/outcome/classes ~ variables)
## family = "binomial" is how we make it a *logistic* regression
## data = our_data_frame
default_lm <- glm(default ~ balance, family = "binomial", data = Default)
summary(default_lm)

# the output of our model: 
  # the probability that that subject will default, P(Default | balance information)
y_hat <- predict(default_lm, data = Default, type = "response")
predicted_class <- vector(length = length(y_hat))
predicted_class[y_hat > 0.5] <- "Yes"
predicted_class[y_hat <= 0.5] <- "No"
predicted_class <- as.factor(predicted_class)

## how do we know 1 is no and 2 is yes:
levels(Default$default)
levels(predicted_class)


x <- cbind(predicted_class, Default$default)
## we sum up the logical/Boolean vector here to see how many times our model was right
sum(predicted_class == Default$default) 

## divide that by the number of observations to see our accuracy
sum(predicted_class == Default$default) / length(predicted_class) * 100

## Note: this is the accuracy on data used to build/create the model. 
## we're going to be interested in how well the model works on NEW data. 


# install.packages("palmerpenguins")
library(palmerpenguins)
penguins

my_penguin <- penguins %>% na.omit() %>% filter(species %in% c("Adelie", "Chinstrap")) 

## with multiple variables, pick a few and then use the accuracy of the model to find a good combination of variables
## you will need to use "as.factor" on the y to change it from a character vector to a factor
    ## Or use strings.as.Factors == T


#### 
my_penguin_model <- glm(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm  , family = "binomial",  data = my_penguin)
summary(my_penguin_model)

## the output of this function is the *probability* of being in the second class, in this case, Chinstrap
y_hat <- predict(my_penguin_model, data = my_penguin, type = "response")


predicted_class <- vector(length = length(y_hat))
predicted_class[y_hat > 0.5] <- "Chinstrap"
predicted_class[y_hat <= 0.5] <- "Adelie"
predicted_class <- as.factor(predicted_class)

## table of my predictions vs the truth
table(predicted_class, my_penguin$species)

## accuracy: num of times the model is right / total number of times 
(145 + 66) / length(predicted_class)





## for your data
## if you want to use ALL the variables: diagnosis ~ . 
  ## the dot is "use all the other variables" --- you'll need to remove the ID variable tho

data$diagnosis <- as.factor(data$diagnosis)
levels(data$diagnosis)



###### Validation SET

# install.packages("palmerpenguins")
library(palmerpenguins)
penguins

my_penguin <- penguins %>% na.omit() %>% filter(species %in% c("Adelie", "Gentoo")) 

## with multiple variables, pick a few and then use the accuracy of the model to find a good combination of variables
## you will need to use "as.factor" on the y to change it from a character vector to a factor
## Or use strings.as.Factors == T


#### 
n <- nrow(my_penguin) # setting the sample size
test <- sample(1:n, 26, replace = F)
## test is the indexes of the validation set

## set the train and test parts of the data
test_pens <- my_penguin[test, ]
train_pens <- my_penguin[-test, ]

validate_penguin_model <- glm(species ~ flipper_length_mm   , family = "binomial",  data = train_pens)
summary(validate_penguin_model)

## the output of this function is the *probability* of being in the second class, in this case, Chinstrap
## NOW we predict on the test data
y_hat <- predict(validate_penguin_model, newdata = test_pens, type = "response")


predicted_class <- vector(length = length(y_hat))
predicted_class[y_hat > 0.5] <- "Gentoo"
predicted_class[y_hat <= 0.5] <- "Adelie"
predicted_class <- as.factor(predicted_class)

## table of my predictions vs the truth
table(predicted_class, test_pens$species)


####### Doing logistic regression with "worse" data #######
###########################################################
set.seed(1389)
## install.packages("mlbench")
data(BreastCancer, package="mlbench")
bc <- BreastCancer %>% na.omit()  # create copy, removing rows with missing data
bc <- bc[, -1] # remove ID column
# this data has some weird variable types, so we want them to be numbers
for (i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}

glimpse(bc)
levels(bc$Class)

bc_model <- glm(Class ~ ., family = 'binomial', data = bc)
summary(bc_model)

y_hat <- predict(bc_model, data = bc, type = "response")

predicted_class <- vector(length = length(y_hat))
predicted_class[y_hat > 0.5] <- "malignant"
predicted_class[y_hat <= 0.5] <- "benign"
predicted_class <- as.factor(predicted_class)

table(predicted_class, bc$Class)
(434 + 228) / nrow(bc)
## 96.93% accuracy 

## validation set

n <- nrow(bc)
test_index <- sample(1:n, floor(n/5), replace = F)  ## floor(n/5) takes n/5 and rounds it down to nearest whole number
test_bc <- bc[test_index, ]
train_bc <- bc[-test_index, ]


validated_bc_model <- glm(Class ~ ., family = 'binomial', data = train_bc) # model with the train data
summary(bc_model)

y_hat <- predict(bc_model, newdata = test_bc, type = "response") ## predict on the 'new' data

predicted_class <- vector(length = length(y_hat))
predicted_class[y_hat > 0.5] <- "malignant"
predicted_class[y_hat <= 0.5] <- "benign"
predicted_class <- as.factor(predicted_class)

table(predicted_class, test_bc$Class)
(83 + 46) / nrow(test_bc)
# 94.8% accuracy
  # this will change if you change the set.seed()




## k-fold CV using the caret package
###########################################################

library(caret)
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model <- train(species ~ flipper_length_mm,
               data = my_penguin,
               trControl = train_control,
               method = "glm",
               family=binomial())

# print cv scores
summary(model)



