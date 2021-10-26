
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



