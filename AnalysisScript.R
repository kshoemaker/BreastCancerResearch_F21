
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
data %>% group_by(diagnosis) %>% ggplot(aes(x = radius_mean, col = diagnosis))  + geom_density()
