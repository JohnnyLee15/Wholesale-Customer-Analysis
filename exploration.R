library(tidyverse)

### ABOUT DATA
#--------------------------------------------------------------------------------------------------------------------------------
# Data is from a Wholesaler who sold to customers in Portugal
# Each grocery category is the annual spending in monetary units (measure of spending)
# Delicassen = Meats, cheese, and prepared foods (Short-hand for deli)
# Channel = Type of customer where 1 = Hotel/Restaurant/Cafe, and 2 = Retail Customer (ex. grocery store)
# Region = City that bought from the wholesaler, 1 = Lisbon, 2 = Orporto, 3 = Other Customers in Portugal not from Lisbon and Orporto
# Fresh = perishable items like fruits and vegetables
# Grocery = non-perishable items like canned food, crackers
# Detergents_Paper = Cleaning supplies, garbage bags, etc.

data = read.csv("Wholesale_customers_data.csv")
data$Region = factor(data$Region)
data$Channel = factor(data$Channel)


### SUMMARY STATS
#--------------------------------------------------------------------------------------------------------------------------------
cont_data  = data %>% 
  select(-c(Channel, Region))

summary(cont_data)

# Fresh has the highest Median and Mean
# Detergents and Paper has the lowest Median
# Delicassen has the lowest Mean
# All data is right skewed as medians are smaller than means. Hence, we should
# probably use Spearman correlation

### DENSITY PLOTS
#--------------------------------------------------------------------------------------------------------------------------------
for (i in 1:ncol(cont_data)) {
  dplot = cont_data %>% 
    ggplot(aes(x = cont_data[,i])) +
    geom_density(fill = "orange", color = "black")
    var_name = colnames(cont_data)[i]
    labs(title = cat("Density of ", var_name),
         x = var_name)
  print(dplot)
}

# All right skewed

### DENSITIES BY REGIOM
#--------------------------------------------------------------------------------------------------------------------------------
# Lisbon (Region = 1)
lisbon_cont_data = data %>% 
  filter(Region == 1) %>% 
  select(-c(Channel, Region))

for (i in 1:ncol(lisbon_cont_data)) {
  dplot = lisbon_cont_data %>% 
    ggplot(aes(x = lisbon_cont_data[,i])) +
    geom_density(fill = "orange", color = "black") +
    ylab(colnames(lisbon_cont_data[i]))
  print(dplot)
}
# All right skewed

# Orporto (Region = 2)
oporto_cont_data = data %>% 
  filter(Region == 2) %>% 
  select(-c(Channel, Region))

for (i in 1:ncol(oporto_cont_data)) {
  dplot = oporto_cont_data %>% 
    ggplot(aes(x = oporto_cont_data[,i])) +
    geom_density(fill = "orange", color = "black") +
    ylab(colnames(oporto_cont_data[i]))
  print(dplot)
}
# All right skewed

# Other Regions (Region = 3)
other_cont_data = data %>% 
  filter(Region == 2) %>% 
  select(-c(Channel, Region))

for (i in 1:ncol(other_cont_data)) {
  dplot = other_cont_data %>% 
    ggplot(aes(x = other_cont_data[,i])) +
    geom_density(fill = "orange", color = "black") +
    ylab(colnames(horeca_cont_data[i]))
  print(dplot)
}
# All right skewed

# All continuous variables are right skewed which seems to mean there are
# a small number of customers buying lots of stock

### DENSITIES BY CHANNEL
#--------------------------------------------------------------------------------------------------------------------------------
# HORECA (Channel = 1)
horeca_cont_data = data %>% 
  filter(Channel == 1) %>% 
  select(-c(Channel, Region))

for (i in 1:ncol(horeca_cont_data)) {
  dplot = horeca_cont_data %>% 
    ggplot(aes(x = horeca_cont_data[,i])) +
    geom_density(fill = "orange", color = "black") +
    ylab(colnames(horeca_cont_data[i]))
  print(dplot)
}
# All right skewed

# Retail (Channel = 2)
retail_cont_data = data %>% 
  filter(Channel == 2) %>% 
  select(-c(Channel, Region))

for (i in 1:ncol(retail_cont_data)) {
  dplot = retail_cont_data %>% 
    ggplot(aes(x = retail_cont_data[,i])) +
    geom_density(fill = "orange", color = "black")  +
    ylab(colnames(retail_cont_data[i]))
  print(dplot)
}
# All right skewed

# All distributions have been right skewed so far, meaning there are a few
# companies buying lots of stock, with most buying a smaller amount

### CORRELATIONS
#--------------------------------------------------------------------------------------------------------------------------------
cor(cont_data, method = "spearman")

# Grocery and Detergents have the highest correlation at 0.80
# Frozen and Detergents have the most negative correlation at -0.21

# Correlations worth looking at:

# Grocery and Detergent
# Milk and Grocery
# Milk and Detergent

# Could make some regression model between these variables
### BOXPLOTS
#--------------------------------------------------------------------------------------------------------------------------------
# By Region
# Start at 3 as 1 and 2 are channel and region respectively
for (i in 3:ncol(data)) {
  bplot = data %>% 
    ggplot(aes(x = Region, y = data[,i])) +
    geom_boxplot(aes(fill = Region)) +
    geom_jitter(height = 0, width = 0.25)  +
    ylab(colnames(data[i]))
  
  print(bplot)
}

# Outliers are making it difficult to see the plots, Lets resize the plots

for (i in 3:ncol(data)) {
  q3 = quantile(data[,i], 0.75)
  bplot = data %>% 
    ggplot(aes(x = Region, y = data[,i])) +
    geom_boxplot(aes(fill = Region), outlier.shape = NA) +
    geom_jitter(height = 0, width = 0.25) +
    ylim(0, 1.6*q3) +
    ylab(colnames(data[i]))
  
  print(bplot)
}

# It appears medians are quite similar for all groups. There might be a difference
# in medians for the grocery and mil variables

# By Channel
for (i in 3:ncol(data)) {
  q3 = quantile(data[,i], 0.75)
  bplot = data %>% 
    ggplot(aes(x = Channel, y = data[,i])) +
    geom_boxplot(aes(fill = Channel), outlier.shape = NA) +
    geom_jitter(height = 0, width = 0.25) +
    ylim(0, 1.6*q3) +
    ylab(colnames(data[i]))
  
  print(bplot)
}

# There appears to be a large difference in medians between milk, grocery,
# and detergents

# By Channel Per Region

# Lisbon
lisbon = data %>% 
  filter(Region == 1)

for (i in 3:ncol(lisbon)) {
  q3 = quantile(lisbon[,i], 0.75)
  bplot = lisbon %>% 
    ggplot(aes(x = Channel, y = lisbon[,i])) +
    geom_boxplot(aes(fill = Channel), outlier.shape = NA) +
    geom_jitter(height = 0, width = 0.25) +
    ylim(0, 1.6*q3) +
    ylab(colnames(lisbon[i]))
  
  print(bplot)
}
# Very similar to looking at all regions combined

# Oporto
oporto = data %>% 
  filter(Region == 2)

for (i in 3:ncol(oporto)) {
  q3 = quantile(oporto[,i], 0.75)
  bplot = oporto %>% 
    ggplot(aes(x = Channel, y = oporto[,i])) +
    geom_boxplot(aes(fill = Channel), outlier.shape = NA) +
    geom_jitter(height = 0, width = 0.25) +
    ylim(0, 1.6*q3) +
    ylab(colnames(oporto[i]))
  
  print(bplot)
}
# Very similar to looking at all regions combined

# Other
other = data %>% 
  filter(Region == 2)

for (i in 3:ncol(other)) {
  q3 = quantile(other[,i], 0.75)
  bplot = other %>% 
    ggplot(aes(x = Channel, y = other[,i])) +
    geom_boxplot(aes(fill = Channel), outlier.shape = NA) +
    geom_jitter(height = 0, width = 0.25) +
    ylim(0, 1.6*q3) +
    ylab(colnames(other[i]))
  
  print(bplot)
}
# Very similar to looking at all regions combined

# We can probably make a classification model to figure out which channel ordered
# based off their milk, grocery, or detergent cost

# Lets look at total stock vs regions and channels
data$Total_Stock = rowSums(data[,3:8])

# Channels
q3 = quantile(data$Total_Stock, 0.75)
data %>% 
  ggplot(aes(x = Channel, y = Total_Stock)) +
  geom_boxplot(aes(fill = Channel), outlier.shape = NA) +
  geom_jitter(height = 0, width = 0.25) +
  ylim(0, 1.6*q3) 

# There appears to be a slight difference in medians between the channels

# Regions
q3 = quantile(data$Total_Stock, 0.75)
data %>% 
  ggplot(aes(x = Region, y = Total_Stock)) +
  geom_boxplot(aes(fill = Region), outlier.shape = NA) +
  geom_jitter(height = 0, width = 0.25) +
  ylim(0, 1.6*q3) 

# Pretty much no difference between the regions

### Hypothesis Tests
#--------------------------------------------------------------------------------------------------------------------------------
# Kruskal Tests for differences in median stock price between regions
for (i in 3:ncol(data)) {
  formula = as.formula(paste(colnames(data)[i], "~Region"))
  result = kruskal.test(formula, data = data)
  cat(
    "Data: ", result$data.name, "\n",
    "p-val: ", format.pval(result$p.value, digits = 3), "\n\n"
  )
}
# All Tests came back insignificant

# Wilcox test for differences in median stock price between Channels
for (i in 3:ncol(data)) {
  formula = as.formula(paste(colnames(data)[i], "~Channel"))
  result = wilcox.test(formula, data = data)
  cat(
    "Data: ", result$data.name, "\n",
    "p-val: ", format.pval(result$p.value, digits = 3), "\n\n"
  )
}

# All median stock prices are different between channel 1 and 2. Even though,
# most medians appeared insignificant visually, the tests confirms with high probability
# that the medians are different. So all variables might be great in the classification model.

# Mann-Whitney test for Difference in median total stock between Channels
wilcox.test(Total_Stock~Channel, data = data)
# Significant


# Kruskal-Wallis test for Difference in median total stock between Regions
kruskal.test(Total_Stock~Region, data = data)

# Insignificant

# Chi-squared test for dependence among region and channel
chisq.test(data$Channel, data$Region)

# Insignificant

### SCATTERPLOTS
#--------------------------------------------------------------------------------------------------------------------------------
data %>% 
  ggplot(aes(x = Grocery, y = Detergents_Paper)) +
  geom_point() +
  labs(title = "Detergents Vs. Grocery")

data %>% 
  ggplot(aes(x = Milk, y = Detergents_Paper)) +
  geom_point() +
  labs(title = "Detergents Vs. Milk")

data %>% 
  ggplot(aes(x = Milk, y = Grocery)) +
  geom_point() +
  labs(title = "Grocery Vs. Milk")

# There is definitely a linear relationship between Detergents and Grocery,
# a linear model could be made here

# Channel 1
data %>% 
  filter(Channel == 1) %>% 
  ggplot(aes(x = Grocery, y = Detergents_Paper)) +
  geom_point() +
  labs(title = "Detergents Vs. Grocery")

data %>% 
  filter(Channel == 1) %>% 
  ggplot(aes(x = Milk, y = Detergents_Paper)) +
  geom_point() +
  labs(title = "Detergents Vs. Milk")

data %>% 
  filter(Channel == 1) %>% 
  ggplot(aes(x = Milk, y = Grocery)) +
  geom_point() +
  labs(title = "Grocery Vs. Milk")

# These plots don't look great.

# Channel 2
data %>% 
  filter(Channel == 2) %>% 
  ggplot(aes(x = Grocery, y = Detergents_Paper)) +
  geom_point() +
  labs(title = "Detergents Vs. Grocery")

data %>% 
  filter(Channel == 2) %>% 
  ggplot(aes(x = Milk, y = Detergents_Paper)) +
  geom_point() +
  labs(title = "Detergents Vs. Milk")

data %>% 
  filter(Channel == 2) %>% 
  ggplot(aes(x = Milk, y = Grocery)) +
  geom_point() +
  labs(title = "Grocery Vs. Milk")

# The linear relationship between Detergent and Grocery is very strong for
# channel 1. A model can definitely be made here. Let's check the correlation

data %>% 
  filter(Channel == 2) %>% 
  summarise(corr = cor(Grocery, Detergents_Paper + Milk, method = "spearman"))

# Correlation is 0.86, meaning there is a strong correlation between
# Grocery and Detergent for channel 2 (Retail)


### OVERALL
# Given that Milk, Detergent, and Grocery are are correlated we can definitely
# make regression models with them, but we will need to address the
# multicollinearity. We can use the amount of detergent bought to predict
# the amount of groceries bought, we can also add in other features to see how
# the model preforms, as well as trying different regression techniques,
# using splines, ridge, and lasso regression.

# We can also make a classification model to predict which channel has ordered
# based on the given features, clearly Milk, Grocery, and Detergent will
# be the primary predictors, as the medians of these values are quite different
# between groups. However, given the Mann-Whitney tests are significant for
# all stock prices we can probably use all features in the classification
# model.

# It doesn't appear worth it to make a classification model to figure out
# which region is purchasing from the wholesaler as every single Kruskal-Wallis
# test was insignificant (all median spending was the same, so no predictive power), and the chi-squared test
# between channel and region was also insignificant (no relationship between them, so no predictive power)

### MODEL POSSIBILITIES:
# Predicting the amount spent on groceries from amount spent on other stock, channel, and region using regression.
# Predicting the amount spent on detergent from amount spent on other stock, channel, and region using regression.
# Predicting the amount spent on milk from amount spent on other stock, channel, and region using regression.
# Predicting the channel from amount spent on stock and region using Random Forests.
# POTENTIAL MODELS: 4
# Variables will be removed as models are built and validated







