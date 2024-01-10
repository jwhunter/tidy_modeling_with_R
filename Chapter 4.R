# 4.1 Exploring Features of Homes in Ames
library(janitor)
library(modeldata)
data(ames)
glimpse(ames)

dim(ames)

ames <- clean_names(ames)

table(ames$ms_sub_class)

library(tidymodels)
library(janitor)
tidymodels_prefer()

ggplot(ames, aes(x = sale_price)) +
    geom_histogram(bins = 50, col = 'white') +
    scale_x_continuous(labels = label_number(prefix = "$", scale = 1e-3, suffix = "k"))

# We should log scale the sale price
    # this prevents negative sale prices and 
    # errors in predicting expensive houses will not have undue influence on the model

ggplot(ames, aes(x = sale_price)) +
    geom_histogram(bins = 50, col = 'white') +
    scale_x_log10()

ames <- ames %>% 
    mutate(log_sale_price = log10(sale_price))

# 4.2 Chapter Summary