# 3.1 An example ----

data(crickets, package = "modeldata")
names(crickets)
crickets

ggplot(crickets,
       aes(x = temp, y = rate, color = species, pch = species, lty = species)) +
    geom_point(size = 2) +
    geom_smooth(method = lm, se = F, alpha = 0.5) +
    scale_color_brewer(palette = "Paired") +
    labs(x = "Temp (C)", y = "Chirp Rate (per minute)")

crickets <- crickets %>% 
    mutate(
        species_code = species == "O. niveus"
    )
crickets

interaction_fit <- lm(rate ~ (temp + species)^2, data = crickets)
interaction_fit

plot(interaction_fit)

main_effect_fit <- lm(rate ~ temp + species, data = crickets)
main_effect_fit

anova(main_effect_fit, interaction_fit)

summary(main_effect_fit)

new_values <- data.frame(
    species = "O. exclamationis", 
    temp = 15:20
)

predict(main_effect_fit, new_values)

# 3.2 What does the R formula do? ----
# 3.3 Why tidiness is important for modeling ----

corr_res <- map(mtcars %>% select(-mpg), cor.test, y = mtcars$mpg)

tidy(corr_res[[1]])

corr_res %>% 
    map_dfr(tidy, .id = "predictor") %>% 
    ggplot(aes(x = fct_reorder(predictor, estimate))) +
    geom_point(aes(y = estimate)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
    labs(x = NULL, y = "Correlation with mpg")

# 3.4 Combining base R models and the tidyverse

split_by_species <- crickets %>% 
    group_nest(species)

model_by_species <- split_by_species %>% 
    mutate(model = map(data, ~ lm(rate ~ temp, data = .x)))

model_by_species %>% 
    mutate(coef = map(model, tidy)) %>% 
    select(species, coef) %>% 
    unnest(cols = c(coef))

# 3.5 The tidymodels Metapackage


