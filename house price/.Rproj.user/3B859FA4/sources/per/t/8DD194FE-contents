library(tidyverse)
library(tidytext)
library(tidymodels)
library(patchwork)
library(ggwordcloud)

train_raw <- read.csv("data/train.csv")
train_raw <- as_tibble(train_raw)
train_raw

price_plot <- train_raw %>%
  mutate(priceRange = parse_number(priceRange)) %>%
  ggplot(aes(longitude, latitude, z = priceRange)) +
  stat_summary_hex(alpha = 0.8, bins = 50) +
  scale_fill_viridis_c() +
  labs( fill = "mean", title = "Price" )

price_plot

plot_austin <- function(var, title)
  train_raw %>%
  ggplot(aes(longitude, latitude, z = {{var}} )) +
  stat_summary_hex(alpha = 0.8, bins = 50) +
  scale_fill_viridis_c() +
  labs( fill = "mean", title = title )

(price_plot + plot_austin(avgSchoolRating, "School Rating")) /
  (plot_austin(yearBuilt, "Year Built") + plot_austin(log(lotSizeSqFt), "Lot Size")) 


austin_tidy <- train_raw %>%
  mutate(priceRange = parse_number(priceRange) + 100000) %>%
  unnest_tokens(word, description) %>%
  anti_join(get_stopwords())

austin_tidy %>%
  count(word, sort = TRUE)

austin_tidy

top_words <- austin_tidy %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% as.character(1:100000),
         !word == "w") %>%
  slice_max(n, n = 1000) %>%
  pull(word)



word_freq <- austin_tidy %>%
  count(word, priceRange) %>%
  complete(word, priceRange, fill = list(n = 0)) %>%
  group_by(priceRange) %>%
  mutate(price_total = sum(n), 
         propotion = n / price_total) %>%
  ungroup() %>%
  filter(word %in% top_words)

word_freq


word_mods <- word_freq %>%
  nest(data = -word) %>%
  mutate(model = map(data, ~ glm(cbind(n, price_total) ~ priceRange, ., 
                                 family = "binomial")),
         model = map(model, tidy)) %>%
  unnest(model) %>%
  filter(term == "priceRange") %>%
  mutate(p.value = p.adjust(p.value)) %>%
  arrange(estimate)


word_mods


higher_words <- word_mods %>%
  filter(p.value < 0.05) %>%
  slice_max(estimate , n = 20) %>%
  pull(word)

higher_words

lower_words <- word_mods %>%
  filter(p.value < 0.05) %>%
  slice_max(-estimate , n = 20) %>%
  pull(word)

lower_words

word_word_cloud <- word_freq %>%
  filter(word %in% higher_words)  %>%
  filter(priceRange == 750000) %>%
  select(-priceRange, -price_total, -propotion) %>%
  mutate(class = "high_words") %>%
  rbind(
    word_freq %>%
      filter(word %in% lower_words)  %>%
      filter(priceRange == 100000) %>%
      select(-priceRange, -price_total, -propotion) %>%
      mutate(class = "low_words")
  )


word_word_cloud %>%
  mutate(class = factor(class)) %>%
  ggplot(aes(label = word, color = class, size = n)) +
  geom_text_wordcloud_area() +
  facet_wrap(~class) +
  scale_size_area(max_size = 16)


highest_words <- word_mods %>%
  filter(p.value < 0.05) %>%
  slice_max(estimate , n = 7) %>%
  pull(word)

highest_words

word_freq %>%
  filter(word %in% highest_words) %>%
  ggplot(aes(priceRange, propotion, color = word)) +
  geom_line(size = 2.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(vars(word), scales = "free_y") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(x = NULL, y = "proportion of total words used for homes at high price") +
  theme_light()

lowest_words <- word_mods %>%
  filter(p.value < 0.05) %>%
  slice_max(-estimate , n = 8) %>%
  pull(word)


word_freq %>%
  filter(word %in% lowest_words) %>%
  ggplot(aes(priceRange, propotion, color = word)) +
  geom_line(size = 2.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(vars(word), scales = "free_y") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(x = NULL, y = "proportion of total words used for homes at lower price") 


set.seed(123)

austin_split <- train_raw %>%
  select (-city) %>%
  mutate(description = str_to_lower(description)) %>%
  initial_split(strata = priceRange)

austin_split

austin_test <- testing(austin_split)
austin_train <- training(austin_split)

set.seed(234)

austin_folds <- vfold_cv(austin_train, v =5 , strata = priceRange)

higher_pat <- glue::glue_collapse(higher_words, sep = "|")
lower_pat <- glue::glue_collapse(lower_words, sep = "|")


austin_recipe <- recipe(priceRange ~ ., data = austin_train) %>%
  update_role(uid, new_role = "uid") %>%
  step_regex(description, pattern = higher_pat, result = "high_price_words") %>%
  step_regex(description, pattern = lower_pat, result = "low_price_words") %>%
  step_rm(description) %>%
  step_novel(homeType) %>%
  step_unknown(homeType) %>%
  step_other(homeType, threshold = 0.02) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors())

austin_recipe


######################
feature_data <- train_raw %>%
  mutate(priceRange = parse_number(priceRange)) %>%
  select(-uid, -city, -description)


###################
feature_split <- initial_split(feature_data)

feature_train <- training(feature_split)
feature_test <- testing(feature_split)

#recipe
feature_rec <- recipe(priceRange ~ ., data = feature_train) %>%
  step_other(homeType, threshold = 0.01)

feature_prep <- prep(feature_rec)

# Step 3: Train a model
rf_spec <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Combine recipe and model into a workflow
rf_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(feature_rec)

#resamples
feature_folds <- vfold_cv(feature_train)

rf_rs <- fit_resamples(
  rf_wf,
  feature_folds,
  control = control_resamples()
)

rf_rs %>%
  collect_metrics()

library(vip)
vip_spec <- rf_spec %>%
  finalize_model(select_best(rf_rs)) %>%
  set_engine("ranger", importance = "permutation")
  

workflow() %>%
  add_model(vip_spec) %>%
  add_recipe(feature_rec) %>%
  fit(feature_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")



processed_data <- feature_rec %>%
  prep(training = feature_train) %>%
  juice()


# Fit the Random Forest model using the updated preprocessed data
rf_model_fit <- ranger::ranger(
  formula = priceRange ~ ., 
  data = processed_data,  # Use the updated preprocessed data
  num.trees = 500,
  mtry = 4,
  min.node.size = 5,
  seed = 42  # Set seed for reproducibility
)

# Save the trained Random Forest model
saveRDS(rf_model_fit, file = "rf_model.rds")

# Load the saved Random Forest model
model <- readRDS(file = "rf_model.rds")

# Prepare feature data for prediction 
new_data <- data.frame(
  homeType = "Single Family",
  latitude = 30.38009,
  longitude = -97.82995,
  garageSpaces = 2,
  hasSpa = "True",
  yearBuilt = 2000,
  numOfPatioAndPorchFeatures = 1,
  lotSizeSqFt = 5000,
  avgSchoolRating = 9,
  MedianStudentsPerTeacher = 20,
  numOfBathrooms = 1,  
  numOfBedrooms = 1,  
  priceRange = NA
)

# Reorder the columns to match the order in processed_data
new_data <- new_data[, colnames(processed_data)]


# Make predictions using the loaded Random Forest model
predictions <- predict(model, data = new_data)

# Print the predicted loan amount
print(predictions$predictions)

##########
library(tidyverse)
library(tidytext)
library(tidymodels)


train_raw <- read.csv("data/train.csv")
train_raw <- as_tibble(train_raw)
train_raw


austin_tidy <- train_raw %>%
  mutate(priceRange = parse_number(priceRange) + 100000) %>%
  unnest_tokens(word, description) %>%
  anti_join(get_stopwords())

top_words <- austin_tidy %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% as.character(1:5),
         ,!word %>% str_detect("\\d")) %>%
  slice_max(n, n = 100) %>%
  pull(word)


word_freqs <- austin_tidy %>%
  count(word, priceRange) %>%
  complete(word, priceRange, fill = list(n = 0)) %>%
  group_by(priceRange) %>%
  mutate(
    price_total = sum(n),
    proportion = n / price_total
  ) %>%
  ungroup() %>%
  filter(word %in% top_words)


word_freqs

word_mods <- word_freqs %>%
  nest(data = c(priceRange, n, price_total, proportion)) %>%
  mutate(
    model = map(data, ~ glm(cbind(n, price_total) ~ priceRange, ., family = "binomial")),
    model = map(model, tidy)
  ) %>%
  unnest(model) %>%
  filter(term == "priceRange") %>%
  mutate(p.value = p.adjust(p.value)) %>%
  arrange(-estimate)

word_mods

higher_words <- word_mods %>%
  filter(p.value < 0.05) %>%
  slice_max(estimate, n = 30) %>%
  pull(word)

lower_words <- word_mods %>%
  filter(p.value < 0.05) %>%
  slice_max(-estimate, n = 30) %>%
  pull(word)


set.seed(123)

austin_split <- train_raw %>%
  select(-city) %>%
  mutate(description = str_to_lower(description)) %>%
  initial_split(strata = priceRange)
austin_train <- training(austin_split)
austin_test <- testing(austin_split)
austin_metrics <- metric_set(accuracy, roc_auc, mn_log_loss)

set.seed(234)
austin_folds <- vfold_cv(austin_train, v = 5, strata = priceRange)
austin_folds

higher_pat <- glue::glue_collapse(higher_words, sep = "|")
lower_pat <- glue::glue_collapse(lower_words, sep = "|")

austin_rec <-
  recipe(priceRange ~ ., data = austin_train) %>%
  update_role(uid, new_role = "uid") %>%
  step_regex(description, pattern = higher_pat, result = "high_price_words") %>%
  step_regex(description, pattern = lower_pat, result = "low_price_words") %>%
  step_rm(description) %>%
  step_novel(homeType) %>%
  step_unknown(homeType) %>%
  step_other(homeType, threshold = 0.02) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors())

austin_rec



xgb_spec <-
  boost_tree(
    trees = 1000,
    tree_depth = tune(),
    min_n = tune(),
    mtry = tune(),
    sample_size = tune(),
    learn_rate = tune()
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_word_wf <- workflow(austin_rec, xgb_spec)

set.seed(123)
xgb_grid <- grid_max_entropy(
    tree_depth(c(5L, 10L)),
    min_n(c(10L, 40L)),
    mtry(c(5L, 10L)),
    sample_prop(c(0.5, 1.0)),
    learn_rate(c(-2, -1)),
    size = 20
  )

xgb_grid



library(finetune)
doParallel::registerDoParallel()

set.seed(234)
xgb_word_rs <-
  tune_race_anova(
    xgb_word_wf,
    austin_folds,
    grid = xgb_grid,
    metrics = metric_set(mn_log_loss),
    control = control_race(verbose_elim = TRUE)
  )

xgb_word_rs
















