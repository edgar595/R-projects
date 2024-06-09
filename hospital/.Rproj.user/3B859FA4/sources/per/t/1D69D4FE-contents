
library(dplyr)
library(sf)
library(readr)
library(ggplot2)
library(tidyr)


####################
counties2 <- st_read("C:/Users/USER/Downloads/kenyan-counties")

health_data <- read_csv("data/health_data.csv")

# Plot the Kenyan counties
ggplot(counties2) +
  geom_sf() +
  theme_minimal()

# Plot the Turkana county
ggplot() +
  geom_sf(data = turkana) +
  theme_minimal()


# Filter the data for Turkana county
Mandera <- counties2 %>%
  filter(COUNTY == "Mandera")

ggplot() +
  geom_sf(data = Mandera, fill = "green") +
  geom_sf(data = counties2 %>% filter(COUNTY != "Mandera"), fill = "white") +
  theme_minimal()



health_longer <- health_data %>%
  arrange(county) %>%
  pivot_longer(total_dewormed:stunted_child, names_to = "Deficiency", values_to = "Cases")

baringo_diarrhoea <- health_longer %>%
  filter(county == "Nakuru", Deficiency == "diarrhoea_cases") %>%
  summarise(county_cases = mean(Cases))  %>%
  pull(county_cases)

# Create a data frame for Baringo County only
data <- data.frame(
  County = "Baringo",
  Cases = baringo_diarrhoea
)

data

data$Highlight <- ifelse(data$Cases > (data$Cases + 1), "Beyond 2000", "Below 2000")


# Plotting
ggplot(data, aes(x = "", y = Cases, fill = Highlight)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_segment(aes(x = 1, xend = 1, y = data$Cases + 1, yend = 10300), color = "white", size = 65) + # Adjusting size to make it thicker
  labs(title = "Diarrhoea Cases in Baringo County",
       y = "Cases",
       x = NULL) +
  scale_y_continuous(limits = c(0, 10300)) +  # Setting y-axis limits
  theme(legend.position = "none") +
  coord_flip()



highest_diarrhoea <- health_longer %>%
  filter(Deficiency == "underweight_child") %>%
  group_by(county) %>%
  summarise(highest_cases = mean(Cases)) %>%
  arrange(desc(highest_cases)) %>%
  pull(highest_cases)

#######underweight#############
# Calculate mean cases for all counties
mean_cases <- health_longer %>%
  filter(Deficiency == "underweight_child") %>%
  group_by(period) %>%
  summarize(mean_cases = mean(Cases))

# Plotting
p5 <- health_longer %>%
  ggplot(aes(x = period, y = Cases, color = county)) +
  geom_line(data = mean_cases, aes(y = mean_cases), color = "blue") + 
  geom_line(data = filter(health_longer, Deficiency == "underweight_child", county == "Busia"), color = "red") + 
  labs(x = "Period", y = "Cases", title = "Underweight") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("blue","red")) 

###############dewormed###########
mean_cases <- health_longer %>%
  filter(Deficiency == "total_dewormed") %>%
  group_by(period) %>%
  summarize(mean_cases = mean(Cases))

# Plotting
p4 <- health_longer %>%
  ggplot(aes(x = period, y = Cases, color = county)) +
  geom_line(data = mean_cases, aes(y = mean_cases), color = "blue") + 
  geom_line(data = filter(health_longer, Deficiency == "total_dewormed", county == "Busia"), color = "red") + 
  labs(x = "Period", y = "Cases", title = "Deworming") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("blue","red")) 
###########malnutrition#########
mean_cases <- health_longer %>%
  filter(Deficiency == "acute_malnutrition") %>%
  group_by(period) %>%
  summarize(mean_cases = mean(Cases))

# Plotting
p3 <- health_longer %>%
  ggplot(aes(x = period, y = Cases, color = county)) +
  geom_line(data = mean_cases, aes(y = mean_cases), color = "blue") + 
  geom_line(data = filter(health_longer, Deficiency == "acute_malnutrition", county == "Busia"), color = "red") + 
  labs(x = "Period", y = "Cases", title = "Malnutrition") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("blue","red")) 
############################diarrhoea#################
mean_cases <- health_longer %>%
  filter(Deficiency == "diarrhoea_cases") %>%
  group_by(period) %>%
  summarize(mean_cases = mean(Cases))

# Plotting
p1 <- health_longer %>%
  ggplot(aes(x = period, y = Cases, color = county)) +
  geom_line(data = mean_cases, aes(y = mean_cases), color = "blue") + 
  geom_line(data = filter(health_longer, Deficiency == "diarrhoea_cases", county == "Busia"), color = "red") + 
  labs(x = "Period", y = "Cases", title = "diarrhoea") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("blue","red")) 

ggplotly(p1)

##########
mean_cases_stunted <- health_longer %>%
  filter(Deficiency == "stunted_child") %>%
  group_by(period) %>%
  summarize(mean_cases = mean(Cases))

# Plotting
p2 <- health_longer %>%
  ggplot(aes(x = period, y = Cases, color = county)) +
  geom_line(data = mean_cases_stunted, aes(y = mean_cases), color = "blue") + 
  geom_line(data = filter(health_longer, Deficiency == "stunted_child", county == "Busia"), color = "red") + 
  labs(x = "Period", y = "Cases", title = "stunted") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("blue","red")) 

p1 <- ggplotly(p1)
p2 <- ggplotly(p2)
p3 <- ggplotly(p3)
p4 <- ggplotly(p4)
p4 <- ggplotly(p5)

subplot(p2, p1, p3, p4, p5 , nrows = 2) %>%
  layout(title = "Distribution for:diarrhoea, stunted, Malnutrition, Deworm, Underweight")



##############################################
health_longer %>%
  mutate(period = as.character(period))

health_longer %>%
  filter()


health_slopes %>%
  rename(period = term) %>%
  select(county, period ,Deficiency, Cases = estimate) %>%
  mutate(period = case_when(
    str_detect(period, "period") ~ "predicted",
    TRUE ~ period
  ))

merged_data <- health_longer %>%
  mutate(period = as.character(period)) %>%
  bind_rows(
    health_slopes %>%
    rename(period = term) %>%
    select(county, period ,Deficiency, Cases = estimate) %>%
    mutate(period = case_when(
            str_detect(period, "period") ~ "predicted",
            TRUE ~ period))
            )


merged_data %>%
  filter(county == "Embu", Deficiency == "total_dewormed") %>%
  print(n =31)

previous_dewormed <- merged_data %>%
  filter(county == "Embu", Deficiency == "total_dewormed") %>%
  filter(period == "2023-05-28") %>%
  pull(Cases)

predicted_dewormed <- merged_data %>%
  filter(county == "Embu", Deficiency == "total_dewormed") %>%
  filter(period == "predicted")  %>%
  pull(Cases)




























