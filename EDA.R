library(tidyverse)
library(dplyr)
library(ggplot2)

df <- read_csv('https://www.dropbox.com/scl/fi/zmvcst0q4s3p4bqi5ybqy/15_train.csv?rlkey=rb3r4i2uid0dsdg59otloissq&dl=1')


df %>% 
  janitor::clean_names() -> df

### Exploratory Data Analysis ---------------------------------------


# (3 points) Examine the characteristics of the dependent variable that
# you’ll be training a model to predict. Look at the distribution and 
# anything else that seems relevant, focusing especially on anything that
# you’ll likely need to account for as you build your model(s). Include one
# or more visualizations to illustrate the most important takeaway(s).


df %>% 
  summarize(mean = mean(price),
            median = median(price),
            sd = sd(price),
            min = min(price),
            max = max(price)
            )

# Wow, this is some incredibly skewed data.
df %>% 
  ggplot(aes(x = price)) +
  geom_density(alpha = .4) +
  theme_bw()


df %>% 
  ggplot(aes(x = price)) +
  geom_density(alpha = 1) +
  geom_histogram(alpha = .4)


# Can I cap it off a little just to get a better picture of the price variable?
# Maybe there is something else we need to look at
price_cap <- quantile(df$price, .99)

df %>% 
  summarize(
    count_pice_over_cap = sum(price > price_cap)
  )

df %>% 
  mutate(price = if_else(price > price_cap, price_cap, price)) %>% 
  ggplot(aes(x = price)) +
  geom_density(alpha = .4)


# (4 points) Summarize the characteristics of the remaining variables in the
# dataset at a high level. This should include any relevant groupings of like
# variables, a description of the number of categorical vs. continuous variables,
# any relevant patterns of missingness or odd distributions, etc. This should not
# be a long, comprehensive section of your report. What we’re looking for here is
# a high-level sketch of the dataset so that, in a page or two, the reader can get
# an idea of the data that you’ll be using and anything in particular that you’ll
# need to watch out for as you progress to building your model.
# (Note that, even though I’m asking for just a page or two of column summary, 
#   your team will likely need to look fairly carefully at each of the columns in 
#   order to identify any lurking issues and know which columns to highlight, etc.)

propertysqft_summary <- df %>% 
  summarize(mean = mean(propertysqft),
            median = median(propertysqft),
            sd = sd(propertysqft),
            min = min(propertysqft),
            max = max(propertysqft)
  )

# The propertysqft column is skewed as well
df %>% 
  ggplot(aes(x = propertysqft)) +
  geom_density(alpha = .4) +
  theme_bw()
# Count unique brokers
brokers <- df %>% 
  count(brokertitle)
# Count type of properties
type_cat <- df %>% 
  count(type)
# Count of property bedroom counts
beds <- df %>% 
  count(beds)
# Count of property bathroom counts
baths <- df %>% 
  count(bath)

df %>% 
  count(brokers)

df %>% 
  glimpse()
df %>% 
  mutate(bedBathDiff = (beds - bath)) %>% 
  filter(abs( bedBathDiff) > 10) %>% 
  print(n=20)


df %>% 
  filter(!str_detect( brokertitle, 'Broker') ) -> noBroker

# (5 points) Perform an initial analysis in which you systematically examine each 
# independent variable as it relates to your dependent variable. This will provide
# your group with an initial idea of which (if any) variable jump out as especially 
# promising features. 

  # - For numeric (continuous) variables, this will likely involve looking at correlations
  # or something similar in order to identify variables (if any) that are likely related 
  # to your dependent variable. Any particularly interesting continuous variables may
  # deserve a scatterplot or some other visual demonstration of the relationship

df %>%
  select(beds, bath, propertysqft, price) %>%
  cor()

# Price vs Bed Scatterplot

ggplot(df, aes(x = beds, y = price)) +
  geom_point(aes(color = bath), size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "No. of Bedrooms", 
       y = "Price", 
       color = "No. of Bathrooms") +
  ggtitle("Price vs Bedrooms") + 
  theme_bw()

#broker title - price (cor)

# Broker title Summary statistics
bokertitle_summary <- df %>%
  group_by(brokertitle) %>%
  summarise(
    mean_price = mean(price),
    median_price = median(price),
    min_price = min(price),
    max_price = max(price)
  )


# For categorical variables, this will involve looking at the pattern of the dependent
  # variable at various levels of the category. This will look different depending on your 
  # dependent variable, but what you’re looking to do is highlight any categorical variables 
  # where the dependent variable appears “different”
  # (in terms of rate, frequency, average, etc.) at different levels of that category. 
  # Summarize anything you find particularly interesting or relevant using an appropriate 
  # visualization.


cor(df$price, df$propertysqft, method="pearson")

# Finding correlations between numeric values
df %>%
  summarize(across(is.numeric, list(correlation_with_price = ~cor(.x, price, method = "pearson")))) %>%
  pivot_longer(cols = everything()) %>% 
  print(n=30)


# (scatter plot) How does sublocality look, could it be used to group as neighborhoods?
df %>% select(price, address:longitude) %>% 
  ggplot(aes(x = longitude, y = latitude, color = sublocality)) +
  geom_point()

# (boxplot) How does sublocality relate to price? Note I'm capping price to help with visibility
df %>%
  select(price, address:longitude) %>%
  mutate(price = if_else(price > price_cap, price_cap, price)) %>%
  ggplot(aes(y = price, x = sublocality)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

=======
# Lets analyze the price by the type of property
type_price <- df %>% 
  group_by(type) %>% 
  summarize(mean = mean(price),
            median = median(price),
            sd = sd(price),
            min = min(price),
            max = max(price)
  )

# Create a boxplot of price by property type to see distributions. 
# We're going to exclude the listings that have prices above the price cap
df %>% 
  mutate(price = if_else(price > price_cap, price_cap, price)) %>% 
  ggplot(aes(x = type,
             y = price)) +
  geom_boxplot() +
  theme_bw()
