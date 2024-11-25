install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(stringr)

data <- read.csv("F:/amazon.csv", stringsAsFactors = FALSE)

str(data)
data$discounted_price <- as.numeric(gsub("[₹,]", "", data$discounted_price))
data$actual_price <- as.numeric(gsub("[₹,]", "", data$actual_price))

head(data[, c("discounted_price", "actual_price")])

data$discounted_price <- as.numeric(gsub("[^0-9.]", "", data$discounted_price))
data$actual_price <- as.numeric(gsub("[^0-9.]", "", data$actual_price))

sum(is.na(data$discounted_price)) 
sum(is.na(data$actual_price))    

exchange_rate <- 0.012
data$discounted_price_usd <- data$discounted_price * exchange_rate
data$actual_price_usd <- data$actual_price * exchange_rate

head(data[, c("discounted_price_usd", "actual_price_usd")])

data$rating <- as.numeric(data$rating)

data$rating_count <- as.numeric(gsub(",", "", data$rating_count))

install.packages("stringr")
library("stringr")
data <- data %>%
  mutate(category_split = str_split_fixed(category, "\\|", 3))

data$main_category <- data$category_split[, 1]
data$sub_category1 <- data$category_split[, 2]
data$sub_category2 <- data$category_split[, 3]

head(data[, c("main_category", "sub_category1", "sub_category2")])

data$category <- str_split_fixed(data$category, "\\|", 3)

data <- data[, !duplicated(names(data))]

filtered_data <- data %>%
  filter(rating >= 4.0, rating_count >= 1000)

head(filtered_data)

avg_price_by_category <- data %>%
  group_by(main_category) %>%
  summarise(avg_discounted_price = mean(discounted_price, na.rm = TRUE)) %>%
  arrange(desc(avg_discounted_price))

print(avg_price_by_category)
install.packages("ggplot")
library("ggplot")
library("ggplot2")
ggplot(data, aes(x = rating)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Product Ratings", x = "Rating", y = "Count")

top_reviewed_products <- data %>%
  arrange(desc(rating_count)) %>%
  select(product_name,rating_count) %>%
  head(10)

print(top_reviewed_products)
write.csv(data, "cleaned_amazon_data.csv", row.names = FALSE)
top_reviewed_products <- filtered_data %>%
  arrange(desc(rating_count)) %>%
  head(10)

ggplot(top_reviewed_products, aes(x = reorder(product_name, -rating_count), y = rating_count)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Top 10 Most Reviewed Products", x = "Product", y = "Review Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  coord_flip()  
str(filtered_data$discount_percentage)

unique(filtered_data$discount_percentage)
filtered_data$discount_percentage <- as.numeric(gsub("[^0-9.]", "", filtered_data$discount_percentage))

sum(is.na(filtered_data$discount_percentage))
avg_discount_by_category <- filtered_data %>%
  group_by(main_category) %>%
  summarise(avg_discount = mean(discount_percentage, na.rm = TRUE)) %>%
  arrange(desc(avg_discount))

print(avg_discount_by_category)

str(avg_discount_by_category)

head(avg_discount_by_category)

sum(is.na(avg_discount_by_category))
avg_discount_by_category <- avg_discount_by_category %>%
  ggplot(filtered_data, aes(x = discounted_price_usd, y = rating)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Relationship Between Price and Ratings", x = "Discounted Price (USD)", y = "Rating") +
  theme_minimal()

avg_rating_by_category <- filtered_data %>%
  group_by(main_category) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating))
ggplot(avg_rating_by_category, aes(x = reorder(main_category[,1], -avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  labs(title = "Average Rating by Category", x = "Category", y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
avg_discount_by_category <- filtered_data %>%
  group_by(main_category) %>%
  summarise(avg_discount = mean(discount_percentage, na.rm = TRUE)) %>%
  arrange(desc(avg_discount))

ggplot(avg_discount_by_category, aes(x = reorder(main_category[,1], -avg_discount), y = avg_discount)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Average Discount Percentage by Category", x = "Category", y = "Average Discount (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
ggplot(filtered_data, aes(x = main_category[,1], y = discounted_price_usd)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Price Distribution by Category", x = "Category", y = "Discounted Price (USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
ggplot(filtered_data, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Product Ratings", x = "Rating", y = "Count") +
  theme_minimal()
ggplot(filtered_data, aes(x = main_category[,1], y = discount_percentage)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Discount Percentage by Category", x = "Category", y = "Discount Percentage (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
ggplot(filtered_data, aes(x = discount_percentage, y = rating)) +
  geom_point(alpha = 0.6, color = "red") +
  labs(title = "Rating vs. Discount Percentage", x = "Discount Percentage (%)", y = "Rating") +
  theme_minimal()
product_count_by_category <- filtered_data %>%
  group_by(main_category) %>%
  summarise(count = n())

ggplot(product_count_by_category, aes(x = reorder(main_category[,1], -count), y = count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Count of Products by Category", x = "Category", y = "Count of Products") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
avg_rating_discount <- filtered_data %>%
  group_by(main_category) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE), avg_discount = mean(discount_percentage, na.rm = TRUE))

ggplot(avg_rating_discount, aes(x = main_category[,1], y = avg_discount, fill = avg_rating)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Heatmap of Average Rating and Discount by Category", x = "Main Category", y = "Average Discount (%)", fill = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
ggsave("avg_discount_by_category.png", width = 10, height = 6)
write.csv(filtered_data, "filtered_data.csv", row.names = FALSE)













