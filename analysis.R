# PROJECT 5: CONSUMER SHOPPING BEHAVIOR
# Load the dataset

shopping_data <- read.csv("consumer_shopping_behavior.csv")

# Display first six records
head(shopping_data)

# Check dataset structure
str(shopping_data)

# Check number of rows and columns
dim(shopping_data)

# Check missing values
colSums(is.na(shopping_data))

# DESCRIPTIVE STATISTICS

# Average consumer age
mean_age <- mean(shopping_data$Age)

# Average monthly income
mean_income <- mean(shopping_data$Monthly_Income_PHP)

# Average monthly spending
mean_spending <- mean(shopping_data$Monthly_Spending_PHP)

# Average purchase amount
mean_purchase <- mean(shopping_data$Average_Purchase_PHP)

# Average monthly transactions
mean_transactions <- mean(shopping_data$Monthly_Transactions)

# Average satisfaction rating
mean_satisfaction <- mean(shopping_data$Satisfaction_Rating)

# Display results
mean_age
mean_income
mean_spending
mean_purchase
mean_transactions
mean_satisfaction

# Summary of the dataset
summary(shopping_data)

# SHOPPING FREQUENCY

frequency_count <- table(
  shopping_data$Shopping_Frequency
)

frequency_percentage <- prop.table(
  frequency_count
) * 100

frequency_count
frequency_percentage


# PREFERRED SHOPPING METHOD

method_count <- table(
  shopping_data$Preferred_Shopping_Method
)

method_count


# PRODUCT CATEGORY

category_count <- table(
  shopping_data$Product_Category
)

category_count


# PAYMENT METHOD

payment_count <- table(
  shopping_data$Payment_Method
)

payment_count

# CREATE FIGURES FOLDER

if (!dir.exists("figures")) {
  dir.create("figures")
}

# GRAPH 1: Average Spending by Product Category

category_spending <- aggregate(
  Monthly_Spending_PHP ~ Product_Category,
  data = shopping_data,
  FUN = mean
)

png(
  "figures/average_spending_by_category.png",
  width = 800,
  height = 600
)

barplot(
  category_spending$Monthly_Spending_PHP,
  names.arg = category_spending$Product_Category,
  col = "steelblue",
  main = "Average Monthly Spending by Product Category",
  xlab = "Product Category",
  ylab = "Average Spending (PHP)",
  las = 2
)

dev.off()

# GRAPH 2: Monthly Income vs Monthly Spending

png(
  "figures/income_vs_spending.png",
  width = 800,
  height = 600
)

plot(
  shopping_data$Monthly_Income_PHP,
  shopping_data$Monthly_Spending_PHP,
  main = "Monthly Income vs Monthly Spending",
  xlab = "Monthly Income (PHP)",
  ylab = "Monthly Spending (PHP)",
  pch = 19,
  col = "blue"
)

abline(
  lm(
    Monthly_Spending_PHP ~ Monthly_Income_PHP,
    data = shopping_data
  ),
  col = "red",
  lwd = 2
)

dev.off()

# GRAPH 3: Shopping Frequency Distribution

frequency_count <- table(
  shopping_data$Shopping_Frequency
)

png(
  "figures/shopping_frequency_distribution.png",
  width = 800,
  height = 600
)

barplot(
  frequency_count,
  col = "seagreen",
  main = "Shopping Frequency Distribution",
  xlab = "Shopping Frequency",
  ylab = "Number of Consumers",
  las = 2
)

dev.off()

# CORRELATION ANALYSIS

income_spending_correlation <- cor(
  shopping_data$Monthly_Income_PHP,
  shopping_data$Monthly_Spending_PHP
)

transactions_spending_correlation <- cor(
  shopping_data$Monthly_Transactions,
  shopping_data$Monthly_Spending_PHP
)

income_spending_correlation

transactions_spending_correlation

# SAVE SUMMARY STATISTICS

summary_statistics <- data.frame(
  Measure = c(
    "Average Age",
    "Average Monthly Income",
    "Average Monthly Spending",
    "Average Purchase Amount",
    "Average Monthly Transactions",
    "Average Satisfaction Rating",
    "Income and Spending Correlation",
    "Transactions and Spending Correlation"
  ),
  
  Value = c(
    mean_age,
    mean_income,
    mean_spending,
    mean_purchase,
    mean_transactions,
    mean_satisfaction,
    income_spending_correlation,
    transactions_spending_correlation
  )
)

write.csv(
  summary_statistics,
  "summary_statistics.csv",
  row.names = FALSE
)


# SAVE SHOPPING FREQUENCY SUMMARY

frequency_summary <- data.frame(
  Shopping_Frequency = names(frequency_count),
  Number_of_Consumers = as.integer(frequency_count)
)

write.csv(
  frequency_summary,
  "shopping_frequency_summary.csv",
  row.names = FALSE
)

