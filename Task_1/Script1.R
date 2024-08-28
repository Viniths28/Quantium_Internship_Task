library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(readxl)
library (tidyverse)

purchase_behav <- read.csv("QVI_purchase_behaviour.csv")
transaction <- read_excel("QVI_transaction_data.xlsx")

head(transaction)

#### Examine transaction data
str(transaction)

#### Convert DATE column to a date format
transaction$DATE <- as.Date(transaction$DATE, origin = "1899-12-30")

#### Examine PROD_NAME
summary(transaction$PROD_NAME)

transaction %>% 
  group_by(transaction$PROD_NAME) %>% 
  summarise (count =n())

#### Examine the words in PROD_NAME to see if there are any incorrect entries
#### such as products that are not chips
productWords <- data.table(unlist(strsplit(unique(transaction$PROD_NAME), " ")))

setnames(productWords, 'words')

#### Removing digits
productWords <- productWords[!grepl("\\d|&", words)]

productWords <- productWords[!grepl("[0-9]", words)]

#### Remove Special characters
productWords <- productWords[!grepl("[^a-zA-Z0-9]", words)]

#### Let's look at the most common words by counting the number of times a 
#### word appears and sorting them by this frequency in order of 
#### highest to lowest frequency

productWords <- productWords %>% group_by(words)%>%
  summarize(count=n())

productWords <- productWords %>% arrange(desc(productWords$count))

#### Remove salsa products
transaction <- as.data.table(transaction)
transaction[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transaction <- transaction[SALSA == FALSE, ][, SALSA := NULL]

#### Summarise the data to check for nulls and possible outliers
summary(transaction)

#### Filter the dataset to find the outlier
outliers <- transaction[PROD_QTY == 200]
print(outliers)

#### See if the customer has had other transactions
customer_id <- outliers$LYLTY_CARD_NBR[1]
customer_transactions <- transaction[LYLTY_CARD_NBR == customer_id]
print(customer_transactions)

#### Filter out the customer based on the loyalty card number
transaction <- transaction[LYLTY_CARD_NBR != customer_id]

#### Re-examine transaction data
summary(transaction)

#### Count the number of transactions by date
transactions_by_day <- transaction %>% group_by(DATE)%>%
  summarize(count=n())

colnames(transactions_by_day) <- c('DATE','N') 

print(transactions_by_day)

#### Create a sequence of dates
seq_dates <- data.table(DATE = seq.Date(as.Date("2018-07-01"), 
                                        as.Date("2019-06-30"), by = "day"))
transactions_by_day <- merge(seq_dates, transactions_by_day, by = "DATE", 
                             all.x = TRUE)
transactions_by_day[is.na(N), N := 0]

#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#### Plot transactions over time
ggplot(transactions_by_day, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Days", y = "Number of transactions", 
       title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Filter to December and look at individual days
december_transactions <- transactions_by_day[DATE >= as.Date("2018-12-01") & 
                                               DATE <= as.Date("2018-12-31")]

ggplot(december_transactions, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", 
       title = "Transactions over time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Pack size
#### We can work this out by taking the digits that are in PROD_NAME
transaction[, PACK_SIZE := parse_number(PROD_NAME)]

#### Let's check if the pack sizes look sensible
transaction[, .N, PACK_SIZE][order(PACK_SIZE)]

pack_sizeSummary <- transaction[, .N, PACK_SIZE][order(PACK_SIZE)]
print(pack_sizeSummary)


#### plot Histogram
ggplot(pack_sizeSummary, aes(x = PACK_SIZE)) +
  geom_histogram(binwidth = 10, fill = "yellow", color = "black") +
  labs(x = "Size", y = "Number of quantity sold", 
       title = "Packsize") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Extract first name 
transaction[, BRAND := toupper(sub(" .*", "", PROD_NAME))]

#### Summary of brand name
brand_summary <- transaction[, .N, BRAND][order(-N)]

#### Clean brand names
transaction[BRAND == "RED", BRAND := "RRD"]

transaction[, .N, BRAND][order(-N)]

### Examining customer data
summary(purchase_behav)
str(purchase_behav)

#### Merge transaction data to customer data
data <- merge(transaction, purchase_behav, all.x = TRUE)

#### Check for missing customer details
missing_customers <- data[is.na(LIFESTAGE) | is.na(PREMIUM_CUSTOMER)]
print(missing_customers)

#### Write it to CSV
fwrite(data, paste0("QVI_data.csv"))

#### Total sales by LIFESTAGE and PREMIUM_CUSTOMER
sales_by_segment <- data[, .(total_sales = sum(TOT_SALES)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

ggplot(sales_by_segment, aes(x = PREMIUM_CUSTOMER, y = total_sales, 
                             fill = LIFESTAGE)) +
  geom_bar(stat = "identity", position = "identity") +
  labs(x = "PREMIUM CUSTOMER", y = "Total Sales", 
       title = "Total Sales by Lifestage and Premium Customer")

#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customers_by_segment <- data[, .N, by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

ggplot(customers_by_segment, aes(x = PREMIUM_CUSTOMER, y = N, 
                             fill = LIFESTAGE)) +
  geom_bar(stat = "identity", position = "identity") +
  labs(x = "PREMIUM CUSTOMER", y = "Number of Customer", 
       title = "Number of Customers by Lifestage and Premium Customer")


#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
units_per_customer <- data[, .(avg_units = mean(PROD_QTY)), 
                           by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

ggplot(units_per_customer, aes(x = PREMIUM_CUSTOMER, y = avg_units, 
                               fill = LIFESTAGE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "PREMIUM CUSTOMER", y = "Average Units per Customer", 
       title = "Average Units per Customer by Lifestage and Premium Customer")

### Average Chip Price per unit 
average_chip_price <-data[, .(avg_price = mean(TOT_SALES / PROD_QTY)), 
                          by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

ggplot(average_chip_price, aes(x = PREMIUM_CUSTOMER, y = avg_price, 
                               fill = LIFESTAGE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "PREMIUM CUSTOMER", y = "Average price per unit", 
       title = "Average Units per Unit by Lifestage and Premium Customer")

#### T-Test
# To perform a t-test to see if the difference in average prices 
# between mainstream and premium/budget midage and young singles and couples 

mainstream_data <- data[LIFESTAGE %in% 
                          c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") 
                        & PREMIUM_CUSTOMER == "Mainstream"]

premium_budget_data <- data[LIFESTAGE %in% 
                              c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") 
                            & PREMIUM_CUSTOMER %in% c("Budget", "Premium")]

t_test <- t.test(mainstream_data$TOT_SALES / mainstream_data$PROD_QTY, 
                 premium_budget_data$TOT_SALES / premium_budget_data$PROD_QTY)

print(t_test)

#### Preferred pack size compared to the rest of the population

mainstream_young_singles_couples <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & 
                                           PREMIUM_CUSTOMER == "Mainstream"]


pack_size_preferences <- mainstream_young_singles_couples %>% group_by(PACK_SIZE)%>%
  summarize(count=n())
pack_size_preferences <- pack_size_preferences %>% arrange(desc(pack_size_preferences$count))

print(pack_size_preferences)

#### Preferred Brand compared to the rest of the population
Brand_preferences <- mainstream_young_singles_couples %>% group_by(BRAND)%>%
  summarize(count=n())
Brand_preferences <- Brand_preferences %>% arrange(desc(Brand_preferences$count))

print(Brand_preferences)

