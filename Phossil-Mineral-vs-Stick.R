library(tidyverse)
library(lubridate)
library(xml2)
library( gridExtra )
library(cowplot)

#pulls in data
data <- read.csv("raw-data/orders.csv")
data <- data %>% select(order_number, entry_date, dist_id, sku, qty_sold, dist_status, order_type) %>%
  mutate(entry_date = ymd(entry_date)) %>%
  filter(order_type != "R - Replacement") %>%
  mutate(qty_sold = ifelse(order_type == "C - Credit/RMA", qty_sold * -1, qty_sold))

#summarizes skus by month and qty sold
grouped_data <- data %>% mutate(entry_month = month(entry_date)) %>%
  group_by(sku, entry_date, dist_status) %>% 
  summarize(qty_sold = sum(qty_sold)) %>%
  arrange(sku)

#creates graph variable of 2 items
pwc <- grouped_data %>% mutate(sku = factor(sku)) %>%  # Convert 'sku' to a factor
  filter(dist_status == "W - Wholesale") %>%
  ggplot(aes(entry_date, qty_sold, fill = sku)) +
  geom_bar(stat = "identity") +
  ggtitle("WC")
  
pwa <- grouped_data %>% mutate(sku = factor(sku)) %>%  # Convert 'sku' to a factor
  filter(dist_status == "D - Distributor") %>%
ggplot(aes(entry_date, qty_sold, fill = sku)) +
  geom_bar(stat = "identity")+
  ggtitle("WA")

# Align the plots horizontally
plot_grid(pwc, pwa, align = "h")

#Find out which dist bought mineral stick for further analysis
Stick_data <- data %>% filter(sku == 60226907) %>%
  group_by(dist_id) %>%
  summarize(qty_sold = sum(qty_sold)) %>%
  write.csv("Stick_Dist_data.csv")

#Imports all orders for above dist IDs
stick_customer_orders <- read.csv("raw-data/stick_orders.csv")

stick_customer_orders <- stick_customer_orders %>% select(dist_id, order_number, pv_amount, product_sales, pv_date) %>%
  arrange(pv_date) %>% filter(pv_date %in% c("202402", "202403")) %>%
  group_by(dist_id, pv_date) %>%
  summarize(pv = sum(pv_amount))

stick_customer_orders %>% ggplot(aes(pv, fill = as.factor(pv_date), alpha = .5)) +
  geom_density(bw = .5) +
  scale_x_log10()

#Make feb and march data seperate columns for analysis
stick_customer_orders_wide <- stick_customer_orders %>% pivot_wider(names_from = pv_date, values_from = pv) %>%
  drop_na("202402", "202403")

#seperate out march and feb data
Feb_data <- stick_customer_orders_wide %>% select(dist_id, "202402")

March_data <- stick_customer_orders_wide %>% select(dist_id, "202403")

result <- t.test(Feb_data$'202402', March_data$'202403', paired = TRUE)
print(result)

