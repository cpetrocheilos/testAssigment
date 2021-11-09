library(dplyr)
library(readr)
library(readr)
library(tidyr)
library(lubridate)

# Read Data
data <- read_csv("data.csv",
                 col_types = cols(session_id = col_character(),
                                  date = col_date(format = "%Y-%m-%d")))
#'
#' Info about the experiment
#'
#' The experiment has 3 groups
#'
#' control group = Original
#' Variations 1 = Page loads with popup
#' Variation 2 = Popup loads when user clicks an item on the menu
#'
#' Target: Improve convention rate of transaction among the variations
#'   


# 1. How long the experiment is available? ###########################################
max(data$date) - min(data$date) # 127 days

# how many observations we have each month
data %>%
  mutate(year_month = paste0(lubridate::year(date),"-",lubridate::month(date))) %>%
  group_by(year_month,variation_name) %>%
  summarise(obj = n())

# Since there are only 5 observations in July 2018 and this might affect seasonality it will removed from the dataset
data <-
  data %>%
  filter(lubridate::month(date) != 7)

max(data$date) - min(data$date) # 38 days after cleaning.

#' Also the randomization of observations among between the three groups is clear.
#'
#' The columns exp_participated_page, exp_id, exp_name has the same values for all the data
#' frame so is useless for the experiment evaluation
#'
# Remove columns not needed
data$exp_participated_page <- NULL
data$exp_id <- NULL
data$exp_name <- NULL
#

# Split session_id column
data <- data %>%
  separate(session_id,c("user_id","visit_id"),remove = FALSE)

# 
unique_sessions <- n_distinct(data$session_id)
unique_sessions # There are 21677 unique sessions
# 
unique_users <- n_distinct(data$user_id)
unique_users # There are 16932 unique users
# 
# Calculate unique users per variations scale
# user_per_variations <- data %>%
#   group_by(variation_name) %>%
#   summarise(users = n_distinct(user_id),.groups = "drop")
# 
# Data consistency ###################################################################

consistency = data %>% 
  group_by(variation_name,event_key,date) %>% 
  summarise(obs = n())

library(ggplot2)

ggplot(consistency)+
  geom_line(aes(x = date, y = obs,col = variation_name))+
  geom_point(aes(x = date, y = obs,col = variation_name))+
  facet_wrap(~event_key)+
  scale_x_date(breaks = '2 days',date_labels = "%d")+
  scale_y_log10()

#' Findings 
#' 
#' The plot shows crossing trend lines between three variations for all the events
#' that shows lower statistical confidence.
#' The data looks quite enough and this show that the change is not significant.
#' 
######################################################################################
# Analyze events rates ###############################################################
# Below analyze the success rate of each event key for each variation to see any significance 
# 
# in transaction
# 
test_transactions <-
  data %>% 
  mutate(transaction_made = ifelse(event_key =='transaction',1,0),
         variation_name = as.factor(variation_name)) 

test_treat = test_transactions[test_transactions$variation_name !="Original",]
test_treat$variation_name = droplevels(test_treat$variation_name)


test_treat1 = test_transactions[test_transactions$variation_name !="Variation #1",]
test_treat1$variation_name = droplevels(test_treat1$variation_name)

test_treat2 = test_transactions[test_transactions$variation_name !="Variation #2",]
test_treat2$variation_name = droplevels(test_treat2$variation_name)


prop.test(xtabs(~variation_name+transaction_made, data=test_treat)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat)[,2:1])
prop.test(xtabs(~variation_name+transaction_made, data=test_treat1)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat1)[,2:1])
prop.test(xtabs(~variation_name+transaction_made, data=test_treat2)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat2)[,2:1])
# 
# In checkout.loaded
# 
test_checkout.loaded <-
  data %>% 
  mutate(transaction_made = ifelse(event_key =='checkout.loaded',1,0),
         variation_name = as.factor(variation_name)) 

test_treat = test_checkout.loaded[test_checkout.loaded$variation_name !="Original",]
test_treat$variation_name = droplevels(test_treat$variation_name)


test_treat1 = test_checkout.loaded[test_checkout.loaded$variation_name !="Variation #1",]
test_treat1$variation_name = droplevels(test_treat1$variation_name)

test_treat2 = test_checkout.loaded[test_checkout.loaded$variation_name !="Variation #2",]
test_treat2$variation_name = droplevels(test_treat2$variation_name)


prop.test(xtabs(~variation_name+transaction_made, data=test_treat)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat)[,2:1])
prop.test(xtabs(~variation_name+transaction_made, data=test_treat1)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat1)[,2:1])
prop.test(xtabs(~variation_name+transaction_made, data=test_treat2)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat2)[,2:1])
# 
# in suggested_modal.shop_list.clicked
# 
test_suggested_modal.shop_list.clicked <-
  data %>% 
  mutate(transaction_made = ifelse(event_key =='suggested_modal.shop_list.clicked',1,0),
         variation_name = as.factor(variation_name)) 

test_treat = test_suggested_modal.shop_list.clicked[test_suggested_modal.shop_list.clicked$variation_name !="Original",]
test_treat$variation_name = droplevels(test_treat$variation_name)


test_treat1 = test_suggested_modal.shop_list.clicked[test_suggested_modal.shop_list.clicked$variation_name !="Variation #1",]
test_treat1$variation_name = droplevels(test_treat1$variation_name)

test_treat2 = test_suggested_modal.shop_list.clicked[test_suggested_modal.shop_list.clicked$variation_name !="Variation #2",]
test_treat2$variation_name = droplevels(test_treat2$variation_name)


prop.test(xtabs(~variation_name+transaction_made, data=test_treat)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat)[,2:1])
prop.test(xtabs(~variation_name+transaction_made, data=test_treat1)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat1)[,2:1])
prop.test(xtabs(~variation_name+transaction_made, data=test_treat2)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat2)[,2:1])
# 
# in suggested_popup.closed
# 
test_suggested_popup.closed <-
  data %>% 
  mutate(transaction_made = ifelse(event_key =='suggested_popup.closed',1,0),
         variation_name = as.factor(variation_name)) 

test_treat = test_suggested_popup.closed[test_suggested_modal.shop_list.clicked$variation_name !="Original",]
test_treat$variation_name = droplevels(test_treat$variation_name)


test_treat1 = test_suggested_popup.closed[test_suggested_modal.shop_list.clicked$variation_name !="Variation #1",]
test_treat1$variation_name = droplevels(test_treat1$variation_name)

test_treat2 = test_suggested_popup.closed[test_suggested_modal.shop_list.clicked$variation_name !="Variation #2",]
test_treat2$variation_name = droplevels(test_treat2$variation_name)


prop.test(xtabs(~variation_name+transaction_made, data=test_treat)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat)[,2:1])
prop.test(xtabs(~variation_name+transaction_made, data=test_treat1)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat1)[,2:1])
prop.test(xtabs(~variation_name+transaction_made, data=test_treat2)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat2)[,2:1])
# 
# in suggested_shop.clicked 
# 
test_suggested_shop.clicked <-
  data %>% 
  mutate(transaction_made = ifelse(event_key =='suggested_popup.closed',1,0),
         variation_name = as.factor(variation_name)) 

test_treat = test_suggested_shop.clicked[test_suggested_modal.shop_list.clicked$variation_name !="Original",]
test_treat$variation_name = droplevels(test_treat$variation_name)


test_treat1 = test_suggested_shop.clicked[test_suggested_modal.shop_list.clicked$variation_name !="Variation #1",]
test_treat1$variation_name = droplevels(test_treat1$variation_name)

test_treat2 = test_suggested_shop.clicked[test_suggested_modal.shop_list.clicked$variation_name !="Variation #2",]
test_treat2$variation_name = droplevels(test_treat2$variation_name)


prop.test(xtabs(~variation_name+transaction_made, data=test_treat)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat)[,2:1])
prop.test(xtabs(~variation_name+transaction_made, data=test_treat1)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat1)[,2:1])
prop.test(xtabs(~variation_name+transaction_made, data=test_treat2)[,2:1])
t.test(xtabs(~variation_name+transaction_made, data=test_treat2)[,2:1])
# Convention Rate ####################################################################
# 1. Conversion rate = (Conversions or goals achieved / Total visitors) * 100
convertion_rate_among_unique_users <-
  data %>% 
  filter(event_key =='transaction') %>% 
  group_by(variation_name) %>% 
  summarise(number_of_transactions = n()) %>%
  mutate(conversion_rate_1 =(number_of_transactions/unique_users)*100)
# 
convertion_rate_among_unique_users

# 2. Conversion Rate = (Total number of conversions / Total number of sessions) * 100
convertion_rate_among_sessions <- 
  data %>% 
  filter(event_key =='transaction') %>% 
  group_by(variation_name) %>% 
  summarise(number_of_transactions = n()) %>%
  mutate(conve_rate_1 = number_of_transactions/unique_sessions*100)
# 
convertion_rate_among_sessions

# 3. Conversion rate per visitor 
data %>% 
  group_by(user_id,visit_id) %>% 
  summarise(no_visits = n())



# Calculate users convection for transaction
convetion_per_user = data %>% 
  group_by(user_id,visit_id) %>% 
  summarise(no_visits = n(),.groups = 'drop') %>% 
  merge(data,by = c("user_id",'visit_id'),all.y = TRUE) %>% 
  filter(event_key =='transaction') %>%
  group_by(user_id,variation_name) %>% 
    summarise(no_visits = max(no_visits),
              convertions = n(),
              conve_rate_per_user = convertions/no_visits,.groups = 'drop') 

# There is small difference in mean per visitor 
convetion_per_user %>% 
  group_by(variation_name) %>% 
  summarise(mean_user_convention = mean(conve_rate_per_user))

# I am not wating good p values. 
t.test(conve_rate_per_user~variation_name,data = convetion_per_user[convetion_per_user$variation_name !='Original',])
t.test(conve_rate_per_user~variation_name,data = convetion_per_user[convetion_per_user$variation_name !='Variation #2',])
t.test(conve_rate_per_user~variation_name,data = convetion_per_user[convetion_per_user$variation_name !='Variation #1',])


# Other Insights #####################################################################
######################################################################################
# Clicks between variations
data %>% 
  # mutate(month = lubridate::month(date)) %>% 
    group_by(variation_name) %>%
    summarise(convertion_rate = mean(events))
# Clicks are affected through the variations 

data %>% 
  group_by(variation_name,lubridate::month(date)) %>%
  summarise(convertion_rate = mean(events))
# But this shows to have seasonality between groups and the most clicks for variations happened on the 3rd 2018

data %>% 
  mutate(transaction = ifelse(event_key =='transaction',1,0)) %>% 
  group_by(variation_name,medium) %>% 
  summarise(total_transactions = sum(transaction)) %>% 
  spread(medium,total_transactions)
# It seems that between the variations the number of success transactions are more than for cost per click than the other medium.

