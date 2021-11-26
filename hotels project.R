# import libraries
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(dplyr)
getwd()

# load the data set
hotels <- read.csv("hotel_bookings.csv")

# inspect the data
glimpse(hotels)
colnames(hotels)
str(hotels)

# conduct summary statistics
hotels %>%
  summarise(average_lead_time = mean(lead_time),
            max_lead_time = max(lead_time),
            min_lead_time = min(lead_time))

# analyze the data
# total bookings
hotels %>%
  group_by(hotel, is_canceled) %>%
  filter(is_canceled == 0) %>%
  summarise(bookings = n(), .groups = 'drop')

# number of bookings for every month
hotels %>%
  group_by(is_canceled, arrival_date_month) %>%
  filter(is_canceled == 0) %>%
  summarise(num_of_bookings = n(), .groups = 'drop') %>%
  arrange(arrival_date_month)
# order the months 
hotels$arrival_date_month <- ordered(hotels$arrival_date_month, 
                                     levels = c('January','February','March','April',
                                                'May','June','July','August','September',
                                                'October','November','December'))
# order the days
hotels$arrival_date_day_of_month <- ordered(hotels$arrival_date_day_of_month, 
                                            levels = c('Monday','Tuesday','Wednesday',
                                                       'Thursday','Friday','Saturday',
                                                       'Sunday'))
# number of bookings by day
hotels %>%
  filter(is_canceled == 0) %>%
  group_by(is_canceled, arrival_date_day_of_month) %>%
  summarise(bookings_per_day = n(), .groups = 'drop') %>%
  arrange(arrival_date_day_of_month)

# lead time per hotel type
hotels %>%
  group_by(hotel)%>%
  summarise(average_lead_time = mean(lead_time), .groups = 'drop')

# hotel type bookings per month
hotels %>%
  filter(is_canceled == 0) %>%
  group_by(hotel, is_canceled, arrival_date_month) %>%
  summarise(booking_per_hotel = n(), .groups = 'drop') %>%
  arrange(arrival_date_month)
  
# filter the hotels types into different data frames
city_hotel <- filter(hotels, hotels$hotel == "City Hotel")
resort <- filter(hotels, hotels$hotel == "Resort Hotel")

## further analysis with visualization
# people with children lead time check in
ggplot(data=hotels)+
  geom_point(mapping = aes(x=lead_time,y=children))

# people with children weekend stay
ggplot(data=hotels)+
  geom_point(mapping = aes(x=stays_in_weekend_nights, y=children))

# distribution channel
ggplot(data = hotels) +
  geom_bar(mapping = aes(x = distribution_channel))

# market segment distribution channel
ggplot(data = hotels) +
  geom_bar(mapping = aes(x = distribution_channel, fill=market_segment))

# distribution channel by deposit type
ggplot(data = hotels) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_grid(~deposit_type) +
  theme(axis.text.x = element_text(angle = 45))

## market segment per hotel type
ggplot(data = hotels) +
  geom_bar(mapping = aes(x = hotel, fill = market_segment))

# market segment
ggplot(data = hotels) +
  geom_bar(mapping = aes(x = hotel)) +
  facet_wrap(~market_segment)

# online TA market segment 
onlineta_city_hotels_v2 <- hotels %>%
  filter(hotel=="City Hotel") %>%
  filter(market_segment=="Online TA")

ggplot(data = onlineta_city_hotels_v2) +
  geom_point(mapping = aes(x = lead_time, y = children))

ggplot(data = hotels) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel)+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title= "segmented hotel")
# minimun and maximum dates
mindate <- min(hotels$arrival_date_year)
maxdate <- max(hotels$arrival_date_year)

# market segment comparison by hotel type
ggplot(data = hotels) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       caption=paste0("Data from: ", mindate, " to ", maxdate),
       x="Market Segment",
       y="Number of Bookings")

ggsave('Hotel booking chart.png')

ggsave('hotel_booking_chart.png',
       width=16,
       height=8)
