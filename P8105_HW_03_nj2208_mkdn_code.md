HW\_03\_nj2208
================
James Ng
10/12/2019

## Problem 1

``` r
library(p8105.datasets)
data("instacart")
```

### 1a.

The data set is a dataframe of 15 variables and 1384617 observations.
The data frame describes the items available for ordering, their
departments and the time of day that they get ordered.

``` r
dept=instacart %>% 
  as_tibble %>% 
  select(order_id:add_to_cart_order,aisle_id:department) %>% 
  group_by(aisle_id,aisle) %>%  
  summarise(
    total=n()
  ) %>% 
  arrange(
   -total
  )
```

### 1.b

There are 134 aisles in total and the most items are ordered from aisle
83, the fresh vegetables aisle.

``` r
c_dept = dept %>%
  as.data.frame() %>% 
  filter(total>10000) %>% 
  arrange(aisle_id)

c_dept %>% 
  ggplot(aes(x = total, y = aisle))+
  geom_point()
```

<img src="P8105_HW_03_nj2208_mkdn_code_files/figure-gfm/part 1c-1.png" width="90%" />

``` r
baking=instacart %>% 
  group_by(aisle_id,aisle,product_name) %>% 
  filter(
    aisle=="baking ingredients"
    ) %>% 
  summarise(
    total=n()
  ) %>% 
  arrange(
    -total
  ) %>% 
  head(3)

dog_food=instacart %>% 
  group_by(aisle_id,aisle,product_name) %>% 
  filter(
    aisle=="dog food care"
    ) %>% 
  summarise(
    total=n()
  ) %>% 
  arrange(
    -total
  ) %>% 
  head(3)

pack_veg_fruits=instacart %>% 
  group_by(aisle_id,aisle,product_name) %>% 
  filter(
    aisle=="packaged vegetables fruits"
    ) %>% 
  summarise(
    total=n()
  ) %>% 
  arrange(
    -total
  ) %>% 
  head(3)

d_table = bind_rows(baking,dog_food,pack_veg_fruits) %>% 
  rename(total_ordered=total) %>% 
knitr::kable(digits = 1)
```

| aisle\_id | aisle                      | product\_name                                 | total\_ordered |
| --------: | :------------------------- | :-------------------------------------------- | -------------: |
|        17 | baking ingredients         | Light Brown Sugar                             |            499 |
|        17 | baking ingredients         | Pure Baking Soda                              |            387 |
|        17 | baking ingredients         | Cane Sugar                                    |            336 |
|        40 | dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |             30 |
|        40 | dog food care              | Organix Chicken & Brown Rice Recipe           |             28 |
|        40 | dog food care              | Small Dog Biscuits                            |             26 |
|       123 | packaged vegetables fruits | Organic Baby Spinach                          |           9784 |
|       123 | packaged vegetables fruits | Organic Raspberries                           |           5546 |
|       123 | packaged vegetables fruits | Organic Blueberries                           |           4966 |

``` r
pink_lady = instacart %>% 
  select(product_name,order_dow,order_hour_of_day) %>% 
  filter(
    product_name=="Pink Lady Apples"
    ) %>%
  group_by(product_name,order_dow) %>% 
  summarise(
    avg_hour_ordered=mean(order_hour_of_day)
  ) %>% 
  pivot_wider(
    names_from = order_dow,
    values_from = avg_hour_ordered
  )

ice_cream = instacart %>% 
  filter(
    product_name=="Coffee Ice Cream"
    ) %>% 
  select(product_name,order_dow,order_hour_of_day) %>% 
  group_by(product_name,order_dow) %>% 
  summarise(
    avg_hour_ordered=mean(order_hour_of_day)
  ) %>% 
  pivot_wider(
    names_from = order_dow,
    values_from = avg_hour_ordered
  )

e_table = bind_rows(pink_lady,ice_cream) %>% 
  rename(Monday="0",
         Tuesday="1",
         Wednesday="2",
         Thursday="3",
         Friday="4",
         Saturday="5",
         Sunday="6",
         Product="product_name") %>%
knitr::kable(digits = 1)
```

| Product          | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday |
| :--------------- | -----: | ------: | --------: | -------: | -----: | -------: | -----: |
| Pink Lady Apples |   13.4 |    11.4 |      11.7 |     14.2 |   11.6 |     12.8 |   11.9 |
| Coffee Ice Cream |   13.8 |    14.3 |      15.4 |     15.3 |   15.2 |     12.3 |   13.8 |

## Problem 2

``` r
library(p8105.datasets)
data("brfss_smart2010")
```

``` r
brfss_cleaning=brfss_smart2010 %>% 
  janitor::clean_names() %>% 
  filter(topic == "Overall Health") %>% 
  rename(state = locationabbr,location=locationdesc) %>% 
  mutate(
    response=factor(response,levels = c("Poor","Fair","Good","Very good","Excellent")),
  ) 
```

``` r
brfss_2002 =  brfss_cleaning %>% 
  filter(year==2002) %>% 
  group_by(state) %>% 
  summarise(
    observed = n()/5
  ) %>% 
  filter(observed>=7)

brfss_2010 =  brfss_cleaning %>% 
  filter(year==2010) %>% 
  group_by(state) %>% 
  summarise(
    observed = n()/5
  ) %>% 
  filter(observed>=7)
```

CT, FL, MA, NC, NJ, PA were the states with 7 or more than observation
sites in 2002.

CA, CO, FL, MA, MD, NC, NE, NJ, NY, OH, PA, SC, TX, WA were the states
with 7 or more than observation sites in 2010.

``` r
exc_health=brfss_cleaning %>% 
  filter(response=="Excellent") %>% 
  group_by(year,state) %>%
  summarise(
    avg_data_value=mean(data_value,na.rm = TRUE)
  )

exc_health %>%   
  ggplot(aes(x=year,y=avg_data_value,color=state))+
  geom_line(alpha=.3,aes(group=state))+
  theme(legend.position="none")
```

<img src="P8105_HW_03_nj2208_mkdn_code_files/figure-gfm/problem 2b plot-1.png" width="90%" />

``` r
nyc_data_2006=brfss_cleaning %>% 
  filter(state=="NY",year=="2006") %>% 
  group_by(response)

nyc_data_2010=brfss_cleaning %>% 
  filter(state=="NY",year=="2010") %>% 
  group_by(response)

nyc_data=bind_rows(nyc_data_2006,nyc_data_2010) %>% 
  group_by(year,response) %>% 
  summarise(
    data_avg=mean(data_value)
  )

nyc_data %>% 
  ggplot(aes(x=response,y=data_avg))+
  geom_col()+
  facet_grid(~year)+
  labs(
    x="Response",
    y="Number of Responses",
    title="Survey Data in NYS" 
  )
```

<img src="P8105_HW_03_nj2208_mkdn_code_files/figure-gfm/problem 2c-1.png" width="90%" />

## Problem 3

``` r
accel=read_csv(file="./accel_data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
accel_mod=accel %>% 
  janitor::clean_names() %>% 
  mutate(
    weekday_vs_weekend = case_when(
      day == "Monday" ~ "Weekday",
      day == "Tuesday" ~ "Weekday",
      day == "Wednesday" ~ "Weekday",
      day == "Thursday" ~ "Weekday",
      day == "Friday" ~ "Weekday",
      day == "Saturday" ~ "Weekend",
      day == "Sunday" ~ "Weekend",
  )) %>% 
  select(week,day_id,day_of_the_week = day,weekday_vs_weekend,everything()) %>% 
  pivot_longer(
    activity_1:activity_1440,
    names_to = "activity_num", 
    values_to = "activity_count"
    )
```

The cleaned dataset is a data set with 6 variables and 50400
observations. The variables of this dataframe are week, day\_id,
day\_of\_the\_week, weekday\_vs\_weekend, activity\_num,
activity\_count.

``` r
activity_day=accel_mod %>% 
  group_by(day_id,day_of_the_week) %>% 
  summarize(
    total_activity=sum(activity_count)
  )
activity_day %>% 
 knitr::kable(digits = 1)
```

| day\_id | day\_of\_the\_week | total\_activity |
| ------: | :----------------- | --------------: |
|       1 | Friday             |        480542.6 |
|       2 | Monday             |         78828.1 |
|       3 | Saturday           |        376254.0 |
|       4 | Sunday             |        631105.0 |
|       5 | Thursday           |        355923.6 |
|       6 | Tuesday            |        307094.2 |
|       7 | Wednesday          |        340115.0 |
|       8 | Friday             |        568839.0 |
|       9 | Monday             |        295431.0 |
|      10 | Saturday           |        607175.0 |
|      11 | Sunday             |        422018.0 |
|      12 | Thursday           |        474048.0 |
|      13 | Tuesday            |        423245.0 |
|      14 | Wednesday          |        440962.0 |
|      15 | Friday             |        467420.0 |
|      16 | Monday             |        685910.0 |
|      17 | Saturday           |        382928.0 |
|      18 | Sunday             |        467052.0 |
|      19 | Thursday           |        371230.0 |
|      20 | Tuesday            |        381507.0 |
|      21 | Wednesday          |        468869.0 |
|      22 | Friday             |        154049.0 |
|      23 | Monday             |        409450.0 |
|      24 | Saturday           |          1440.0 |
|      25 | Sunday             |        260617.0 |
|      26 | Thursday           |        340291.0 |
|      27 | Tuesday            |        319568.0 |
|      28 | Wednesday          |        434460.0 |
|      29 | Friday             |        620860.0 |
|      30 | Monday             |        389080.0 |
|      31 | Saturday           |          1440.0 |
|      32 | Sunday             |        138421.0 |
|      33 | Thursday           |        549658.0 |
|      34 | Tuesday            |        367824.0 |
|      35 | Wednesday          |        445366.0 |

There are 2 saturdays where there is practically no activity by the man
under observation.

``` r
act_by_day=accel_mod %>% 
  group_by(day_id)

act_by_day %>% 
  ggplot(aes(x=day_id, y=activity_count))+
  geom_col()+
  facet_grid(~day_of_the_week)
```

<img src="P8105_HW_03_nj2208_mkdn_code_files/figure-gfm/problem 3c-1.png" width="90%" />

This person experiences the most fluctuation in his activity over the
weekends and monday. His activity is fairly consistent across most of
the other days of the week. He also has 2 saturdays of literally no
activity when compared to the other days.
