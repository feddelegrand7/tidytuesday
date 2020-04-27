library(tidyverse)
library(lubridate)


# Reading the Data --------------------------------------------------------


grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 10000)

cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')




# Deflating the prices ----------------------------------------------------


grosses <- grosses %>% mutate(
  
  year_month =  paste(year(grosses$week_ending), month(grosses$week_ending), sep = "-")
  
) 

cpi <- cpi %>% rename(consumer_price_index = cpi) 
  
cpi <- cpi %>%  mutate(
    
    year_month =  paste(year(cpi$year_month), month(cpi$year_month), sep = "-")
    
    )


full_data <- grosses %>% left_join(cpi, by = "year_month")



# deflated prices ---------------------------------------------------------

full_data$consumer_price_index

deflated_data <- full_data %>% mutate_at(vars(weekly_gross_overall,
                             weekly_gross, 
                             potential_gross,
                             avg_ticket_price,
                             top_ticket_price), 
                             .funs = ~./(consumer_price_index / 100))




# plotting ----------------------------------------------------------------


deflated_data %>% group_by(theatre) %>% 
ggplot(aes(avg_ticket_price, seats_sold)) +
  geom_hex() +
  scale_fill_gradientn(colours = c("black", 
                                   "#214D72", 
                                   "#2C7695", 
                                   "#50BFC3",
                                   "#F7C232")) +
  labs(x = "Average price of tickets sold ($ deflated)", 
       y = "Total seats sold", 
       title = "What's the most common price-seats combination in Broadway ?",
       caption = "by @moh_fodil for TidyTuesday 18") +
  hrbrthemes::theme_modern_rc(axis_title_size  = 14, plot_title_size = 12)












