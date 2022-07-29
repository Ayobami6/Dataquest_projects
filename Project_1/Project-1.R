library(tidyverse)
library(janitor)
for (col in colnames(df)) {
  print(class(df[[col]]))
  }

str(df)

typeof(df$price)
class(df$price)


for (col in colnames(df)) {
  print(col)
  print(unique(df[[col]]))
}

table(df$review)

sum(is.na(df))

new_df = df %>% 
  filter(!is.na(review))
## Cleaning the dataset 

clean_df <- new_df %>% 
  mutate(
    state = case_when(
      state == 'California' ~ 'CA',
      state == 'New York' ~ 'NY',
      state == 'Texas' ~ 'TX',
      state == 'Florida' ~ 'FL',
      TRUE ~ state
    )
  )

# using the table function to get the preview of the book sold 
table(clean_df$book)

# Analysing the data to satistify the objectives
library(ggplot2)
analysis <- clean_df %>% 
  group_by(book) %>% 
  summarise(revenue = sum(price)) %>% 
  arrange(-revenue) %>% 
  ggplot(aes(x=book, y=revenue, fill=book)) + geom_col(position = 'dodge') + 
  labs(title = "Total sold price per books",
       subtitle = 'This shows the total price all the books sold', 
       x= "Books", 
       y= 'Total sold price', 
       caption= "Data from dataquest class") + theme(axis.text.x = element_blank() #element_text(angle=90, hjust=1,
                                                                                #face = "bold"
                                                       )
analysis <- analysis + scale_fill_brewer(palette="Set2") + theme(legend.position = 'bottom') 
analysis
ggsave('Result_chart.png', dpi=200)
help("theme")
clean_df %>% 
  group_by(book) %>% 
  summarise(sold = n()) %>%
  arrange(-sold)



df_clean %>%
  mutate(weekday=wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides =n(), average_duration = mean(ride_duration)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) + geom_col(position = 'dodge')
ggsave('barchart.png', dpi=200)
