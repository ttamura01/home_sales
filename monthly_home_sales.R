# monthly existing home sales

setwd("/Users/takayukitamura/Documents/R_Computing/home_sales")
library(tidyverse)
library(dplyr)
library(glue)
library(ggtext)
library(scales)

home_sales <- read_csv("/Users/takayukitamura/Documents/R_Computing/home_sales/monthly_homesales.csv") #%>% 
# rename(date = DATE, existing_home_sales = EXHOSLUSM495S)

head(home_sales)
tail(home_sales)

# home_sales <- home_sales %>% 
#   select(date, existing_home_sales)

updates <- tribble(~date, ~existing_home_sales,
                   "2025-07-01", 4010000) %>% 
  mutate(home_sales_mil = existing_home_sales/1000000)


home_sales <- rbind(home_sales, updates) #%>% 
# mutate(home_sales_mil = existing_home_sales/1000000)

write_csv(home_sales, "/Users/takayukitamura/Documents/R_Computing/home_sales/monthly_homesales.csv")

home_sales$latest_data <- ifelse(home_sales$date == max(home_sales$date), TRUE, FALSE)

home_sales_change <- home_sales %>% 
  mutate(latest_data = ifelse(home_sales$date == max(home_sales$date), TRUE, FALSE),
         yoy_change = (home_sales_mil/lag(home_sales_mil, 12) -1) *100,
         mom_change = (home_sales_mil/lag(home_sales_mil, 1) -1) *100,
         mom_change_status = if_else(is.na(mom_change), NA_character_,
                                     if_else(mom_change > 0, "increased",
                                             if_else(mom_change < 0, "declined", "flat")))) 

latest_month <- home_sales_change %>% 
  slice_max(date)


latest_month <- latest_month %>% 
  mutate(month_label = month(date, label = TRUE, abbr = FALSE),
         previous_month_label = month(date %m-% months(1), label = TRUE, abbr = FALSE))

# latest_month_label <- latest_month[7]
# previous_month_label <- latest_month[8]
# latest_home_sales <- latest_month[,3] 
# latest_yoy_change <- latest_month[,5]
# latest_mom_change <- latest_month[6]

latest_month_label <- latest_month$month_label
previous_month_label <- latest_month$previous_month_label
latest_home_sales <- latest_month$home_sales_mil
latest_yoy_change <- latest_month$yoy_change
latest_mom_change <- latest_month$mom_change
latest_mom_change_status <- latest_month$mom_change_status


home_sales %>% 
  ggplot(aes(x = date, y = home_sales_mil, fill = latest_data)) +
  geom_col(show.legend = FALSE) +
  geom_text(data = subset(home_sales, latest_data == TRUE),
            aes(label = glue("{home_sales_mil}\n mil")), 
            vjust = -0.2, hjust = 0.65, fontface = "bold", color = "blue", size = 6, lineheight = .75 ) +
  scale_fill_manual(breaks = c(TRUE, FALSE),
                    values = c("#0000FF", "#AAAAAA")) +
  # scale_x_date(limits = c(min(home_sales$date)-2, max(home_sales$date) + 6)) +
  scale_y_continuous(limits = c(0, 4.5),
                     breaks = seq(0, 5, 1),
                     label = seq(0, 5, 1)) +
  labs(title = glue("Home Sales in {latest_month_label} {latest_mom_change_status} {round(latest_mom_change,2)}% from {previous_month_label} to the SAAR at {latest_home_sales} mil"),
       #subtitle = "Despite the High home prices and mortgage rates continue to weigh on sales activity", 
       caption = "National Association of Realtors", 
       x = NULL,
       y = "Exingting Home Sales (mil)") +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 14, face = "bold", margin = margin(b = 0.9)),
    plot.subtitle = element_textbox_simple(face = "italic"),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 12, face = "bold")
  )

ggsave("existing_home_sales_monthly_col.png", height = 5.5, width = 6.0)

home_sales %>% 
  select(date, home_sales_mil) %>% 
  ggplot(aes(x = date, y = home_sales_mil)) +
  geom_line(show.legend = FALSE) +
  geom_text(data = subset(home_sales, latest_data == TRUE),
            aes(label = glue("{home_sales_mil}\n mil")), 
            vjust = -0.2, hjust = 0.65, fontface = "bold", color = "blue", size = 6, lineheight = .75 ) +
  # scale_x_continuous(limits = c(2023-09-01, ))
  labs(title = glue("Home Sales in {latest_month_label} {latest_mom_change_status} {round(latest_mom_change,2)}% from {previous_month_label} to the SAAR at {latest_home_sales} mil"),
       caption = "National Association of Realtors", 
       x = NULL, 
       y = "Annualized Home Sales (mil)",
       
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 14, face = "bold", margin = margin(t= 1, r= 1, b = 1, l = 1)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_textbox_simple()
  )

ggsave("existing_home_sales_monthly_line.png", height = 5.5, width = 6.0)
