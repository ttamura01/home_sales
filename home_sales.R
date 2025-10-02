setwd("/Users/takayukitamura/Documents/R_Computing/home_sales")
library(tidyverse)
library(dplyr)
library(glue)
library(ggtext)
library(scales)
us_home_sales <- read_csv("/Users/takayukitamura/Documents/R_Computing/home_sales/us_existing_home_sales.csv")

tail(us_home_sales)
summary(us_home_sales)

us_home_sales <- us_home_sales %>% 
  mutate(existing_home_sales = ifelse(year == 2024, 4.06, existing_home_sales))

us_home_sales %>% 
  summarise(mean = mean(existing_home_sales),
            median = median(existing_home_sales),
            min = min(existing_home_sales),
            max = max(existing_home_sales),
            "25th_percentile" = quantile(existing_home_sales, 0.25),
            "75th_percentile" = quantile(existing_home_sales, 0.75),
            "10th_percentile" = quantile(existing_home_sales, 0.10),
            "90th_percentile" = quantile(existing_home_sales, 0.90),
            sd = sd(existing_home_sales),
            count= length(existing_home_sales)
  )

highlight_data <- us_home_sales %>% 
  slice_max(year)

us_home_sales$latest_data <- ifelse(us_home_sales$year == max(us_home_sales$year), TRUE, FALSE)

us_home_sales %>% 
  ggplot(aes(x = year, y = existing_home_sales, fill = latest_data)) +
  geom_col(show.legend = FALSE) +
  geom_text(data = subset(us_home_sales, latest_data == TRUE),
            aes(label = glue("{existing_home_sales}")), 
            vjust = -0.5, hjust = 0.75, fontface = "bold", color = "blue", size = 6 ) +
  scale_fill_manual(breaks = c(FALSE, TRUE),
                    values = c("#AAAAAA", "#0000FF")) +
  scale_y_continuous(limits = c(0, 7.5),
                     breaks = seq(0, 6, 2),
                     labels = label_comma(accuracy = 0.01),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(1980, 2025),
                     expand = c(0.01,0)) +
  labs(title = "US Existing Home Sales in November at 4.15mil (annualized), recovered from recent level but still low",
       x = NULL,
       y = "US Existing Home Sales (Million Units)",
       caption = "source: NRA, WSJ") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 16, face = "bold")
    )

ggsave("us_home_sales.png", width = 6, height = 4.5)

us_home_sales %>% 
  ggplot(aes(x = existing_home_sales)) +
  geom_histogram(bins = 8) +
  geom_density(aes(y = ..count..), color = "red")

us_home_sales %>% 
  ggplot(aes(x = existing_home_sales)) +
  geom_density()

#assess the correlation between US Population and home sales
population <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=B230RC0A052NBEA&scale=left&cosd=1929-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-11-08&revision_date=2024-11-08&nd=1929-01-01") %>% 
  rename(date = observation_date, population = B230RC0A052NBEA)

updates <- tribble(~date, ~population,
                   "2024-01-01", 341320)

population <- population %>% 
  rbind(population, updates)

mean(us_home_sales$existing_home_sales)

home_pop <- population %>% 
  mutate(year = year(date)) %>% 
  select(year, population) %>%  
  inner_join(., us_home_sales, by = "year")

tail(home_pop)

model <- lm(existing_home_sales~population, data = home_pop)
summary(model)

home_pop %>% 
  ggplot(aes(x = population, y = existing_home_sales)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(2, 7.5),
                     breaks = seq(2, 7.5, 1),
                     labels = label_comma(accuracy = 0.01),
                     expand = c(0.01,0)) +
  scale_x_continuous(limits = c(250000, 340000),
   breaks = seq(240000, 340000, 25000),
   labels = label_comma(accuracy = 0.1),
   expand = c(0.01,0)) +
  labs(
    title = "US Exinging Home Sales and US Population",
    x = "US Population (x1,000)",
    y = "US Existing Home Sales (million units)",
    caption = "source: NRA, WSJ"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 16, face = "bold")
  )

model <- lm(existing_home_sales~population, data = home_pop)
summary(model)

ggsave("us_home_sales_pop.png", width = 6, height = 4.5)

## pre-financial crisis

home_pop_pre_gfc <- home_pop %>% 
  filter(year <= 2007)

model <- lm(existing_home_sales~population, data = home_pop_pre_gfc)

coefficients <-  coef(model)

intercept <- format(coefficients[1], digits = 5, scientific = TRUE)

slope <- format(coefficients[2], digits = 5, scientific = TRUE)

r.squared <- format(summary(model)$r.squared, digits = 3)

home_pop_pre_gfc %>% 
  ggplot(aes(x = population, y = existing_home_sales)) +
  geom_point()+
  geom_smooth(method = "lm") +
  annotate("text", x = 255000, y = 6.0,
           label = glue("Y = X * {slope} + {intercept} \n R^2 = {r.squared}"), color = "red", size=5, fontface = "italic") + 
  scale_y_continuous(limits = c(2, 7.5),
                     breaks = seq(2, 7.5, 1),
                     labels = label_comma(accuracy = 0.01),
                     expand = c(0.01,0)) +
  scale_x_continuous(limits = c(225000, 305000),
                     breaks = seq(225000, 305000, 25000),
                     labels = label_comma(accuracy = 0.1),
                     expand = c(0.01,0)) +
  labs(
    title = "US Exinging Home Sales and US Population - Pre-GFC(~2007)",
    x = "US Population (x1,000)",
    y = "US Existing Home Sales (million units)",
    caption = "source: NRA, WSJ") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 16, face = "bold",margin = margin(b=5)))

ggsave("us_home_saves_pop_preGFC.png", width = 6, height = 4.5)

### post financial crisis
home_pop_post_gfc <- home_pop %>% 
  filter(year >= 2008 & year <= 2022)

model <- lm(existing_home_sales~population, data = home_pop_post_gfc)

coefficients <-  coef(model)

intercept <- format(coefficients[1], digits = 4, scientific = TRUE)

slope <- format(coefficients[2], digits = 4, scientific = TRUE)

r.squared <- format(summary(model)$r.squared, digits = 3)

home_pop_post_gfc %>% 
  ggplot(aes(x = population, y = existing_home_sales)) +
  geom_point()+
  geom_smooth(method = "lm") +
  geom_point(aes(x = 335208, y = 4.09), color = "darkgreen") +
  geom_point(aes(x = 335893, y = 4.06), color = "red", size =3) + 
  annotate("text", x = 315000, y = 6.0,
           label = glue("Y = X * {slope} + {intercept} \n R^2 = {r.squared }"), color = "red", fontface = "italic") +
  scale_y_continuous(limits = c(3.5, 6.5),
                     breaks = seq(3.5, 6.5, 1),
                     labels = label_comma(accuracy = 0.01),
                     expand = c(0.01,0)) +
  scale_x_continuous(limits = c(304000, 337000),
                     breaks = seq(304000, 337000, 10000),
                     labels = label_comma(accuracy = 0.1),
                     expand = c(0.01,0)) +
  labs(
    title = "US Exinging Home Sales and US Population - Post-GFC(2008~)",
    x = "US Population (x1,000)",
    y = "US Existing Home Sales (million units)",
    caption = "source: NRA, WSJ") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(b=5)))

ggsave("us_home_saves_pop_postGFC.png", width = 6, height = 4.5)

### prediction

335893*5.441e-05 + -1.245e+01

# monthly existing home sales

home_sales <- read_csv("/Users/takayukitamura/Documents/R_Computing/home_sales/monthly_homesales.csv") #%>% 
  # rename(date = DATE, existing_home_sales = EXHOSLUSM495S)

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

latest_month <- home_sales %>% 
  mutate(latest_data = ifelse(home_sales$date == max(home_sales$date), TRUE, FALSE),
         yoy_change = (home_sales_mil/lag(home_sales_mil, 12) -1) *100,
         mom_change = (home_sales_mil/lag(home_sales_mil, 1) -1) *100) %>% 
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


home_sales %>% 
  ggplot(aes(x = date, y = home_sales_mil, fill = latest_data)) +
  geom_col(show.legend = FALSE) +
  geom_text(data = subset(home_sales, latest_data == TRUE),
            aes(label = glue("{home_sales_mil} mil")), 
            vjust = -0.5, hjust = 0.75, fontface = "bold", color = "blue", size = 6 ) +
  scale_fill_manual(breaks = c(TRUE, FALSE),
                    values = c("#0000FF", "#AAAAAA")) +
  scale_y_continuous(limits = c(0, 4.5),
                     breaks = seq(0, 5, 1),
                     label = seq(0, 5, 1)) +
  labs(title = glue("Home Sales in {latest_month_label} rose {round(latest_mom_change,2)}% from {previous_month_label} to the SAAR at {latest_home_sales} mil"),
       subtitle = "Despite the High home prices and mortgage rates continue to weigh on sales activity", 
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

ggsave("monthly_home_sales.png", height = 4.5 , width = 6)

home_sales %>% 
  select(date, home_sales_mil) %>% 
  ggplot(aes(x = date, y = home_sales_mil)) +
  geom_line(show.legend = FALSE) +
  # scale_x_continuous(limits = c(2023-09-01, ))
  labs(title = "US Home Sales fell 2.7% in June from the prior month to annual rate at 3.93 million",
       subtitle = "the weak home sales attributable to the Record High Home Prices and the significantly declined consumer sentiment in April due to uncertainty asslociated the Tariffs") +
  theme_classic() +
  theme(
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    plot.subtitle = element_textbox_simple()
  )
  
    

#######

x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 5, 4, 5)

model <- lm(y ~ x)

coefficients <- coef(model)

intercept <- coefficients[1]
slope <- coefficients[2]

3 * 0.6 + 2.2

new_data <- data.frame(x = c(6, 7, 8))

predictions <- predict(model, newdata = new_data)

