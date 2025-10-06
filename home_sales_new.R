library(tidyverse)
library(glue)
library(ggtext)
library(scales)
library(patchwork)
# optional for nice model extraction
# library(broom)

us_home_sales <- read_csv("/Users/takayukitamura/Documents/R_Computing/home_sales/us_existing_home_sales.csv") 

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")
population <- fredr(series_id = "B230RC0A052NBEA") %>% 
  select(date, population = value)

updates <- tribble(~date, ~population,
                   "2025-01-01", 342555)

population <- population %>% 
  rbind(., updates)

home_pop <- population %>% 
  mutate(year = year(date)) %>% 
  select(year, population) %>%  
  inner_join(., us_home_sales, by = "year")

# --- 1) Split samples & fit models
home_pop_pre <- home_pop %>% filter(year <= 2007)
home_pop_post <- home_pop %>% filter(year >= 2008, year <= 2022)

m_pre  <- lm(existing_home_sales ~ population, data = home_pop_pre)
m_post <- lm(existing_home_sales ~ population, data = home_pop_post)

# --- 2) Compute predicted (trend) sales using the post-GFC model for modern years
home_pop_trend <- home_pop %>%
  mutate(trend_sales = predict(m_post, newdata = cur_data()),
         gap_mn = existing_home_sales - trend_sales,
         gap_pct = gap_mn / trend_sales)

# --- 3) Table for your newsletter: 2023–2025 gap
gap_tbl <- home_pop_trend %>%
  filter(year >= 2023) %>%
  transmute(
    year,
    population_thou = population,
    actual_mn = existing_home_sales,
    trend_mn = trend_sales,
    gap_mn,
    gap_pct = 100*gap_pct
  )

print(gap_tbl)
# If you want pretty rounding:
gap_tbl %>%
  mutate(across(c(actual_mn, trend_mn, gap_mn), ~round(.x, 2)),
         gap_pct = round(gap_pct, 1)) %>%
  print(n = Inf)

# --- 4) Clean labels for equations
fmt_coef <- function(fit) {
  b <- coef(fit)
  glue("Y = {round(b[2], 5)}×X + {round(b[1], 3)}\nR² = {round(summary(fit)$r.squared, 3)}")
}

eq_pre  <- fmt_coef(m_pre)
eq_post <- fmt_coef(m_post)

# --- 5) Charts
# p1: bars with below-trend highlight post-2022
p1 <- home_pop_trend %>%
  mutate(is_latest = year == max(year),
         below_trend = if_else(year >= 2023 & gap_mn < 0, TRUE, FALSE)) %>%
  ggplot(aes(year, existing_home_sales, fill = below_trend)) +
  geom_col() +
  geom_text(data = ~ filter(.x, is_latest),
            aes(label = glue("Est.\n{round(existing_home_sales,2)}M")),
            vjust = -0.6, fontface = "bold", color = "blue", size = 3.8) +
  scale_fill_manual(values = c(`TRUE` = "#f4a261", `FALSE` = "#9aa0a6")) +
  scale_y_continuous(limits = c(0, 7.5), labels = label_number(accuracy = 0.01)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "US Existing Home Sales (1981–2025 est.)",
    subtitle = "Shaded bars (since 2023) indicate actual sales below the population-based trend",
    x = NULL, y = "Existing Home Sales (Million Units)",
    caption = "Source: NAR (sales), BEA/Census via FRED (population). Chart: Takayuki Tamura"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 16, face = "bold"),
    legend.position = "none",
    panel.background = element_blank()
  )

# p2: pre-GFC
p2 <- home_pop_pre %>%
  ggplot(aes(population, existing_home_sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  annotate("label", x = min(home_pop_pre$population)*1.02,
           y = max(home_pop_pre$existing_home_sales)*0.98,
           label = eq_pre, hjust = 0, vjust = 1,
           size = 3.3, color = "red") +
  labs(
    title = "Population vs Existing Home Sales — Pre-GFC (≤2007)",
    x = "US Population (thousands)", y = "Existing Home Sales (millions)"
  ) +
  theme(
    panel.background = element_blank()
  )

# p3: post-GFC (2008–2022 fit) + highlight 2023–2025
p3 <- home_pop %>%
  ggplot(aes(population, existing_home_sales)) +
  geom_point(data = home_pop_post, alpha = 0.8) +
  geom_smooth(data = home_pop_post, method = "lm", se = TRUE) +
  geom_point(data = subset(home_pop, year >= 2023), color = "red", size = 2.8) +
  ggrepel::geom_text_repel(
    data = subset(home_pop, year >= 2023),
    aes(label = year), size = 3, max.overlaps = 10
  ) +
  annotate("label", x = min(home_pop_post$population)*1.01,
           y = max(home_pop_post$existing_home_sales)*0.99,
           label = eq_post, hjust = 0, vjust = 1,
           size = 3.3, color = "red") +
  labs(
    title = "Population vs Existing Home Sales — Post-GFC Fit (2008–2022)\n2023–2025 highlighted",
    x = "US Population (thousands)", y = "Existing Home Sales (millions)"
  ) +
  theme(
    plot.title = element_textbox_simple(),
    panel.background = element_blank()
  )

# p4: residuals vs year (actual - trend from post-GFC model)
p4 <- home_pop_trend %>%
  ggplot(aes(year, gap_mn)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_col(aes(fill = gap_mn < 0)) +
  scale_fill_manual(values = c("TRUE"="#e76f51","FALSE"="#2a9d8f")) +
  labs(title = "Deviation from Population-Based Trend (Post-GFC model)",
       subtitle = "Millions of units: positive = above trend, negative = below trend",
       x = NULL, y = "Actual – Trend (millions)", caption = "Trend from 2008–2022 regression") +
  theme(
    legend.position = "none",
    panel.background = element_blank())

# Layout & save
final_plot <- p1 / (p2 + p3) / p4 + plot_layout(heights = c(1, 1, 0.8))
ggsave("home_sales_population_trend.png", final_plot, width = 10, height = 12, dpi = 300)
