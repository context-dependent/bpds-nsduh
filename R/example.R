library(tidyverse)

nsduh_data <- read_csv("data/raw/NSDUH Workforce Adults.csv")

nsduh_data <- nsduh_data %>% 
  
  rename(
    id = X1
  ) %>% 
  
  janitor::clean_names()

nsduh_data_long <- nsduh_data %>% 
  
  mutate_at(
    vars(matches("ever$")), 
    list(never = function(x) {as.numeric(!x)})
  ) %>% 
  
  pivot_longer(
    cols = c(matches("year$|month$|ever$|never$")),
    names_to = "drug_period", 
    values_to = "value"
  ) %>% 
  
  mutate(
    drug = drug_period %>% str_remove("year|month|ever_never|ever") %>% str_remove_all("_"),
    last_use = drug_period %>% str_extract("year|month|ever$|never") %>% 
      fct_relevel("month", "year", "ever", "never")
  ) %>% 
  
  filter(
    value == 1
  ) %>% 
  
  arrange(last_use) %>% 
  group_by(id, drug) %>% 
  slice(1) %>% 
  select(-value, -drug_period) %>% 
  ungroup()
  

x %>% 
  pivot_wider(names_from = "drug", values_from = "last_use", names_prefix = "last_use_")


nsduh_data_long %>% group_by(drug, last_use) %>% summarize(employment_rate = mean(employment_status))

nsduh_data_tidy <- nsduh_data_long %>% 
  
  mutate(
    employed = employment_status != 3, 
    personal_income = 
      
      recode(
        personal_income, 
        "1" = 5000, 
        "2" = 15000, 
        "3" = 25000,
        "4" = 35000, 
        "5" = 45000, 
        "6" = 62500, 
        "7" = 90000
      ),
    last_use = fct_rev(last_use)
    
  ) %>% 
  
  select(
    id, drug, last_use, employed, personal_income
  ) %>% 
  
  filter(drug != "countofdrugs", drug != "illicit", drug != "pharma")

nsduh_data_tidy %>% 
  group_by(drug, last_use) %>% 
  summarize(employment_rate = mean(employed)) %>% 
  ggplot(aes(last_use, employment_rate))+ 
  geom_line(aes(group = drug)) + 
  geom_point(aes(fill = drug), shape = 21, size = 3) + 
  ggforce::geom_mark_circle(aes(label = drug, group = drug), data = function(d) d %>% group_by(drug) %>% slice(n()))


nsduh_data_fit <- nsduh_data_tidy %>% 
  pivot_wider(
    names_from = "drug",
    values_from = "last_use", 
    names_prefix = "last_use_"
  ) %>% 
  select(-id) 



fit_emp <- lm(employed ~ ., data = nsduh_data_fit %>% select(-personal_income))

fit_income <- lm(personal_income ~ ., data = nsduh_data_fit %>% select(-employed))


fit_emp_tidy <- fit_emp %>% broom::tidy() %>% 
  mutate(
    drug = term %>% str_remove("year|month|ever"), 
    last_use = term %>% str_extract("year|month|ever") %>% fct_relevel("ever", "year", "month")
  )

fit_emp_tidy %>% 
  filter(term != "(Intercept)") %>% 
  mutate(c5 = estimate - 2 * std.error, c95 = estimate + 2 * std.error) %>% 
  ggplot(aes(last_use, estimate)) + 
  geom_segment(aes(xend = last_use, y = c5, yend = c95)) + 
  geom_point(shape = 21, size = 3, fill = "tomato") +
  geom_hline(yintercept = 0, lwd = 1) + 
  
  
  facet_wrap(vars(drug)) + 
  coord_flip() +
  bptheme::theme_blueprint(grid = "Xx")


fit_income_tidy <- fit_income %>% broom::tidy() 

fit_income_tidy %>% 
  filter(term != "(Intercept)") %>% 
  mutate(c5 = estimate - 2 * std.error, c95 = estimate + 2 * std.error) %>% 
  ggplot(aes(term, estimate)) + 
  geom_segment(aes(xend = term, y = c5, yend = c95)) + 
  geom_point(shape = 21, size = 3, fill = "tomato") +
  geom_hline(yintercept = 0) + 
  
  
  coord_flip()

