# Load packages
library(broom)
library(tidyverse)


# Analysis questions

# - which drugs are the worst for employment? 
# - which are the worst for income? 

# LOAD DATA ---------------------------------------------------------------

# Read and clean names
dat_nsduh_raw <- read_csv("data/raw/NSDUH Workforce Adults.csv")

dat_nsduh <- dat_nsduh_raw %>% 
  rename(
    id = X1
  ) %>% 
  janitor::clean_names()

# CLEAN DATA --------------------------------------------------------------

# Collapse categorical variables

# make never variables
dat_nsduh <- dat_nsduh %>% 
  
  mutate_at(
    .vars = vars(matches("ever")),
    .funs = list(
      never = function(x) {as.numeric(x != 1)}
    )
  )

# Pivot to long
dat_nsduh_long <- dat_nsduh %>% 
  
  pivot_longer(
    cols = c(matches("year$|month$|ever$|never$")), 
    names_to = "drug_period", 
    values_to = "values"
  )

# Extract drug and last_use
dat_nsduh_long <- dat_nsduh_long %>% 
  
  mutate(
    drug = drug_period %>% 
      str_remove("year|month|ever_never|ever") %>% 
      str_remove_all("_"), 
    last_use = drug_period %>% 
      str_extract("year|month|ever$|never")
  ) %>% 
  filter(
    !(drug %in% c("anydrugnomjnever", "countofdrugs", "drugtest2never", "drugtestnever"))
  )

# Filter for most recent last use
# - relevel last_use
# - arrange 
dat_nsduh_long <- dat_nsduh_long %>% 
  mutate(
    last_use = fct_relevel(last_use, "month", "year", "ever", "never")
  ) %>% 
  
  arrange(last_use) %>% 
  filter(values == 1) %>% 
  group_by(id, drug) %>% 
  slice(1)

# Recode outcome variables 
dat_nsduh_tidy <- dat_nsduh_long %>% 
  
  ungroup() %>% 
  mutate(
    personal_income = recode(
     personal_income,
     "1" = 5000,
     "2" = 15000,
     "3" = 25000,
     "4" = 35000,
     "5" = 45000,
     "6" = 62500,
     "7" = 90000
    ),
    employed = employment_status != 3, 
    last_use = fct_rev(last_use)
  )


# Select relevant columns and pivot to wide
dat_nsduh_clean <- dat_nsduh_tidy %>%
  
  select(
    id,
    drug, 
    last_use, 
    personal_income, 
    employed
  ) %>% 
  
  filter(
    !(drug %in% c("illicit", "pharma"))
  ) %>% 
  
  pivot_wider(
    names_from = "drug", 
    values_from = "last_use", 
    names_prefix = "last_use_"
  ) %>% 
  
  select(-last_use_anydrug)


# REGRESS AND PRESENT -----------------------------------------------------

# Fit models
fit_emp <- lm(employed ~ ., data = dat_nsduh_clean %>% select(-id, -personal_income))

# Create summary tables
fit_emp_tidy <- broom::tidy(fit_emp)


# Plot estimated coefficients
fit_emp_tidy <- fit_emp_tidy %>% 
  
  filter(term != "(Intercept)") %>% 
  mutate(
    drug = str_remove(term, "ever|year|month"), 
    last_use = str_extract(term, "ever|year|month"),
    c5 = estimate - 1.96 * std.error, 
    c95 = estimate + 1.96 * std.error
  )

fit_emp_tidy %>% 
  
  ggplot(aes(last_use, estimate)) + 
  geom_segment(aes(xend = last_use, y = c5, yend = c95)) + 
  geom_point(shape = 21, fill = "tomato") + 
  geom_hline(yintercept = 0) + 
  
  facet_wrap(vars(drug)) + 
  coord_flip()



# Fit models
fit_income <- lm(personal_income ~ ., data = dat_nsduh_clean %>% select(-id, -employed))

# Create summary tables
fit_income_tidy <- broom::tidy(fit_income)


# Plot estimated coefficients
fit_income_tidy <- fit_income_tidy %>% 
  
  filter(term != "(Intercept)") %>% 
  mutate(
    drug = str_remove(term, "ever|year|month"), 
    last_use = str_extract(term, "ever|year|month"),
    c5 = estimate - 1.96 * std.error, 
    c95 = estimate + 1.96 * std.error
  )

fit_income_tidy %>% 
  mutate(last_use = fct_relevel(last_use, "ever", "year", "month")) %>% 
  
  ggplot(aes(last_use, estimate)) + 
  geom_segment(aes(xend = last_use, y = c5, yend = c95)) + 
  geom_point(shape = 21, fill = "tomato") + 
  geom_hline(yintercept = 0) + 
  
  facet_wrap(vars(drug)) + 
  coord_flip()

