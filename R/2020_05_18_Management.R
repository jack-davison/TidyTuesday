
library(tidyverse)
library(tidymodels)
library(tidytuesdayR)
library(ggthemr)

ggthemr("fresh", layout = "scientific", text_size = 14)

# Data Processing ---------------------------------------------------------------

# Read in
tt = tt_load("2021-05-18")

# View README
tt

# Extract Data
data = tt$survey

# Get UK Data -------------------------------------------------------------

# Filter for UK data
uk = data %>%
  filter(currency == "GBP") %>%
  janitor::remove_empty(which = c("rows", "cols")) %>%
  janitor::remove_constant() %>%
  select(-currency_other, -additional_context_on_income, -additional_context_on_job_title) %>%
  mutate(timestamp = lubridate::mdy_hms(timestamp),
         overall_experience = parse_number(overall_years_of_professional_experience),
         field_experience = parse_number(years_of_experience_in_field),
         age = parse_number(how_old_are_you),
         .keep = "unused")

# Do some lumping
uk_lumped = uk %>%
  transmute(salary = annual_salary,
            age = age,
            field_experience = field_experience,
            education = highest_level_of_education_completed,
            industry = fct_lump_n(industry, 10),
            gender = fct_lump_n(gender, 2),
            london = fct_lump_n(city, 1, other_level = "Outside London")) %>%
  drop_na(education, gender, industry) %>%
  filter(age < 65)

# EDA
cat_vars = uk_lumped %>%
  pivot_longer(education:london) %>%
  mutate(value = fct_reorder(value, salary)) %>%
  group_by(value, name) %>%
  mutate(avg_sal = median(salary, na.rm = T)) %>%
  ggplot(aes(x = salary, y = value)) +
  ggdist::stat_pointinterval(aes(color = avg_sal), show.legend = F) +
  facet_wrap(~str_to_title(name), scales = "free_y") +
  scale_x_log10(labels = scales::label_dollar(prefix = "£")) +
  scale_color_gradient(trans = "log10") +
  labs(x = "Annual Salary", y = NULL)

num_vars = uk_lumped %>%
  pivot_longer(age:field_experience) %>%
  group_by(value, name) %>%
  mutate(avg_sal = median(salary, na.rm = T),
         name = str_replace(name, "_", " "),
         name = paste("Min Years of", name)) %>%
  ggplot(aes(x = salary, y = value)) +
  ggdist::stat_pointinterval(aes(color = avg_sal), orientation = "horizontal", show.legend = F) +
  facet_wrap(~str_to_title(name), scales = "free_y", ncol = 1) +
  scale_x_log10(labels = scales::label_dollar(prefix = "£")) +
  scale_color_gradient(trans = "log10") +
  labs(x = "Annual Salary", y = NULL) +
  theme(strip.placement = "outside")

cowplot::plot_grid(
  num_vars + labs(title = "Numeric Variables"), 
  cat_vars + labs(title = "Categorical Variables"), 
  axis = "tb", align = "hv", nrow = 1, rel_widths = c(.35, 1)
)

# Fit UK Model ------------------------------------------------------------

nice_names = tribble(
  ~term, ~nice_term,
  "london", "Inside London?",
  "industry", "Industry",
  "field_experience", "Years of Experience\nin Field",
  "education", "Highest Level of\nEducation",
  "gender", "Gender"
)

uk_mod = lm(formula = log(salary) ~ industry + london + education + gender + field_experience, data = uk_lumped)

uk_mod %>% summary()

uk_mod_anova = uk_mod %>%
  anova() %>%
  broom::tidy() %>%
  mutate(perc = sumsq / sum(sumsq)) 

uk_mod_anova %>%
  filter(term != "Residuals") %>%
  left_join(nice_names, by = "term") %>%
  mutate(nice_term = fct_reorder(nice_term, perc)) %>%
  ggplot(aes(y = nice_term, x = perc)) +
  geom_col(aes(fill = perc), show.legend = F) +
  scale_x_continuous(expand = expansion(mult = c(0,.1)), labels = scales::label_percent()) +
  theme_bw() +
  labs(y = NULL, x = "Variance Explained")

uk_lumped %>%
  nest_by(london) %>%
  mutate(mod = list(lm(formula = log(salary) ~ industry + education + gender + field_experience, data = data)),
         anova = list(anova(mod)),
         tidy_anova = list(broom::tidy(anova))) %>%
  unnest(tidy_anova) %>%
  mutate(perc_var = sumsq / sum(sumsq)) %>%
  drop_na() %>%
  ungroup() %>%
  left_join(nice_names, by = "term") %>%
  rename(old_term = term,
         term = nice_term) %>%
  mutate(term = tidytext::reorder_within(term, perc_var, london)) %>%
  ggplot(aes(x = perc_var, y = term)) +
  geom_col(aes(fill = perc_var), show.legend = F) + 
  tidytext::scale_y_reordered() +
  facet_wrap(~london, scales = "free_y") +
  scale_x_continuous(labels = scales::label_percent()) +
  labs(x = "Percentage Variation Explained", y = NULL)
