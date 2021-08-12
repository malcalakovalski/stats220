## ---- read-time-use
library(readxl)
library(tidyverse)
time_use_raw <- read_xlsx("data/time-use-oecd.xlsx")
time_use_raw

## ---- gg-time-use
ggplot(time_use_raw) + #<<
  geom_col(aes(
    Country, `Time (minutes)`, 
    fill = Category))

## ---- gg-time-use-pipe
time_use_raw %>% #<<
  ggplot() +
  geom_col(aes(
    Country, `Time (minutes)`, 
    fill = Category))

## ---- rename
time_use <- time_use_raw %>% 
  rename( # new_name = old_name
    country = Country,
    category = Category, 
    minutes = `Time (minutes)`)
time_use

## ---- distinct
time_use %>% 
  distinct(category)

## ---- distinct2
time_use %>% 
  distinct(country, category)

## ---- slice
time_use %>% 
  slice((n() - 4):n())

## ---- group-by
time_use %>% 
  group_by(country)

## ---- slice-gb
time_use %>% 
  group_by(country) %>% 
  slice((n() - 4):n())

## ---- slice-head
time_use %>% 
  group_by(country) %>% 
  slice_tail(n = 5)

## ---- ungroup
time_use %>% 
  group_by(country) %>% 
  slice_tail(n = 5) %>% 
  ungroup()

## ---- slice-sample
set.seed(220) # an arbitrary number
time_use %>% 
  slice_sample(n = 10)

## ---- arrange
time_use %>% 
  arrange(minutes)

## ---- arrange-desc
time_use %>% 
  arrange(desc(minutes)) # -minutes

## ---- arrange-desc2
time_use %>% 
  arrange(country, desc(minutes))

## ---- filter
time_use %>% 
  filter(minutes == max(minutes))

## ---- filter-gb
time_use %>% 
  group_by(country) %>% 
  filter(minutes == max(minutes))

## ---- filter-in
abr <- c("Argentina", "Brazil")
time_use %>% 
  filter(country %in% abr)

## ---- filter-not
time_use %>% 
  filter(!(country %in% abr)) # ! logical negation (NOT)

## ---- filter-and
time_use %>% 
  filter(country %in% abr, minutes > 30)
time_use %>% 
  filter(country %in% abr & minutes > 30)

## ---- filter-or
time_use %>% 
  filter(country %in% abr | minutes > 30)

## ---- logical-op
x <- c(TRUE, FALSE, TRUE, FALSE)
y <- c(FALSE, TRUE, TRUE, FALSE)

## ---- logical-op-el
x & y
x | y

## ---- logical-op-first
x && y
x || y

## ---- filter-abr
time_use_abr <- time_use %>% 
  filter(country %in% abr)

## ---- filter-abr-line
time_use_nz <- time_use_abr %>% 
  filter(country == "Brazil")
time_use_abr %>% 
  ggplot(aes(as_factor(country), minutes, group = category)) +
  geom_line(aes(colour = category)) +
  geom_point(aes(colour = category)) +
  ggrepel::geom_text_repel(aes(label = category), data = time_use_nz,
    size = 3) +
  theme(legend.position = "none") +
  scale_y_log10()

## ---- select
time_use_abr %>% 
  select(country, category)
time_use_abr %>% 
  select(-minutes) # !minutes
time_use_abr %>% 
  select(country:category)

## ---- select-helpers
time_use_abr %>% 
  select(starts_with("c"))

## ---- relocate
time_use_abr %>% 
  relocate(minutes)

time_use_abr %>% 
  relocate(minutes)

time_use_abr %>% 
  relocate(minutes, .after = country)

## ---- mutate
time_use_abr2 <- time_use_abr %>%
  mutate(
    # new_column = f(existing_column)
    hours = minutes / 60,
    iso = case_when(
      country == "Australia" ~ "AU", 
      TRUE ~ "NZ"))
time_use_abr2

## ---- case-when
z <- 1:10
case_when(
  # LHS ~ RHS
  # logical cond ~ replacement val
  z < 5 ~ "less than 5",
  z > 5 ~ "greater than 5",
  TRUE ~ "equal to 5"
)

## ---- summarise
time_use_abr2 %>% 
  summarise( # summarize()
    min = min(hours), 
    max = max(hours),
    avg = mean(hours))

## ---- summarise-gb
time_use_abr2 %>% 
  group_by(category) %>% 
  summarise(
    min = min(hours), 
    max = max(hours),
    avg = mean(hours))

## ---- chain
time_use %>% 
  filter(country %in% abr) %>% 
  mutate(hours = minutes / 60) %>% 
  group_by(category) %>% 
  summarise(
    min = min(hours), 
    max = max(hours), 
    avg = mean(hours))

## ---- chain-ggplot
time_use %>% 
  filter(country %in% abr) %>% 
  mutate(hours = minutes / 60) %>% 
  group_by(category) %>% 
  summarise(min = min(hours), max = max(hours), avg = mean(hours)) %>% 
  ggplot(aes(x = category, y = avg)) +
  geom_pointrange(aes(ymin = min, ymax = max), colour = "#e6550d") +
  theme(axis.text.x = element_text(angle = 90))

## ---- shortcuts
time_use_abr %>% 
  group_by(country) %>% 
  summarise(n = n())
time_use_abr %>% 
  count(country)
time_use_abr %>% 
  group_by(country) %>% 
  tally()


## ---- countrycode
(country_code <- read_csv("data/countrycode.csv"))

## ---- countrycode2
country_code %>% 
  filter(country_name %in% 
    c("Brazil", "United States"))

## ---- time-use-2
time_use %>% 
  filter(country %in% c("Brazil", "USA")) %>% 
  distinct(country, .keep_all = TRUE)

## ---- inner-join
time_use %>% 
  inner_join(country_code, by = c("country" = "country_name"))

## ---- left-join
time_use %>% 
  left_join(country_code, by = c("country" = "country_name"))

## ---- left-join-na
time_use %>% 
  left_join(country_code, by = c("country" = "country_name"))%>%
  filter(country %in% c("Brazil", "USA")) %>% 
  group_by(country) %>% 
  slice_head()

## ---- right-join
time_use %>% 
  right_join(country_code, by = c("country" = "country_name"))

## ---- full-join
time_use %>% 
  full_join(country_code, by = c("country" = "country_name"))

## ---- semi-join
time_use %>% 
  semi_join(country_code, by = c("country" = "country_name"))

## ---- anti-join
time_use %>% 
  anti_join(country_code, by = c("country" = "country_name"))

## ---- lookup
time_use_ctr <- unique(time_use$country)
country_code_name <- unique(country_code$country_name)

time_use_ctr[!time_use_ctr %in% country_code_name]
