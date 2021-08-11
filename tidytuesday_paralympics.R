library("tidyverse")
library("countrycode")
library(scales)


athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')


# number of events by types sports in each year----------------

total_events <- athletes %>% 
  #filter(type == "Athletics") %>% 
  #filter(str_detect(event,"100 m | Triple Jump))%>%
  group_by(event, type, year) %>% 
  summarise(tot_events = length(event)) %>% 
  arrange(-tot_events)

total_events


unique(total_events$event)




# number of type sports in each year------------

athletes$type <- as.factor(athletes$type)

number_type <- athletes %>% 
  group_by(type, year) %>% 
  summarise(n_type = length(type)) %>% 
  arrange(-n_type) 


ggplot(number_type, aes(x = type, y = n_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year, ncol = 2, nrow = 5) +
  theme(axis.text.y = element_text(size = 8)) +
  coord_flip()


# distribution events ----------------

dist <- athletes %>% 
  group_by(year, type, event, gender) %>% 
  count(event) %>% 
  rename("Appearances" = n) %>% 
  arrange(-Appearances)

dist

dist %>% 
  drop_na(gender) %>% 
ggplot(aes(x = factor(year), y = Appearances, fill = factor(gender))) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ type, ncol = 3, nrow = 4)


#  top-scoring athletes -----------------


athlete_games_counts <- athletes %>%
  drop_na(athlete) %>% 
  distinct(athlete, year, gender, .keep_all = T)

athlete_games_counts

dat_text <- data.frame(
  label = c("Buggenhagen"),
  gender   = "Women",
  x     = c(7),
  y     = 1e+01
)
  




athlete_games_counts %>% 
  drop_na(gender) %>% 
  count(athlete, gender, sort = T)  %>% 
  ggplot(aes(factor(n))) +
  geom_bar(fill = "#646ECA") +
  scale_y_log10(label = scientific_format())+
  facet_grid(gender ~ .) +
  labs(title = "Distribution of athletes in regard to their number of appearances", 
       x = "Number of Olympic Games attended", 
       y = "Number of athletes") +
  theme_classic() +
  theme(text = element_text(family = "Cabin"),
        plot.title = element_text(
          face = "bold",
          hjust = 0),
        axis.title = element_text(
          face = "bold",
          size = rel(1)),
        axis.text = element_text(
          face = "bold",
          size = rel(0.85))) +
  geom_text(size = c(4) , family = c("arial"),
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label)
  ) 




# ------ how many do only appear on one Season? ------


athlete_games_counts %>% 
  count(athlete, sort = T) %>% 
  mutate(one_timer = n < 2 ) %>% 
  summarise(one_timer_frac = mean(one_timer))


# ------------- WHO APPEARED MOST OFTEN AND IN WHAT DISCIPLINE?-----------------------

# ---- top scoring female-----------------

female_most_experienced <- athlete_games_counts %>% 
  filter(gender == "Women") %>% 
  count(athlete, sort = T) %>% 
  head(10)

female_most_experienced %>% 
  left_join(athlete_games_counts) %>% 
  select(athlete, n, type, abb) %>% 
  rename("Olmpic_Games" = n) %>% 
  distinct()


# ----- top scoring male--------------

male_most_experienced <- athlete_games_counts %>% 
  filter(gender == "Men") %>% 
  count(athlete, sort = T) %>% 
  head(10)

male_most_experienced %>% 
  left_join(athlete_games_counts) %>% 
  select(athlete, n, type, abb) %>% 
  rename("Olmpic_Games" = n) %>% 
  distinct()


# ------- top scoring mixed-------------

mixed_most_experienced <- athlete_games_counts %>% 
  filter(gender == "Mixed") %>% 
  count(athlete, sort = T) %>% 
  head(100)

mixed_most_experienced %>% 
  left_join(athlete_games_counts) %>% 
  select(athlete, n, type, abb) %>% 
  rename("Olmpic_Games" = n) %>% 
  distinct()

# -------- WHO WON THE MOST MEDALS IN ANY SPORT? --------------

medals_by_sport <- athlete_games_counts %>% 
  filter(!is.na(medal)) %>% 
  group_by(type) %>% 
  count(medal)


# top women's by medal score ---------------------

athlete_games_counts <- athletes %>%
  drop_na(athlete) %>% 
  distinct(athlete, year, event, gender, .keep_all = T)


athlete_games_counts %>% 
  filter(gender == "Women", !is.na(medal)) %>% 
  group_by(athlete, type) %>% 
  count(medal) %>% 
  ungroup() %>% 
  pivot_wider(names_from = medal, values_from = n, values_fill = 0) %>%
  mutate(total = Bronze + Silver + Gold,
         medal_score = 3 * Gold + 2 * Silver + 1 * Bronze) %>% 
  arrange(desc(medal_score), desc(total)) %>% 
  select(athlete, Bronze, Silver, Gold, total, medal_score, type) %>% 
  head(10) %>% 
  paged_table()


# top men's by medal score --------------------------------

athlete_games_counts <- athletes %>%
  drop_na(athlete) %>% 
  distinct(athlete, year, event, gender, .keep_all = T)


athlete_games_counts %>% 
  filter(gender == "Men", !is.na(medal)) %>% 
  group_by(athlete, type) %>% 
  count(medal) %>% 
  ungroup() %>% 
  pivot_wider(names_from = medal, values_from = n, values_fill = 0) %>%
  mutate(total = Bronze + Silver + Gold,
         medal_score = 3 * Gold + 2 * Silver + 1 * Bronze) %>% 
  arrange(desc(medal_score), desc(total)) %>% 
  select(athlete, Bronze, Silver, Gold, total, medal_score, type) %>% 
  head(10) %>% 
  paged_table()



# -------------------- Top Scoring NOCs ------------


noc_medal_count <- athletes %>% 
  group_by(abb) %>% 
  count(medal, sort = T) %>% 
  pivot_wider(names_from = medal, values_from = n, values_fill = 0) %>% 
  mutate(total = Bronze + Silver + Gold,
         medal_score = 3 * Gold + 2 * Silver + 1 * Bronze) %>% 
  arrange(desc(medal_score), desc(total)) %>% 
  relocate(Gold, .after = Silver) %>% 
  paged_table()

noc_medal_count



# medal count by the number of Games each NOC appeared ----------------


noc_game_count <- athletes %>% 
  distinct(abb, year, .keep_all = T) %>% 
  count(abb) %>% 
  rename(n_games = n) 
noc_game_count

medal_count_by_NOC_appeared <- noc_medal_count %>% 
  left_join(noc_game_count) %>% 
  mutate(Gold = Gold / n_games,
         Silver = Silver / n_games,
         Bronze = Bronze / n_games,
         total = total / n_games,
         medal_score = medal_score / n_games) %>% 
  arrange(desc(medal_score), desc(total)) 

medal_count_by_NOC_appeared


# Does money buy medals?

athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')


library(readxl)
library("countrycode")

data_gapminder <- read_excel("GM-GDP per capita - Dataset - v27.xlsx", sheet = "data-GDP-per-capita-in-columns")




data_gapminder$IOC <- countrycode(sourcevar = data_gapminder$country,
                                  origin = "country.name", 
                                  destination = "ioc")



data_gapminder$Continent <- countrycode(sourcevar = data_gapminder$country,
                                  origin = "country.name", 
                                  destination = "continent")


pop <- read_excel("population_total.xlsx")

pop$IOC <- countrycode(sourcevar = pop$country,
                                  origin = "country.name", 
                                  destination = "ioc")


glimpse(pop)

pop$IOC <- as.numeric(pop$IOC)

pop <- pop %>%
  mutate_at(vars(c(`1799`:`2100`)), as.numeric)
  



OG_years <- athletes %>% 
  distinct(year) %>% 
  pull()





data_gapminder <- data_gapminder %>% 
  mutate_at(vars(c(`1884`:`2050`)), as.numeric)
glimpse(data_gapminder)


gapminder_long <- data_gapminder %>% 
  pivot_longer(-c(country, IOC, Continent), 
               names_to = "year", 
               values_to = "GDPpc", 
               names_transform = list(year = as.integer)) %>% 
  filter(year %in% OG_years)



pop_long <- pop %>% 
  pivot_longer(-c(country, IOC),
               names_to = "year", 
               values_to = "population",
               names_transform = list(year = as.integer)) %>% 
  filter(year %in% OG_years)

pop_long$IOC <- countrycode(sourcevar = pop_long$country,
                       origin = "country.name", 
                       destination = "ioc")




athletes <-  athletes %>% 
  add_column(ID = 1:19547)


athlete_counts <- athletes %>% 
  distinct(country, abb, year, ID, .keep_all = T) %>% 
  group_by(abb, year) %>% 
  summarise(ath_count = n())

gap_medal_counts <- athletes %>% 
  filter(!is.na(medal)) %>% 
  group_by(abb, year) %>% 
  summarise(med_count = n())

gap_med_ath <- gapminder_long %>% 
  inner_join(athlete_counts,
             by = c("IOC" = "abb", "year" = "year"),
             suffix = c("_gap", "_ath")) %>% 
  inner_join(gap_medal_counts,
             by = c("IOC" = "abb", "year" = "year"),
             suffix = c("_gap", "_ath")) %>% 
  inner_join(pop_long,
             by = c("year", "country")) %>% 
  mutate(ath_frac = ath_count / population) %>% 
  rename(Medals = med_count)

gap_med_ath <- gap_med_ath %>% 
  select(-IOC.y) %>% 
  rename(IOC = IOC.x)



# correlation analysis ----------------------------

#gap_med_ath <- gap_med_ath %>% 
  #filter(Continent == "Africa")

cor_dat <- gap_med_ath %>% 
  filter(year == 2016) %>% 
  select(-c(country, Continent, year, IOC))


plot(cor_dat)

cor_dat %>% 
  cor(method = "spearman")

cor.test(cor_dat$GDPpc, cor_dat$ath_frac, method = "spearman")




library(ggrepel)


set.seed(75881)
global <- gap_med_ath %>% 
  filter(year == 2016) %>% 
  ggplot(aes(GDPpc, ath_frac, color = Continent, group = Continent)) +
  #facet_wrap(~Continent, ncol = 1, nrow = 5) +
  geom_jitter(alpha = 0.4) +
  geom_text_repel(aes(label = IOC), size = 4, hjust = 0, vjust = 0, position = "dodge") +
  scale_color_manual(values=c("purple", "red", "yellow", "green", "blue")) +
  #stat_smooth(method = "lm", se = F) +
  #stat_smooth(method = "lm", se = F, group = 1, color = "white", alpha = 0.2) +
  scale_x_continuous()+
  scale_y_log10() +
  # coord_trans(x= "log10", y = "log10") +
  labs(title = "Paralympic Games. GDP per capita vs athletes 'per capita' in 2016", 
       x = "GDP per capita", 
       y = "Fraction of athletes per population (log10 scale)",
       caption = "IPC & gapminder.org | #Tidytuesday W32 | @dataR_amateur") +
  theme(text = element_text(size=12, face = "bold.italic"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="blue", size=10, 
                                   face="bold"),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue"),
        legend.position = "top") +
  guides(color = F)

global

global + ggsave("paralympics_.pdf", width = 13, height = 8.5, dpi = 500)


africa <- gap_med_ath %>% 
filter(Continent == "Africa")

cor_dat <- gap_med_ath %>% 
  filter(year == 2016) %>% 
  select(-c(country, Continent, year, IOC))


plot(cor_dat)

cor_dat %>% 
  cor(method = "spearman")

cor.test(cor_dat$GDPpc, cor_dat$ath_frac, method = "spearman")



plot_africa <- africa %>% 
  filter(year == 2016) %>% 
  ggplot(aes(GDPpc, ath_frac, color = Continent, group = Continent)) +
  #facet_wrap(~Continent, ncol = 1, nrow = 5) +
  geom_point(alpha = 0.4) +
  geom_text_repel(aes(label = country), size = 4, hjust = 0, vjust = 0, position = "dodge",) +
  scale_color_manual(values=c("purple", "blue", "yellow", "green", "red")) +
  stat_smooth(method = "lm", se = F) +
  stat_smooth(method = "lm", se = F, group = 1, color = "white", alpha = 0.2) +
  scale_x_continuous()+
  scale_y_log10() +
  # coord_trans(x= "log10", y = "log10") +
  labs(title = "GDP per capita vs athletes 'per capita' in 2016", 
       x = "GDP per capita", 
       y = "Fraction of athletes per population (log10 scale)",
       caption = "") +
  theme(text = element_text(size=10, face = "bold.italic"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold.italic"),
        panel.grid = element_line(color = "black"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 8.5, hjust = 1),
        axis.title = element_text(color = "white"),
        legend.text = element_text(colour="blue", size=10, 
                                   face="bold"),
        legend.background = element_rect(fill="black",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue"),
        legend.position = "top") +
  guides(color = F)
plot_africa
