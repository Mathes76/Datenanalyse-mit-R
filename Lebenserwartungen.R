library(dplyr)
library(dslabs)
library(ggplot2)
data(gapminder)


# herausfiltern der Daten von Deutschland, USA und China fuer die Jahre kleiner/gleich 2015
ger_us_ch <- gapminder %>%
  filter(country %in% c('Germany', 'United States', 'China'), year <= 2015) %>%
  mutate(Lebenserwartung = life_expectancy) %>% 
  mutate(Jahr = year)

ger_us_ch


# Graph zu den Lebenserwartungen 
ger_us_ch_plot <- ger_us_ch %>%
  ggplot(aes(Jahr, Lebenserwartung, color = country)) +
  geom_line()

ger_us_ch_plot


# Zusammenfassung der Lebenserwartungen fuer Deutschland, USA und China zwischen 1960 und 2015:
# geringste, 1. Quantil, Median, Durchschnitt, 3. Qunatil, hoechste
summary(ger_us_ch$life_expectancy)


# Lebenserwartung europaeischer Laender mit mehr als 20Mio Einwohnern in den Jahren 1960 bis 2015
# in Fuenfjahresschritten
europe <- gapminder %>%
  filter(year %in% c(1960,1965,1970, 1975,1980,1985,1990,1995,2000,2005,2010,2015),
         continent == 'Europe',
         fertility <= 3,
         population >= 20000000) %>%
  select(country, life_expectancy, population, year)

europe


# Graph zu Lebenserwartung (s.o.)
europe_plot <- europe %>% 
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_point()

europe_plot

# Zusammenfassung der Lebenserwartungen fuer Europa > 20Mio Einwohnern im Jahr 1905 bis 2015:
# geringste, 1. Quantil, Median, Durchschnitt, 3. Qunatil, hoechste
summary(europe$life_expectancy)


# Graph ueber Anstieg der Lebenserwartungen in Deutschland von 1960 - 2015
germany <-
  gapminder %>% filter(country == 'Germany', year <= 2015) %>%
  ggplot(aes(year, life_expectancy, color = country)) + geom_line()

germany


# Durchschnittsgehaelter in Euro 2010 in Europa (EUR/Dollar Kurs entspricht 2010er Jahresdurchschnitt)
euro_monat <- gapminder %>%
  filter(year == 2010, continent == 'Europe',!is.na(gdp)) %>%
  mutate(dollars_per_month = gdp / population / 12) %>%
  mutate(euros_per_month = dollars_per_month / 1.33)

euro_monat


# Dichtediagramm der Durchschnittsgehaelter in Europa im Jahr 2010
euro_monat %>%
  ggplot(aes(euros_per_month)) +
  scale_x_continuous(trans = 'log2') +
  geom_density()


# Durchschnittliches Monatsgehalt in Europa in den Jahren 1970,1990,2010 (in Dollar)
monatsdollar <- gapminder %>%
  filter(year %in% c(1970, 1990, 2010), continent == 'Europe',!is.na(gdp)) %>%
  mutate(dollars_per_month = gdp / population / 12)


# Dichtediagramm zu oben  
monatsdollar %>%
  ggplot(aes(dollars_per_month, fill = region)) +
  scale_x_continuous(trans = 'log2') +
  geom_density(bw = 0.5, position = 'stack') +
  facet_grid(. ~ year)

# Lebenserwartung in Abhaengigkeit vom taeglichen Einkommen
gapminder_Europe_2010 <- gapminder %>%
  filter(
    year %in% c(1970, 1990, 2010),
    continent == 'Europe',!is.na(gdp),!is.na(infant_mortality)
  ) %>%
  mutate(taegliches_Einkommen = gdp / population / 365) %>% 
  mutate(Lebenserwatung = life_expectancy)

gapminder_Europe_2010

# Graph zur Lebenserwartung/taegliches Einkommen
gapminder_Europe_2010 %>%
  ggplot(aes(
    taegliches_Einkommen,
    Lebenserwatung,
    color = region,
    label = country
  )) +
  scale_x_continuous(trans = 'log2') +
  geom_point() +
  geom_text() +
  facet_grid(. ~ year)

 
# Anstieg der Weltpopulation von 1960 bis 2015
rise_population <- aggregate(population ~ year, data = gapminder,sum)
rise_population %>% 
  ggplot(aes(year, population/1000000000)) +
  geom_line()

# Zusmmenfassung der Bevoelkerung von 1960 bis 2015
summary(rise_population)
    