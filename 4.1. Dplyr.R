# U. Internacional de Aguascalientes
# Doctorado en Tecnologías de la Transformación Digital
# Materia:  Ingeniería para el procesamiento masivo de datos
# Tutor:    Dr. Jonás Velasco Álvarez
# Alumno:   Luis Alejandro Santana Valadez

# Trabajo:  Ejercicios extraidos del PDF: <<< 4.1. Dplyr.pdf >>>
# Objetivo: Practicar el lenguaje R y sus funciones avanzadas



# CHAPTER 3. Data Transformation with dplyr
# ---------------------------------------------


# 1. Instalación de paquetes (PASO CERO)
# -----------------------------------------

install.packages("dplyr")
install.packages("nycflights13")
install.packages("tidyverse")

install.packages("ggplot2")
install.packages("Lahman")



# 2. Carga de paquetes 
# -----------------------------------------

library(dplyr)
library(nycflights13)
library(ggplot2)
library(Lahman)


# 3. Ejercicios con R
# -----------------------------------------

names(flights)
nycflights13::flights


# filter() --> operaciones lógicas y condicionales
# -----------------------------------------

jan1 <- filter(flights, month == 1, day == 1)
print(jan1)
(dec25 <- filter(flights, month == 12, day == 25))
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))
print(nov_dec)

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)


# Comparaciones
# -----------------------------------------

sqrt(2) ^ 2 == 2
#> [1] FALSE
1/49 * 49 == 1
#> [1] FALSE

near(sqrt(2) ^ 2, 2)
#> [1] TRUE
near(1 / 49 * 49, 1)
#> [1] TRUE


# Valores perdidos
# -----------------------------------------

NA > 5
#> [1] NA
10 == NA
#> [1] NA
NA + 10
#> [1] NA

x <- NA
is.na(x)
#> [1] TRUE


df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
#> # A tibble: 1 × 1
#> x
#> <dbl>
#> 1 3
filter(df, is.na(x) | x > 1)
#> # A tibble: 2 × 1
#> x
#> <dbl>
#> 1 NA
#> 2 3


# arrange()
# -----------------------------------------
arrange(flights, year, month, day)

arrange(flights, desc(arr_delay))

df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))


# select()
# -----------------------------------------
# Select columns by name
select(flights, year, month, day)

# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

rename(flights, tail_num = tailnum)

select(flights, time_hour, air_time, everything())

select(flights, contains("TIME"))


# mutate)
# -----------------------------------------
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)



# Useful Creation Functions
# -----------------------------------------

# Offsets

(x <- 1:10)
#> [1] 1 2 3 4 5 6 7 8 9 10
lag(x)
#> [1] NA 1 2 3 4 5 6 7 8 9
lead(x)
#> [1] 2 3 4 5 6 7 8 9 10 NA



# Acumulación de sumas, medias
x
#> [1] 1 2 3 4 5 6 7 8 9 10
cumsum(x)
#> [1] 1 3 6 10 15 21 28 36 45 55
cummean(x)
#> [1] 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5

# Ranking
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
#> [1] 1 2 2 NA 4 5
min_rank(desc(y))
#> [1] 5 3 3 NA 2 1
#> row_number(y)
#> [1] 1 2 3 NA 4 5
dense_rank(y)
#> [1] 1 2 2 NA 3 4
percent_rank(y)
#> [1] 0.00 0.25 0.25 NA 0.75 1.00
cume_dist(y)
#> [1] 0.2 0.6 0.6 NA 0.8 1.0


# Grouped Summaries with summarize()
# -----------------------------------------

summarize(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))



by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")
# It looks like delays increase with distance up to ~750 miles
# and then decrease. Maybe as flights get longer there's more
# ability to make up delays in the air?
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)


delays <- flights %>%
  group_by(dest) %>%
  summarize(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")


not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))


# Conteos
# -----------------------------------------
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)



delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)



# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarize(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )
batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth(se = FALSE)


# Funciones de agrupación y resumen
# -----------------------------------------

# Measures of location
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    # average delay:
    avg_delay1 = mean(arr_delay),
    # average positive delay:
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )


# Measures of spread sd(x), IQR(x), mad(x)

# Why is distance to some destinations more variable
# than to others?
not_cancelled %>%
  group_by(dest) %>%
  summarize(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))


# Measures of rank min(x), quantile(x, 0.25), max(x)
# When do the first and last flights leave each day?
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    first = min(dep_time),
    last = max(dep_time)
  )


# Measures of position first(x), nth(x, 2), last(x)
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )


not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))


# Counts
# Which destinations have the most carriers?
not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))


# How many flights left before 5am? (these usually
# indicate delayed flights from the previous day)
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(n_early = sum(dep_time < 500))


# Grouping by Multiple Variables

daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))
(per_month <- summarize(per_day, flights = sum(flights)))
(per_year <- summarize(per_month, flights = sum(flights)))


# Ungrouping
daily %>%
  ungroup() %>% # no longer grouped by date
  summarize(flights = n()) # all flights


# Grouped Mutates (and Filters)
flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)
popular_dests


popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)












