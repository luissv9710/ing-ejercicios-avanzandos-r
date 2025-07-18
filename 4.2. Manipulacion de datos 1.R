# U. Internacional de Aguascalientes
# Doctorado en Tecnologías de la Transformación Digital
# Materia:  Ingeniería para el procesamiento masivo de datos
# Tutor:    Dr. Jonás Velasco Álvarez
# Alumno:   Luis Alejandro Santana Valadez

# Trabajo:  Ejercicios extraidos del PDF: <<< 4.2. Manipulacion de datos 1.pdf >>>
# Objetivo: Practicar el lenguaje R y sus funciones avanzadas


# Uso de Tibbles con tibble
# ------------------------------

tibble(x = 1:5, y = 1, z = x ^ 2 + y)

as_tibble(iris)
tb <- tibble(`:)` = "smile", ` ` = "space", `2000` = "number")
tb
tribble(
  ~x, ~y, ~z,
  "a", 2, 3.6,
  "b", 1, 8.5
)
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = 1:1000
)

print(nycflights13::flights, n = 10, width = Inf)
df <- tibble(x = runif(5), y = rnorm(5))
df$x
df[["x"]]
df[[1]]
df %>% .$x
as.data.frame(tb)




# Importación de datos con readr
# -----------------------------------

read_csv("a,b,c\n1,2,3\n4,5,6")
read_csv("1,2,3\n4,5,6", col_names = FALSE) read_csv("a,b,c\n1,2,.", na = ".")

parse_integer(c("1", "231", ".", "456"), na = ".") parse_number("$123,456,789")
parse_character("El Ni\xf1o", locale = locale(encoding = "Latin1"))
parse_factor(c("apple", "banana", "bananana"), levels = c("apple", "banana"))
parse_date("01/02/15", "%m/%d/%y") parse_time("11:15:10.12 PM")

read_csv(readr_example("challenge.csv"), col_types = cols(x = col_double(), y = col_date()))
type_convert(tribble(~x, ~y, "1", "1.21", "2", "2.32"))

write_csv(challenge, "challenge.csv") write_rds(challenge, "challenge.rds")



# Datos ordenados con tidyr
# ---------------------------------

table1 %>% 
  mutate(rate = cases / population * 10000) table1 %>% count(year, wt = cases) 
ggplot(table1, aes(year, cases)) + geom_line(aes(group = country)) + geom_point(aes(color = country))

table4a %>% 
  gather(1999, 2000, key = "year", value = "cases") 
table4b %>% 
  gather(1999, 2000, key = "year", value = "population") 
spread(table2, key = type, value = count)
table3 %>% 
  separate(rate, into = c("cases", "population")) 
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")
people <- tribble(~name, ~key, ~value, "Phillip Woods", "age", 45, "Phillip Woods", "height", 186, "Phillip Woods", "age", 50) 
preg <- tribble(~pregnant, ~male, ~female, "yes", NA, 10, "no", 20, 12)


# Combinación de datos con dplyr
# ---------------------------------------

left_join(y, z)
inner_join(x, y) full_join(x, y)
left_join(x, y, by = c("a" = "b")) left_join(x, y, by = c("key1", "key2"))
semi_join(flights, top_dest) anti_join(flights, top_dest)
intersect(df1, df2) union(df1, df2) setdiff(df1, df2)





















