### Loading

require(tidyverse)
require(inmetr)
require(esquisse)


### buscando salvador

tbl <- bdmep_meta

ssa <- filter(bdmep_meta, uf == "BA")

id <- 83229


#### buscando os dados: 


start_date <- "01/01/1961"

end_date <- format(Sys.Date(), "%d/%m/%Y")

met_data <- bdmep_import(id = id,
                         sdate = start_date, 
                         edate = end_date, 
                         email = "tarssio.disap@hotmail.com",
                         passwd = "9mks3vfn",
                         verbose = TRUE)


###


ssa_data <- met_data %>% 
  select(date, prec, tair, tmax, tmin)


### 
require(lubridate)

ssa_data <- ssa_data %>% 
  mutate(date_ = floor_date(date, unit = "day"))

## group_by + summarise

ssa_data_x <- ssa_data %>% 
  group_by(date_) %>% 
  summarise_if(is.numeric, funs(mean(., na.rm = TRUE)))

## slice :

ssa_data_x <- ssa_data_x %>% 
  na.omit()


### um pouco sobre probabilidade: 

# install.packages("fitdistrplus")

require(fitdistrplus)


### Agrupando chuva por ano

ssa_prec <- ssa_data_x %>% 
  mutate(year = year(date_)) %>% 
  group_by(year) %>% 
  summarise(acc_prec = sum(prec))

# Criando modelos probabilisticos: Normal e Gamma

ssa_prec %>% 
  ggplot(aes(x = acc_prec)) + 
  geom_density() +
  theme_classic()

aux <- fitdist(ssa_prec$acc_prec, "norm")

summary(aux)

# Logo:  

qnorm(0.9, mean = 1877.4, sd = 527.9)

pnorm(2500, mean = 1877.4, sd = 527.9)


####Contando numero de dias com chuva

ssa_day <- ssa_data_x %>% 
  mutate(year = year(date_)) %>% 
  group_by(year) %>% 
  count(prec > 0 )

ssa_day %>% 
  ggplot(aes(x = n)) +
  geom_histogram(col = "black") +
  theme_classic()

### Distribuicao discreta: Poisson

aux2 <- fitdist(ssa_day$n, "pois")

summary(aux2)


###

qpois(0.5, 174.0)
