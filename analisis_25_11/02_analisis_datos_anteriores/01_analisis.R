setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readxl)
library(janitor)
library(dplyr)
library(psych)
library(tidyr)
library(ggplot2)


datos <- read_xlsx('../../respaldo_analisis_anterior/22_12_ipn_analisis_felicidad/Prueba piloto nueva medición Comprom15o 20_12.xls')

nombres_originales <- datos %>% 
  dplyr::select(42:74) %>% 
  names

datos <- datos %>% 
  clean_names

nombres_cortos <- c(
  'oportunidad de brillar',
  'he crecido en los ultimos 6 meses',
  'meritocracia',
  'evaluación clara para promociones',
  'buen feedback',
  'me siento valorado',
  'soy reconocido por mi buen trabajo',
  'me pagan lo justo',
  'mi trabajo interfiere con mi vida',
  'mi lider me apoya en temas personales',
  'se respetan mis tiempos de descanso',
  'los logros del equipo son importantes',
  'trabajo en una empresa lider',
  'siento orgullo por el lugar donde trabajo',
  'siento orgullo de trabajar con mi lider',
  'me han ayudado a conocer mis fortalezas',
  'tengo claras mis funciones',
  'cuento con lo necesario para trabajar',
  'capacitación',
  'tengo la confianza de aportar ideas',
  'mi lider confía en mí',
  'se respetan y toman en cuenta los puntos de vista',
  'mi equipo está comprometido con su trabajo',
  'hay personas en las que confío, inspiran y motivan',
  'confío en las decisiones de mi lider',
  'quiero trabajar muchos años aquí',
  'tengo seguridad laboral',
  'puedo expresear mi opinión',
  'mi lider me ha ayudado a mejorar',
  'tengo feedback una vez al año',
  'he tenido conversaciones con mi lider',
  'me siento bien con mis funciones',
  'pensar en ir a trabajar altera mi estado'
)


datos <- datos %>%  
  mutate(
    across(42:74, ~as.factor(.)),
    across(42:74, ~factor(., levels = c('Totalmente en desacuerdo','Muy en desacuerdo', 'En desacuerdo', 'De acuerdo', 'Muy de acuerdo', 'Totalmente de acuerdo'))),
    across(c(42+8,42+32), ~factor(., levels = c('Totalmente de acuerdo', 'Muy de acuerdo', 'De acuerdo', 'En desacuerdo', 'Muy en desacuerdo', 'Totalmente en desacuerdo')))
  ) %>% 
  mutate(
    across(everything(), ~ordered(.))
  ) %>% 
  filter(if_all(42:74, ~ !is.na(.))) %>% 
  mutate(across(42:74, ~as.numeric(.)))


names(datos)[42:74] <- nombres_cortos

datos %>% 
  dplyr::select(42:74) %>% 
  summary

datos %>% 
  dplyr::select(42:74) %>% 
  str

datos <- datos %>% 
  clean_names


# distribuciones ------------

resumen <- datos %>% 
  dplyr::select(42:74) %>% 
  mutate(id = 1:nrow(.)) %>% 
  pivot_longer(-id)


ggplot(resumen, aes(x = value, group = name, color = name)) +
  geom_density(show.legend = F) +
  theme_minimal()




poly <- polychoric(datos %>% dplyr::select(42:74) %>% data.frame)

r <- poly$rho

fa.parallel(
  r,
  n.obs = nrow(datos %>% 
                 dplyr::select(42:74) ),
  fm    = "ml",   # ML factor analysis
  fa    = "fa"
)


k<- 7


efa1 <- fa(
  r,
  nfactors = k,
  n.obs    = 1568,
  fm       = "ml",       # or "minres" if ML has issues
  rotate   = "oblimin"
)


fa.diagram(
  efa1,digits = 2,simple = F
)

