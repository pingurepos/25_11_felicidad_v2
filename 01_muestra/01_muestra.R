library(readxl)
library(dplyr)
library(janitor)
library(BalancedSampling)
library(tidyr)

datos <- read_xlsx('../Plantilla_Aplicacion_Q3_2025_PILOTO_201125.xlsx')

datos <- datos %>% clean_names

head(datos)


names(datos)


bateria <- datos %>% 
  dplyr::select(starts_with('nvl')) %>% 
  dplyr::select(ends_with('id'))
  

bateria

bateria %>% 
  dplyr::select(1:7) %>% summary


bateria <- bateria %>% 
  mutate(
    across(everything(), ~replace_na(.,0))
  )




unique(bateria$nvl_08_id)





balanced_cube_sample <- function(data, aux_vars, n, pik = NULL) {
  # data: data.frame with population
  # aux_vars: names of balancing variables
  # n: desired sample size
  # pik: optional vector of inclusion probs; if NULL, equal probs
  
  X <- as.matrix(data[aux_vars])
  N <- nrow(data)
  
  if (is.null(pik)) {
    pik <- rep(n / N, N)
  } else {
    stopifnot(length(pik) == N)
  }
  
  s <- BalancedSampling::cube(prob = pik, x = X)
  data[s == 1, ]
}


muestra_balanceada <- balanced_cube_sample(
  data = bateria,
  aux_vars = paste0('nvl_0',1:8,'_id'),
  n = 3000
)




muestra <- datos[s,]


datos %>% 
  group_by(nvl_05_id) %>% 
  summarise(conteo = n()) %>% 
  mutate(porcentaje = conteo / sum(conteo))

datos %>% 
  group_by(nvl_05_id) %>% 
  summarise(conteo = n()) %>% 
  mutate(porcentaje = conteo / sum(conteo))



write.csv(muestra, 'muestra.csv')






