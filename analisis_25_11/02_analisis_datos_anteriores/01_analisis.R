setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readxl)
library(janitor)
library(dplyr)
library(psych)
library(tidyr)
library(ggplot2)
library(stringr)

mar_original <- par("mar")

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
  filter(if_all(42:74, ~ !is.na(.))) 



names(datos)[42:74] <- nombres_cortos

datos %>% 
  dplyr::select(42:74) %>% 
  summary

datos %>% 
  dplyr::select(42:74) %>% 
  str


datos <- datos %>% mutate(across(42:74, ~as.numeric(.)))

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


# clusterización jerárquica ---------------


library(dendextend)

poly <- polychoric(datos %>% dplyr::select(42:74) %>% data.frame %>% setNames(str_replace_all(names(.),'_',' ')))

r <- poly$rho

distancia <- as.dist(1-r)

hc <- hclust(distancia, method = 'ward.D2')


dendograma <- as.dendrogram(hc)

dendograma <- dendograma %>% 
  color_branches(k = 15) %>% 
  color_labels (k = 15)


par(mar = c(1,1,1,30))
plot(dendograma, horiz = T, axes = F)


rect.dendrogram(
  dendograma,
  k = 7,
  horiz = T,
  border = 'black',xpd = T
  
)


clusters <- cutree(hc, k = 7)

info_clusters <- data.frame(
  variable = names(clusters),
  cluster = clusters %>% unname
)

# evaluación de información aportada --------------------

library(mirt)
library(purrr)

modelo <- mirt(datos %>% dplyr::select(42:74) %>% data.frame %>% setNames(str_replace_all(names(.),'_',' ')), model = 1, itemtype = 'graded',
               technical = list(
                 NCYCLES = 3000  
       
               ))

datos$score <- fscores(modelo, method = 'EAP') %>% as.vector

mean(datos$score)

ggplot(datos, aes(x = score)) +
  geom_histogram() + 
  geom_density()


plot(modelo, type = 'info')


informacion <- list()

for(i in 1:33){
  
  item <- extract.item(modelo, i)
  theta_grid <- matrix(seq(-2, 2, length.out = 81), ncol = 1)
  informacion[[i]] <- mirt::iteminfo(item, Theta = theta_grid) 
  informacion[[i]] <- mean(informacion[[i]])
  
}

resumen_info <- data.frame(
  variable = datos %>% dplyr::select(42:74) %>% data.frame %>% setNames(str_replace_all(names(.),'_',' ')) %>% names,
  informacion_promedio = reduce(informacion,c)
) %>% 
  mutate(
    informacion_proporcion = informacion_promedio / sum(informacion_promedio)
  ) %>% 
  arrange(desc(informacion_proporcion)) %>% 
  mutate(
    variable = factor(variable, levels = unique(variable))
  )


ggplot(resumen_info, aes(x = variable, y = informacion_proporcion)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


plot(modelo,type = 'info')
plot(modelo,type = 'rxx')
# plot(modelo,type = 'infocontour')
plot(modelo,type = 'SE')
plot(modelo,type = 'infotrace')
# plot(modelo,type = 'infoSE')
plot(modelo,type = 'trace')
# plot(modelo,type = 'intemscore')
plot(modelo,type = 'score')
# plot(modelo,type = 'scorecontour')
plot(modelo,type = 'posteriorTheta')
# plot(modelo,type = 'EPAsum')


# información por item --------------------

library(ggrepel)

grid <- matrix(seq(-2, 2, length.out = 81), ncol = 1)

informacion <- list()

for(i in 1:33){
  
  item <- extract.item(modelo, i)
  theta_grid <- matrix(seq(-2, 2, length.out = 81), ncol = 1)
  informacion[[i]] <- mirt::iteminfo(item, Theta = theta_grid) 
  
}

informacion_items_2 <- bind_cols(informacion)

names(informacion_items_2) <- nombres_cortos

info_total <- colSums(informacion_items_2)

centro_info <- colSums(informacion_items_2 * grid) / info_total

centro_info <- data.frame(
  variable = names(centro_info),
  centro_info
)

ggplot(centro_info, aes(x = 1, y = centro_info, label = variable)) +
  geom_point()+
  geom_label_repel(max.overlaps = Inf) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# GRÁFICA FINAL

resumen_info <- resumen_info %>% 
  mutate(variable = as.character(variable))

centro_info <- centro_info %>% 
  arrange(variable)

resumen_info <- resumen_info %>% 
  arrange(variable)

info_clusters <- info_clusters %>% 
  arrange(variable)

resumen_final <- bind_cols(
  centro_info,
  resumen_info,
  info_clusters
) %>% 
  clean_names %>% 
  mutate(cluster = as.factor(cluster))
  


ggplot(resumen_final, aes(x = centro_info, y = informacion_proporcion, label = variable_1, color = cluster)) +
  geom_point() +
  geom_label_repel(max.overlaps = Inf) +
  theme_minimal() +
  labs(
    x = 'centro de información',
    y = 'proporción de información aportada'
  )


