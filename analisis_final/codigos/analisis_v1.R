setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(readxl)
library(janitor)
library(dplyr)
library(psych)
library(tidyr)
library(ggplot2)
library(stringr)
library(dendextend)
library(mirt)
library(purrr)
library(ggrepel)
library(Ckmea)

datos <- read_xlsx('../datos/1D26D88A-6841-41DF-94A2-663A3D68CAB7.xls') %>% 
  clean_names() %>% 
  filter(!is.na(mi_lider_me_hace_sentir_valorado_como_persona_y_profesional))



head(datos)
names(datos)

nombres_originales <- names(datos)[44:98]

nombres_cortos <- c(
  'libertad de improvisar forma en que logro mis objetivos',
  'companeros comprometidos con realizar excelente trabajo',
  'pertenencia al mismo barco',
  'los deptos colaboran de manera efectiva',
  'me pagan adecuadamente',
  'beneficios especiales y unicos',
  'formador comunica claramente la direccion empresa',
  'se que se espera de mi en el trabajo',
  'formador comunica como contribuyo a nuestro exito',
  'participo en decisiones que afectan mi trabajo',
  'expreso mi opinion sin represalias',
  'comunicacion interna clara y fortalece companerismo',
  'recibo info y comunicacion para desempenarme eficazmente',
  'tengo conversaciones suficientes con mi formador',
  'recibo feedback util y con seguimiento',
  'cuento con las herramientas necesarias',
  'cuento con recursos y apoyo necesarios',
  'empresa lider',
  'orgullo de trabajar en esta empresa',
  'quiero trabajar muchos anos aqui',  # it's ok 
  'mi trabajo tiene sentido especial',
  'la mision de mi empresa hace sentir importante mi trabajo',
  'vinculo entre mi trabajo y la vision de la empresa',
  'empresa cuida mi bienestar',
  'empresa promueve mi bienestar emocional',
  'las politicas de la empresa son claros y accesibles',
  'crecimiento profesional mediante capacitacion y feedback',
  'la capacitacion sirve para hacer mejor mi trabajo',
  'he crecido como persona desde que trabajo aqui',
  'la empresa aprovecha mis talentos',# it's ok
  'en la empresa aplico mis talentos',
  'percibo oportunidades de crecimiento',
  'meritocracia',
  'cremiento a corto o mediano plazo',
  'mi empresa respeta la diversidad',
  'supervisores representan valores de la empresa',
  'confio en las decisiones del equipo directivo',
  'confio en las decisiones de mi formador',
  'mi formador esta dispuesto a ayudarme',
  'seguiria a mi formador',
  'mi lider me transmite certidumbre',
  'mi lider me inspira',
  'mi lider me hace sentir valorado',# it's ok
  'los superiores son honestos',
  'los lideres inspiran confianza',
  'los superiores hacen un buen trabajo',
  'mi empresa contribuye positivamente a la sociedad',
  'me siento valorado',
  'recibo reconocimiento por mi trabajo',
  'los superiores reconocen el buen trabajo',
  'pensar en ir a trabajar me estresa',
  'estoy entusiasmado con mi trabajo',
  'se anima a una vida equilibrada entre laboral y personal',
  'trabajo no afecta mis actividades personales',
  'me siento seguro en el ambiente laboral'
  )



# datos <- datos %>% 
#   mutate(across(44:98, ~as.factor(.)))


datos %>% 
  dplyr::select(44) %>% 
  unique

bateria <- datos %>% 
  dplyr::select(44:98) %>%
  mutate(
    across(everything(), ~as.factor(.)),
    across(everything(), ~factor(., levels = c('Totalmente en desacuerdo','En desacuerdo', 'Ni de acuerdo, ni en desacuerdo', 'De acuerdo', 'Totalmente de acuerdo'))),
    across(everything(), ~ordered(.)),
    across(everything(), ~as.numeric(.))
  ) %>% 
  setNames(nombres_cortos)
  

bateria %>% summary

poly <- bateria %>% polychoric()

r <- poly$rho

distancia <- as.dist(1-r)

hc <- hclust(distancia, method = 'ward.D2')

dendograma <- as.dendrogram(hc)

dendograma <- dendograma %>% 
  color_branches(k =6) %>% 
  color_labels (k = 6)


par(mar = c(1,1,1,30))
plot(dendograma, horiz = T, axes = F)


clusters <- cutree(hc, k = 6)

info_clusters <- data.frame(
  variable = names(clusters),
  cluster = clusters %>% unname
)

save.image('ws1.RData')

# evaluación de la información aportada ------------------------


load('ws1.RData')

library(mirt)
library(purrr)

modelo <- mirt(bateria, 
               model = 1, 
               itemtype = 'graded'
               # technical = list(
               #   NCYCLES = 3000  
               # )
               )


bateria$score <- fscores(modelo, method = 'EAP') %>% as.vector

ggplot(bateria, aes(x = score)) +
  geom_histogram() + 
  geom_density()

plot(modelo, type = 'info')

informacion <- list()

for(i in 1:55){
  
  item <- extract.item(modelo, i)
  theta_grid <- matrix(seq(-2, 2, length.out = 81), ncol = 1)
  informacion[[i]] <- mirt::iteminfo(item, Theta = theta_grid) 
  informacion[[i]] <- mean(informacion[[i]])
  
}



resumen_info <- data.frame(
  variable = bateria %>% dplyr::select(1:55)  %>% names,
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






library(ggrepel)

grid <- matrix(seq(-2, 2, length.out = 81), ncol = 1)

informacion <- list()

for(i in 1:55){
  
  item <- extract.item(modelo, i)
  theta_grid <- matrix(seq(-2, 2, length.out = 81), ncol = 1)
  informacion[[i]] <- mirt::iteminfo(item, Theta = theta_grid) 
  
}

informacion_items_2 <- bind_cols(informacion)

names(informacion_items_2) <- nombres_cortos 
# %>% str_wrap(.,40)

info_total <- colSums(informacion_items_2)

centro_info <- colSums(informacion_items_2 * grid) / info_total

centro_info <- data.frame(
  variable = names(centro_info),
  
  centro_info
)


centro_info <- left_join(
  centro_info,
  info_clusters
) %>% 
  mutate(
    cluster = factor(cluster),
    cluster = factor(cluster, labels = c('habilitadores de mi desempeño',
                                         'retribución y balance',
                                         'mi formador',
                                         'orgullo y pertenencia',
                                         'desarrollo',
                                         'liderazgo y reconocimiento')
    )
  )



centro_info <- left_join(
  centro_info,
resumen_info
)


centro_info <- centro_info %>% 
  group_by(cluster) %>% 
  mutate(media = weighted.mean(centro_info,informacion_proporcion)) %>% 
  ungroup

# 
# 
# centro_info <- centro_info %>% 
#   mutate(cluster2 = cluster) %>% 
#   mutate(
#     cluster2 = if_else(cluster2 == 'compensación','balance',cluster2),
#     cluster2 = if_else(cluster2 == 'trabajo en equipo','interacción con mi trabajo',cluster2)
#   ) %>% 
#   group_by(cluster2) %>% 
#   mutate(media2 = weighted.mean(centro_info,informacion_proporcion)) %>% 
#   ungroup



ggplot(centro_info, aes(x = cluster, y = centro_info, label = variable, color = cluster)) +
  geom_boxplot()+
  geom_point(aes(y = media), size = 5) +
  # stat_ellipse() +
  # geom_label_repel(max.overlaps = Inf) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(
    y = 'en qué parte de la escala está el centro de cada clúster'
  )


save.image('ws2.RData')


#-----------------------------


load('ws2.RData')

# GRÁFICA FINAL

# resumen_info <- resumen_info %>% 
#   mutate(variable = as.character(variable))
# 
# centro_info <- centro_info %>% 
#   arrange(variable)
# 
# resumen_info <- resumen_info %>% 
#   arrange(variable)
# 
# info_clusters <- info_clusters %>% 
#   arrange(variable)
# 
# resumen_final <- bind_cols(
#   centro_info,
#   resumen_info,
#   info_clusters
# ) %>% 
#   clean_names %>% 
#   mutate(cluster = as.factor(cluster))
# 


centro_info$variable


cuales <- c(0,0,0,1,1,
  1,0,0,0,0,
  1,1,0,0,0,
  0,1,0,1,0,
  0,0,1,0,1,
  0,1,0,0,0,
  0,0,1,0,0,
  0,0,1,0,1,
  1,0,0,0,1,
  1,0,0,0,1,
  0,0,0,1,0) %>%{. == 1} %>% which 


centro_info %>% dplyr::select(variable) %>% mutate(id = 1:nrow(.)) %>% 
  arrange(variable) %>% 
  data.frame


centro_info_seleccion <- centro_info[c(
  23,19,
  30,33,32,
  38,15,40,
  45,46,50,
  12,11,17,4,
  24,5,54
),]


ggplot(centro_info, aes(x = centro_info, y = informacion_proporcion, label = variable, color = cluster)) +
  geom_point() +
  geom_point(data = centro_info_seleccion, aes(x = centro_info, y = informacion_proporcion), color = 'black', size =3) +
  geom_label_repel(max.overlaps = Inf, size = 3) +
  theme_minimal() +
  labs(
    x = 'centro de información',
    y = 'proporción de información aportada'
  )

# índice con los valores indicados ---------------------

set.seed(42)

modelo_final <- mirt(bateria %>% dplyr::select(all_of(centro_info_seleccion$variable)), 
               model = 1, 
               itemtype = 'graded'
               # technical = list(
               #   NCYCLES = 3000  
               # )
)



bateria$score_final  <- fscores(modelo_final, method = 'EAP') %>% as.vector

ggplot(bateria,aes(x = score)) +
  geom_histogram(alpha = .5) +
  geom_histogram(aes(x = score_final), alpha = .3) +
  theme_minimal()

# transformación de la escala ----------------------------


bateria$calificacion <- approx(
  c(
    fscores(modelo_final, response.pattern = rep(1,18), full.scores = T)[1],
    fscores(modelo_final, response.pattern = rep(2,18), full.scores = T)[1],
    fscores(modelo_final, response.pattern = rep(3,18), full.scores = T)[1],
    fscores(modelo_final, response.pattern = rep(4,18), full.scores = T)[1],
    fscores(modelo_final, response.pattern = rep(5,18), full.scores = T)[1]
  ),
  c(1,2,3,4,5),
  xout = bateria$score_final
)$y

bateria$calificacion


ggplot(bateria, aes(x = score_final, y = calificacion)) +
  geom_point() +
  theme_minimal()



# puntos de corte ---------------


quantile(bateria$calificacion, seq(0,1,.01))

corte5 <- quantile(bateria$calificacion, c(0,.10,.30,.60,.80,1.00))

ggplot(bateria, aes(x = score_final, y = calificacion)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 1, ymax = corte5[2]), fill =  "#F6C1CC") +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = corte5[2], ymax = corte5[3]), fill =  "#F8E1B4") +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = corte5[3], ymax = corte5[4]), fill =  "#F3F7C4") +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = corte5[4], ymax = corte5[5]), fill =  "#CFE9E5") +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = corte5[5], ymax = 5.2), fill =  "#D6E3F3") +
  geom_point() +
  geom_hline(yintercept = corte5[2]) +
  geom_hline(yintercept = corte5[3]) +
  geom_hline(yintercept = corte5[4]) +
  geom_hline(yintercept = corte5[5]) +
  geom_text(data = bateria %>% head(1),x = -2, y = corte5[2] - .1, label = '10%') +
  geom_text(data = bateria %>% head(1),x = -2, y = corte5[3] - .1, label = '20%') +
  geom_text(data = bateria %>% head(1),x = -2, y = corte5[4] - .1, label = '30%') +
  geom_text(data = bateria %>% head(1),x = -2, y = corte5[5] - .1, label = '20%') +
  geom_label(data = bateria %>% head(1), x = -3, y = corte5[2], label = corte5[2] %>% round(.,2)) +
  geom_label(data = bateria %>% head(1), x = -3, y = corte5[3], label = corte5[3] %>% round(.,2)) +
  geom_label(data = bateria %>% head(1), x = -3, y = corte5[4], label = corte5[4] %>% round(.,2)) +
  geom_label(data = bateria %>% head(1), x = -3, y = corte5[5], label = corte5[5] %>% round(.,2)) +
  geom_text(data = bateria %>% head(1),x = -2, y = 5.2 - .1, label = '20%') +
  geom_label_repel(data = bateria %>% head(1), x = max(bateria$score_final), y = 5, label = 'más de 15% contestan 5 en todos los atributos',xlim = c(-.5,1)) +

  
  theme_minimal()

# gráficas de resultados ------------


r1 <- bateria %>% 
  dplyr::select(all_of(c(centro_info_seleccion$variable))) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarise(media = mean(value)) %>% 
  rename(variable = name) %>% 
  left_join(
    .,
    centro_info_seleccion %>% dplyr::select(variable, cluster)
  ) %>% 
  arrange(cluster) %>% 
  mutate(
    cluster = factor(cluster, levels = unique(cluster)),
    variable = factor(variable, levels = unique(variable) %>% rev)
  ) %>% 
  rename(dimensión = cluster) %>% 
  group_by(dimensión) %>% 
  mutate(promedio_dim = mean(media)) %>% 
  ungroup() %>% 
  mutate(x = c(19-2.5,NA,NA,NA,19-6,NA,NA,19-9,NA,NA,19-11.5,NA,19-14,NA,NA,19-17,NA,NA))


levels(r1$dimensión) <- c(
  ''
)


ggplot(r1, aes(x = variable, y = media, fill = dimensión, color = dimensión, label = round(media,1))) + 
  geom_bar(stat = 'identity', alpha = .7) + 
  geom_text(nudge_y = -.2, color = 'black') +
  geom_point(aes( x = x), y = .4, size = 15) +
  geom_text(aes(label = round(promedio_dim,1), x = x), y = .4, size = 6, color = 'black') +
  # geom_text(data = r2, x = c(2.5,6,8,10.5,13,15), y = r2$media, label = round(r2$media), fill = 'black') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip() +
  theme_minimal() +
  labs(
    x = 'pregunta',
    y = 'score'
  )




