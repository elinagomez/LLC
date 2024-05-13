
##Crago librerías
library(ggplot2)
library(ggdark)
library(dplyr)
library(htmlwidgets)
library(quanteda)
library(wordcloud2)


###Abro base (limpia de posteos vacíos y con análisis de sentimiento corrido en Python - pysentimiento -)

load("ViolenciaMujeres_12_05_2024/Datos/base_sent.RData")

##Armo gráfico de Seentimiento por sexo 


sentsex = base_sent %>%
  group_by(sexo, sentimiento)%>%
  summarise(n = n()) %>%
  mutate(per = round((n / sum(n))*100 ,1) )

p1=sentsex %>%
  mutate(sexo=factor(sexo,levels=rev(c("Mujer", "Varon"))))%>%
  ggplot(aes(x = sexo, y = per, fill=rev(sentimiento))) +
  geom_col() +
  geom_text(aes(label = paste0(per, "%")),size=5,colour="black",
            position = position_stack(vjust = 0.5),fontface = "bold") +
  scale_color_manual(values = rev(c("#dd6f64", "#eceb55","#b6f1a0")),guide = FALSE)+
  scale_fill_manual(values = rev(c("#dd6f64", "#eceb55","#b6f1a0")),guide = FALSE)+
  labs(title = "Análisis de sentimiento de comentarios de X (Twitter) a políticos/as uruguayos/as según sexo",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Fira Sans Condensed",size = 12, face = "bold"),
        legend.position = "none")+
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))


## Mensajes de odio

##Abro base con idenificación de mensajes de odio (pysentimiento)


load("ViolenciaMujeres_12_05_2024/Datos/base_hate.RData")
base_hate$hate_rec=ifelse(base_hate$hate=="[]","No","Si")

hatesex = base_hate %>%
  group_by(sexo, hate_rec)%>%
  summarise(n = n()) %>%
  mutate(per = round((n / sum(n))*100 ,1) )



p2=hatesex %>%
  filter(hate_rec=="Si")%>%
  mutate(sexo=factor(sexo,levels=rev(c("Mujer", "Varon"))))%>%
  ggplot(aes(x = sexo, y = per, fill=rev(hate_rec))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(per, "%")),size=5,colour="white",
            position = position_stack(vjust = 0.5),fontface = "bold") +
  scale_fill_manual(values = rev(c("#5F021F")),guide = FALSE)+
  ylim(0,15)+
  labs(title = "Proporción de mensajes de odio en comentarios de X (Twitter) a políticos/as uruguayos/as según sexo",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Fira Sans Condensed",size = 12, face = "bold"),
        legend.position = "none")+
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))


##Nubes de palabras entre mensajes de odio (con limpieza)

#Cargo base hate mujeres y varones
load("ViolenciaMujeres_12_05_2024/Datos/bases_hate.RData")

#Cargo palabras ofensivas mujeres
load("ViolenciaMujeres_12_05_2024/Datos/palabras_hate_mujer.RData")
#Cargo palabras ofensivas varones
load("ViolenciaMujeres_12_05_2024/Datos/palabras_hate_varon.RData")


##Armo matriz de términos con frecuencias y luego nube en HTML

##Nubes mujer

dfm_select =quanteda::dfm(quanteda::tokens(hate_mujer_mas$text_proc,remove_punct = TRUE,remove_numbers = TRUE),
tolower=TRUE,verbose = FALSE) %>%
  dfm_select(palabras$palabra) ##me quedo con las selccionadas como más ofensivas

f <- colSums(dfm_select)
a=wordcloud2(data.frame(names(f), f), 
size = 1, ellipticity = 1, shuffle = FALSE, shape = "circle",rotateRatio = 0) + 
WCtheme(1)

saveWidget(a,"ViolenciaMujeres_12_05_2024/Visualizaciones/hate_mujer.html",selfcontained = F) 



##Nube varon

dfm_select_varon =quanteda::dfm(quanteda::tokens(hate_varon_mas$text_proc,remove_punct = TRUE,remove_numbers = TRUE),
                                tolower=TRUE,
                                verbose = FALSE) %>%
  # quanteda::dfm_trim(min_termfreq = 1)%>%
  dfm_select(palabras_var$palabra)

f <- colSums(dfm_select_varon)
a=wordcloud2(data.frame(names(f), f), 
             size = 1, ellipticity = 1, shuffle = FALSE, shape = "circle",rotateRatio = 0) + 
  WCtheme(1)

saveWidget(a,"ViolenciaMujeres_12_05_2024/Visualizaciones/hate_varon.html",selfcontained = F) 




##Comparo entre FA y Coalición

##Frente Amplio

hate_mujer_mas_FA= hate_mujer_mas %>%
  filter(partido=="Frente Amplio")

dfm_select_fa =quanteda::dfm(quanteda::tokens(hate_mujer_mas_FA$text_proc,remove_punct = TRUE,remove_numbers = TRUE),
                             tolower=TRUE,
                             verbose = FALSE) %>%
dfm_select(palabras$palabra)

f <- colSums(dfm_select_fa)
fa=wordcloud2(data.frame(names(f), f), 
              size = 1, ellipticity = 1, shuffle = FALSE, shape = "circle",rotateRatio = 0) + 
  WCtheme(1)

saveWidget(fa,"ViolenciaMujeres_12_05_2024/Visualizaciones/hate_mujer_fa.html",selfcontained = F)

## Coalición

hate_mujer_mas_noFA= hate_mujer_mas %>%
  dplyr::filter(partido!="Frente Amplio")

dfm_select_nofa =quanteda::dfm(quanteda::tokens(hate_mujer_mas_noFA$text_proc,remove_punct = TRUE,remove_numbers = TRUE),
                               tolower=TRUE,
                               verbose = FALSE) %>%
  dfm_select(palabras$palabra)

f <- colSums(dfm_select_nofa)
nofa=wordcloud2(data.frame(names(f), f), 
                size = 1, ellipticity = 1, shuffle = FALSE, shape = "circle",rotateRatio = 0) + 
  WCtheme(1)

saveWidget(nofa,"ViolenciaMujeres_12_05_2024/Visualizaciones/hate_mujer_nofa.html",selfcontained = F)




