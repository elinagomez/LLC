

##Cargo librerías
require(seededlda)
require(quanteda)
library(quanteda.textplots)
library(dplyr)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(ggdark)
library(tidyr)
library(ggrepel)

##Cargos datos (último año, keyword: transformación educativa OR reforma educativa)
load("C:/Users/elina/OneDrive/Escritorio/CISOC/RRSS_campaña/base_completa.RData")

##1011 artículos
##40 medios de prensa digital

#Primer análisis exploratorio: topicmodeling (LDA)
#Exploro topicos emergentes para construir diccionarios
dfm=quanteda::dfm(tokens_ngrams(tokens(base_completa$body,remove_punct = T,
                         remove_numbers = T),n = 2),tolower=TRUE,verbose = FALSE) %>%
  quanteda::dfm_remove(pattern = c(quanteda::stopwords("spanish")),
                       min_nchar=3)

toks <- tokens(base_completa$body, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)
dfma_not <- dfm(toks) %>% 
dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
            max_docfreq = 0.1, docfreq_type = "prop")

tmod_lda <- textmodel_lda(dfma_not, k = 10)

terms(tmod_lda,20)
topics(tmod_lda)

##Armo diccionarios a partir de topicos detectados 


dict <- dictionary(list(confilctos_sindicales = c("paro*", "protesta*", "ocupa*","conflict*",
                                      "polémica*","pérdida de hora*","recorte*","emergencia",
                                      "sanción*","violencia","sindi*","gemi*","federación"),
                        teconología_innovación = c("inteligencia artificial", "tecnología", "digital","innovación",
                                        "ceibal"),
                        implementación = c("inaugura*", "implementación", "programa","impacto","optativas",
                                           "inversión","oferta educativa","repetición",
                                           "lectivo","contenidos","lectura","cursos"),
                        investigación = c("pisa", "pruebas", "indicadores","internacional","encuesta*","investgación*"),
                        paradigma = c("paradigma*","modelo educativo","sistema educativo"),
                        economía = c("inflación","empresas","impuestos","dólares",
                                     "económico","mercado","gasto","salario","rendición")))


##Tokeinzo

toks = tokens_compound(tokens(base_completa$body, remove_punct = TRUE,remove_numbers=TRUE), dict)

##Creo matiz de términos
dfm = quanteda::dfm(toks,tolower = TRUE,verbose = FALSE)%>%
  quanteda::dfm_remove(pattern = c(quanteda::stopwords("spanish")),
                       min_nchar=3)%>%
  dfm_select(dict)


docnames(dfm)= base_completa$id_uri
dfm_df=convert(dfm, to = "data.frame")


midic_result<-dfm_lookup(dfm,dictionary=dict) %>%
convert(to = "data.frame")


#Establezo un umbral de 3 términos para que sea considerado el tema
a=midic_result %>%
  mutate(across(c(2:ncol(midic_result)), ~ifelse( .x >= 3, 1,0))) %>%
  left_join(.,base_completa%>% dplyr::select(medio,fecha,id_uri),by=c("doc_id"="id_uri"))

#acumulo por semana
b= a %>% 
  mutate(week=floor_date(fecha, unit = "week")) %>%
  group_by(week) %>%
  summarize(n=n()) 

p1=ggplot(b, aes(x = as.Date(week), y = n)) +
  geom_line(size = 1.8) +
  geom_vline(              ## 1
             aes(xintercept = as.numeric(as.Date("2023-03-05"))),   ## 2
             linetype = 3, colour = "white")+
  geom_point(size=3)+
  geom_vline(              ## 1
    aes(xintercept = as.numeric(as.Date("2023-06-18"))),   ## 2
    linetype = 3, colour = "white")+
  geom_point(size=3)+
  geom_vline(              ## 1
    aes(xintercept = as.numeric(as.Date("2023-10-22"))),   ## 2
    linetype = 3, colour = "white")+
  geom_point(size=3)+
  geom_vline(              ## 1
    aes(xintercept = as.numeric(as.Date("2024-03-03"))),   ## 2
    linetype = 3, colour = "white")+
  geom_point(size=3)+
  geom_text(aes(y = n, label = ifelse(n >= 20, n, "")),check_overlap = TRUE, nudge_x = -6,nudge_y = 1,hjust= 0.5, vjust = -1, size= 5, color= "white",fontface = "bold") +
  scale_x_date(breaks = as.Date(c("2023-03-05","2023-06-18","2023-10-22", "2024-03-03")), 
               labels=scales::date_format("%b-%Y"),
               )+
  ylim(0, 80)+
  labs(title = "Evolución de artículos de prensa digital sobre Reforma Educativa",
x = "",
y = "",
color = "",
caption = "Fuente: Usina de Percepción Ciudadana")+
 dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.background = element_rect(fill = "grey10"),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))


##Exploro pesos de cada tema en el total 

reforma_temas=a %>% 
  summarise_at(c(2:6), sum, na.rm = TRUE)%>%
  pivot_longer(cols = c(1:5))%>%
  #group_by(name,week)%>%
  mutate(prop = round((value / 1011 * 100),1))

reforma_temas$name= recode(reforma_temas$name,confilctos_sindicales = "Conflictos sindicales" ,
               teconología_innovación = "Teconología e innovación",
               implementación = "Implementación",
               investigación = "Investigación",
               paradigma= "Paradigma educativo")


  
p2= ggplot(reforma4,aes(x = reorder(name,prop), y = prop, fill = name)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=prop),position = position_dodge(width= 1), vjust= 0.5, hjust = -0.5,
            size= 5, color= "white",fontface = "bold",family = "Fira Sans Condensed") + 
  ylim(0,40)+
  labs(title = "Proporción de temas en el total de artículos de prensa digital sobre Reforma Educativa",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.background = element_rect(fill = "grey10"),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        axis.text.y =  element_text(family = "Fira Sans Condensed",size = 14,face="bold"))+
  coord_flip()


## Evolución de temas en el tiempo (semanal)

reforma_temas2=  a %>% 
  mutate(week=floor_date(fecha, unit = "week")) %>%
  group_by(week) %>%
    summarise_at(c(2:6), sum, na.rm = TRUE)%>%
    pivot_longer(cols = c(2:6))

reforma_temas2$name= recode(reforma_temas2$name,confilctos_sindicales = "Conflictos sindicales" ,
                      teconología_innovación = "Teconología e innovación",
                      implementación = "Implementación",
                      investigación = "Investigación",
                      paradigma= "Paradigma educativo")
 
p3=ggplot(reforma_temas2,aes(x= week, y=value, fill=name)) + 
  geom_area()+
  scale_x_date(breaks = as.Date(c("2023-03-05","2023-06-18","2023-10-22", "2024-03-03")), 
               labels=scales::date_format("%b-%Y"),
  )+geom_vline(              ## 1
    aes(xintercept = as.numeric(as.Date("2023-03-05"))),   ## 2
    linetype = 3, colour = "white")+
  geom_vline(              ## 1
    aes(xintercept = as.numeric(as.Date("2023-06-18"))),   ## 2
    linetype = 3, colour = "white")+
  geom_vline(              ## 1
    aes(xintercept = as.numeric(as.Date("2023-10-22"))),   ## 2
    linetype = 3, colour = "white")+
  geom_vline(              ## 1
    aes(xintercept = as.numeric(as.Date("2024-03-03"))),   ## 2
    linetype = 3, colour = "white")+
    labs(title = "Evolución de artículos de prensa digital por tema sobre Reforma Educativa",
         subtitle = "  ",
         x = "",
         y = "",
         color = "",
         caption = "Fuente: Usina de Percepción Ciudadana")+
    dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
    theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
          plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
          plot.background = element_rect(fill = "grey10"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(family = "Fira Sans Condensed",size = 11,face = "bold"),
           axis.text = element_text(family = "Fira Sans Condensed",size = 11,face = "bold",
                                     )) 


##Exploro entidades (personas y organizaciones) en el texto con spacyr
  
library(spacyr)
#spacy_install() 
#spacy_download_langmodel("es")
#spacy.load('es_core_news_sm')
spacyr::spacy_finalize()  
spacyr::spacy_initialize(model = 'es_core_news_sm') #modelo español

prase <- spacy_parse(base_completa$body, lemma = TRUE, entity = TRUE, 
                        pos = TRUE,tag = TRUE,
                        nounphrase = TRUE)

entidades=spacyr::entity_extract(prase,type = "named")
entidades=entidades%>%
  filter(entity_type=="PER") #me quedo con personas

entidades_dic=as.data.frame(table(entidades$entity))
#gardo la base y limipo entidades erróneas, guardo en base personas

personas=openxlsx::read.xlsx("ReformaEducativa_24_03_2024/Datos/personas.xlsx")

personas$Var1=gsub("_"," ",personas$Var1)
dict_per <- dictionary(list(actores= personas$Var1))

toks_per = tokens_compound(tokens(base_completa$body, remove_punct = TRUE,remove_numbers=TRUE), dict_per)

dfm_per = quanteda::dfm(toks_per,tolower = TRUE,verbose = FALSE)%>%
  quanteda::dfm_remove(pattern = c(quanteda::stopwords("spanish"),"presidente","docentes"),
                       min_nchar=3)%>%
dfm_select(dict_per)

docnames(dfm_per)= base_completa$id_uri

dfm_df_per=convert(dfm_per, to = "data.frame")%>%
  left_join(.,base_completa%>% dplyr::select(medio,fecha,id_uri),by=c("doc_id"="id_uri"))


#Sumo apellidos que son mencionados de diferente forma en el texto
dfm_df_per=dfm_df_per%>%
mutate(luis_lacalle_pou = rowSums((dfm_df_per[,c("lacalle", "lacalle_pou", "luis","luis_lacalle_pou")]), na.rm = TRUE))%>%
  select(-lacalle,-lacalle_pou,-luis)%>%
mutate(yamandú_orsi = rowSums((dfm_df_per[,c("yamandú_orsi", "orsi")]), na.rm = TRUE))%>%
  select(-orsi)%>%
  mutate(julio_maría_sanguinetti = rowSums((dfm_df_per[,c("julio_maría_sanguinetti", "sanguinetti")]), na.rm = TRUE))%>%
  select(-sanguinetti)%>%
  mutate(graciela_bianchi = rowSums((dfm_df_per[,c("graciela_bianchi", "bianchi")]), na.rm = TRUE))%>%
  select(-bianchi)%>%
  mutate(carolina_cosse = rowSums((dfm_df_per[,c("carolina_cosse", "cosse")]), na.rm = TRUE))%>%
  select(-cosse)%>%
  mutate(robert_silva = rowSums((dfm_df_per[,c("robert_silva", "robert")]), na.rm = TRUE))%>%
  select(-robert)%>%
  mutate(fernando_pereira = rowSums((dfm_df_per[,c("fernando_pereira", "pereira")]), na.rm = TRUE))%>%
  select(-pereira)%>%
  mutate(juan_pereyra = rowSums((dfm_df_per[,c("juan_pereyra", "pereyra")]), na.rm = TRUE))%>%
  select(-pereyra)%>%
  mutate(pablo_da_silveira = rowSums((dfm_df_per[,c("pablo_da_silveira", "da_silveira")]), na.rm = TRUE))%>%
  select(-da_silveira)%>%
  mutate(gabriel_gurméndez = rowSums((dfm_df_per[,c("gabriel_gurméndez", "gurméndez")]), na.rm = TRUE))%>%
  select(-gurméndez)%>%
  mutate(gonzalo_baroni = rowSums((dfm_df_per[,c("gonzalo_baroni", "baroni")]), na.rm = TRUE))%>%
  select(-baroni)



base= dfm_df_per %>% 
  mutate(week=floor_date(fecha, unit = "quarter")) %>%
  group_by(week) %>%
  summarise_at(c(2:86), sum, na.rm = TRUE)%>%
  pivot_longer(cols = c(2:86))

personas$Var1=tolower(personas$Var1)
personas$Var1=gsub(" ","_",personas$Var1)
base= base %>% 
left_join(.,personas%>% dplyr::select(Var1,tipo),by=c("name"="Var1"))


p4=base %>% 
  filter(is.na(tipo)==F) %>% 
  mutate(tipo=factor(tipo,levels = c("Gobierno","Oposición","Sindical","Institucionalidad educativa"))) %>% 
ggplot() +
  aes(
    x = as.Date(week), y = value, label = name,
    fill = tipo, segment.color = tipo) + 
  geom_vline(data = base,               ## 1
             aes(xintercept = week),   ## 2
             linetype = 3, colour = "white")+
  geom_label_repel(
    color = "white",
    arrow = arrow(
      length = unit(0.02, "npc"), type = "closed", ends = "first"
    ),
   # xlim  = x_limits,
    point.padding = NA,
    box.padding = 0.1,max.overlaps = 62) +
  scale_fill_discrete(
    name = "",
    aesthetics = c("fill", "segment.color"))+
  guides(fill = guide_legend(override.aes = aes(color = NA)))+
  labs(title = "Evolución de menciones a actores por tipo en artículos de prensa digital sobre Reforma Educativa",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(family = "Fira Sans Condensed",size = 11,face = "bold"),
        axis.text = element_text(family = "Fira Sans Condensed",size = 11,face = "bold"))


##Hago un ranking de actores que aparecen


base_actores= dfm_df_per %>% 
  summarise_at(c(2:49), sum, na.rm = TRUE) %>% 
  t()%>% 
  as.data.frame()
base2$name=rownames(base2)


base_actores= base_actores %>% 
  left_join(.,personas%>% dplyr::select(Var1,tipo),by=c("name"="Var1"))



p5=base_actores %>%
  mutate(tipo=factor(tipo,levels = c("Gobierno","Oposición","Sindical","Institucionalidad educativa"))) %>% 
  mutate(name=stringr::str_replace_all(name,pattern = "_"," "))%>%
  slice_max(order_by = V1,n = 15)%>% 
  ggplot(aes(x = reorder(name,V1), y = V1, fill = tipo)) +
  geom_bar(stat = "identity",position = position_dodge(width= 1))+
  geom_text(aes(x = reorder(name,V1), y = V1,label=V1,group=tipo),position = position_dodge(width= 1), vjust= 0.5, hjust = -0.5,
            size= 5, color= "white",fontface = "bold",family = "Fira Sans Condensed") + 
  ylim(0,820)+
  labs(title = "Cantidad de menciones a actores por tipo en artículos de prensa digital sobre Reforma Educativa",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.background = element_rect(fill = "grey10"),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(family = "Fira Sans Condensed",size = 11,face = "bold"),
        axis.text.y =  element_text(family = "Fira Sans Condensed",size = 14,face="bold"))+
  coord_flip()
  

