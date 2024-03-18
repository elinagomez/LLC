

##Cargo librerías
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(plotly)
library(ggthemes)
library(ggdark)
library(dplyr) 
library(rvest) 
library(purrr) 
library(puy)
library(quanteda)

##Cargos datos (último año, keyword: pobreza infantil)

load("PobrezaInfantil_17_03_2024/Datos/base_total_pobreza.RData")

##259 artículos
##21 medios de prensa digital

b= base_total_pobreza %>% 
  mutate(week=floor_date(fecha, unit = "bimonth")) %>%
  group_by(week) %>%
  summarize(n=n()) 

p4=ggplot(b, aes(x = week, y = n)) +
  geom_line(size = 1.8) +
  geom_point(size=2)+
  geom_text(aes(y = n, label = n ), hjust= 0.5, vjust = -2, size= 5, color= "white",fontface = "bold") +
  scale_x_date(breaks = "2 month")+
  ylim(0, 90)+
  labs(title = "Cantidad de artículos de prensa digital sobre Pobreza infantil (bimestre)",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 11),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 8),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))


ggsave(p4, file="PobrezaInfantil_17_03_2024/Visualizaciones/p4.png",
       width = 3000, height = 1200, units = "px")


##Parlamento

#Cargo datos total de menciones
load("PobrezaInfantil_17_03_2024/Datos/parlamento.RData")


p6=data %>%
  filter(término=="Pobreza infantil")%>%
ggplot(aes(x = as.factor(legislatura), y = n, group=1)) +
  geom_line(size = 1.8) +
  geom_point(size=2)+
  geom_text(aes(y = n, label = n ), hjust= 0.5, vjust = -2, size= 5, color= "white",fontface = "bold") +
  #scale_x_date(breaks = "2 month")+
  ylim(0, 60)+
  labs(title = "Cantidad de diarios de sesión que tratan Pobreza infantil por Legislatura",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 11),
        plot.caption  = element_text(family = "Fira Sans Condensed",size = 8),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

ggsave(p6, file="PobrezaInfantil_17_03_2024/Visualizaciones/p6.png",
       width = 3000, height = 1200, units = "px")


#Recupero diarios de sesión con speech (https://github.com/Nicolas-Schmidt/speech)


##creo un objeto "ruta" que es el url raíz para luego pegarle la terminación según el número de página del cual me interesa objeter las url a los pdf. 
ruta= "https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion?Cpo_codigo=All&Lgl_Nro=49&fecha_desde=2020-02-15&fecha_hasta=2025-02-14&Ts_diario=&Ssn_Nro=&Tipobusqueda=E&Texto=pobreza+infantil"
paginas=as.character(c(0:7)) ##defino la cantidad según paginado
 
##creo un objeto "url" dónde me va a guardar el vector de las rutas a los diarios
url <- map(paginas,~ paste0(ruta, "&page=", .))%>% #pego el pedazo de ruta para hacer referencia al número de página
   unlist() %>% 
   map(~ .x  %>% ## con la función map() de purr, ítero a lo largo del vector "paginas"
         read_html() %>%  ##función de rvest para obtener contenido en formato html 
         html_nodes(".views-field-DS-File-IMG a") %>% #identifica qué parte o nodo específico me interesa   
         html_attr("href") %>%
         map(~ paste0("", .)))%>%
   unlist()

#Itero para recuperar la base total
#pobinf <- map(url,possibly(speech_build,otherwise = NULL))

#base con la totalidad de intervenciones de los diarios que mencionan pobreza infantil
load("PobrezaInfantil_17_03_2024/Datos/base_parlamento.RData")

base_parlamento_part=add_party(base_parlamento) #agrego etiqueta partidaria
#me quedo con aquellas menciones que tienen efectivamente el término
dict <- dictionary(list(pobreza_infantil = c("pobreza infantil")))
toks = tokens_compound(tokens(base_parlamento$speech, remove_punct = TRUE,remove_numbers=TRUE),dict)
dfm = quanteda::dfm(toks,tolower = TRUE,verbose = FALSE)%>%
  quanteda::dfm_remove(pattern = c(quanteda::stopwords("spanish")),
                       min_nchar=3)%>%
dfm_select(dict)

base_parlamento$correl=c(1:nrow(base_parlamento))
docnames(dfm)= base_parlamento$correl
dfm_df=convert(dfm, to = "data.frame")

##103 intervenciones

dfm_df=dfm_df %>%
  mutate(across(c(2:ncol(dfm_df)), ~ifelse( .x >= 1, 1,0))) %>% #me quedo con las que mencionan al menos una vez el término
  mutate(doc_id=as.numeric(doc_id))%>%
  left_join(.,base_parlamento,by=c("doc_id"="correl"))%>%
  filter(pobreza_infantil==1)

##102 intervenciones al menos una vez pobreza infantil
#cargo base con partido
load(file="PobrezaInfantil_17_03_2024/Datos/intervenciones.RData")

p=intervenciones %>% 
  group_by(party_acron)%>%
  summarize(n=n())%>%
  mutate(per=n/sum(n)*100)%>%
  ggplot(aes(x = reorder(party_acron,-per), y = per, fill = reorder(party_acron,-per))) +
  geom_bar(stat = "identity",width = 0.5)+
  geom_text(aes(label=paste0(round(per), "%")), vjust=-0.3,label.size = 0.35,size= 5,fontface = "bold") +
  labs(title = "Proporción de intervenciones sobre Pobreza infantil por Partido político",
       subtitle ="Legislatura 2020-2025",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  ylim(0, 100)+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  scale_fill_manual(values=c("#FFC000","#80BFFF","#7A0000","#BF9000","#7031A0","#2D661C"))+
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 11),
        plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 8),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "none")

library(cowplot)
library(magick)

pimage <- axis_canvas(p, axis = 'x')+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Logo_Frente_Amplio.svg/1920px-Logo_Frente_Amplio.svg.png", x = 0.5, scale = 0.5)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/5/5d/Flag_of_the_National_Party_%28Uruguay%29.svg/768px-Flag_of_the_National_Party_%28Uruguay%29.svg.png", x = 1.5, scale = 0.5)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/archive/3/3b/20130913200454%21Flag_of_Colorado_Party_%28Uruguay%29.svg", x = 2.5, scale = 0.5)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/8/8f/Partido_cabildo_abierto_270x180_flag_version.jpg", x = 3.5, scale = 0.5)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Bandera_Partido_Independiente.png/1200px-Bandera_Partido_Independiente.png", x = 4.5, scale = 0.5)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/3/33/Bandera_PERI.png/1200px-Bandera_PERI.png", x = 5.5, scale = 0.5)


p7=ggdraw(insert_xaxis_grob(p, pimage, position = "center"))



ggsave(p7, file="PobrezaInfantil_17_03_2024/Visualizaciones/p7.png",
       width = 3000, height = 1200, units = "px")



