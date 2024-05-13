

##Cargo librerías

library(sf)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggdark)
library(stringr)
library(geouy)

base=openxlsx::read.xlsx("C:/Users/elina/OneDrive/Escritorio/CISOC/RRSS_campaña/Ruteo/Ruteo.xlsx",
                         detectDates = TRUE,sheet = "PN")

base_fa=openxlsx::read.xlsx("C:/Users/elina/OneDrive/Escritorio/CISOC/RRSS_campaña/Ruteo/Ruteo.xlsx",
                         detectDates = TRUE,sheet = "FA")

base$LOCALIDAD.o.BARRIO=toupper(iconv(base$LOCALIDAD.o.BARRIO,from="UTF-8",to="ASCII//TRANSLIT"))
base_fa$LOCALIDAD.o.BARRIO=toupper(iconv(base_fa$LOCALIDAD.o.BARRIO,from="UTF-8",to="ASCII//TRANSLIT"))
base_pc$LOCALIDAD.o.BARRIO=toupper(iconv(base_pc$LOCALIDAD.o.BARRIO,from="UTF-8",to="ASCII//TRANSLIT"))

##Arreglo Ñ
base = base %>%
 mutate(LOCALIDAD.o.BARRIO = case_when(LOCALIDAD.o.BARRIO == "BANADO DE MEDINA" ~ "BAÑADO DE MEDINA",
                                       LOCALIDAD.o.BARRIO == "EGANA" ~ "EGAÑA",
                                       LOCALIDAD.o.BARRIO == "FLOR DE MARONAS" ~ "FLOR DE MAROÑAS",
                                       LOCALIDAD.o.BARRIO == "VILLA ESPANOLA" ~ "VILLA ESPAÑOLA",
                                       .default = LOCALIDAD.o.BARRIO)) %>%
  mutate(LOCALIDAD.o.BARRIO = str_trim(LOCALIDAD.o.BARRIO,side = "both"))

base_fa = base_fa %>%
  mutate(LOCALIDAD.o.BARRIO = case_when(LOCALIDAD.o.BARRIO == "BANADO DE MEDINA" ~ "BAÑADO DE MEDINA",
                                        LOCALIDAD.o.BARRIO == "EGANA" ~ "EGAÑA",
                                        LOCALIDAD.o.BARRIO == "FLOR DE MARONAS" ~ "FLOR DE MAROÑAS",
                                        LOCALIDAD.o.BARRIO == "VILLA ESPANOLA" ~ "VILLA ESPAÑOLA",
                                        .default = LOCALIDAD.o.BARRIO)) %>%
  mutate(LOCALIDAD.o.BARRIO = str_trim(LOCALIDAD.o.BARRIO,side = "both"))





loc=geouy::load_geouy(c=c("Localidades pg"))%>%
  select(NOMBDEPTO,NOMBLOC,the_geom)

barrio=geouy::load_geouy(c=c("Barrios"))%>%
  select(nombbarr,the_geom)%>%
  rename("NOMBLOC" = "nombbarr")%>%
  mutate(NOMBDEPTO="MONTEVIDEO")

loc=bind_rows(loc,barrio)



##Partido Nacional
##Cargo la base con coordenadas
load("C:/Users/elina/OneDrive/Documentos/LLC/Territorio_22_04_2024/Datos/base_loc_pn.RData")

##Saco figuras para mapear
b = base_loc %>%
    group_by(FIGURA,LOCALIDAD.o.BARRIO,centroids)%>%
  summarize(n=n())%>%
  filter(FIGURA%in%c("Carlos Iafigliola","Luis Lacalle Pou")==F)%>%
mutate(FIGURA=factor(FIGURA,levels=c("Alvaro Delgado", "Laura Raffo", "Jorge Gandini")))
  
##Cargo shape de Uruguay para mapear
depto=geouy::load_geouy("Departamentos")
 
##Frente Amplio
##Cargo la base con coordenadas
load("C:/Users/elina/OneDrive/Documentos/LLC/Territorio_22_04_2024/Datos/base_loc_fa.RData")

 
c = base_loc_fa %>%
   group_by(FIGURA,LOCALIDAD.o.BARRIO,centroids)%>%
   summarize(n=n())%>%
   filter(FIGURA%in%c("Fernando Pereyra","Mario Bergara")==FALSE)%>%
   mutate(FIGURA=factor(FIGURA,levels=c("Yamandú Orsi", "Carolina Cosse", "Andres Lima")))%>%
   mutate(n = if_else(FIGURA == "Yamandú Orsi", 2,if_else(FIGURA == "Carolina Cosse", 1.9,
                                                          if_else(FIGURA == "Andres Lima", 1 ,NA_real_))))


##Mapeo

## En facetas:PN
 
 facetas_pn=ggplot() +
   geom_sf(data = depto, fill = "grey95") +
   geom_sf(data = b,pch = 21,
           aes(size = 1,fill = FIGURA),
           col = "grey20") +
   scale_size(range = c(1, 10),guide = "none" ) +
   guides(fill = guide_legend(override.aes = list(size = 6),title="")) +
   scale_fill_manual(values = c('#343C6A','#56B4E9', '#E69F00')) +
   labs(title = "",
        sub = "",
        size = "") +
   theme_void() +
   labs(title = "Precandidatos del Partido Nacional en el territorio",
        subtitle = "Desde el 1 marzo al 15 abril",
        x = "",
        y = "",
        color = "",
        caption = "Fuente: Usina de Percepción Ciudadana")+
   dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14)+
   theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
         plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
         plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
         panel.background = element_blank(),
         axis.ticks = element_blank(),
         legend.key = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
   facet_wrap(vars(FIGURA))
 
 ## En facetas:FA
 
 facetas_fa=ggplot() +
   geom_sf(data = depto, fill = "grey95") +
   geom_sf(data = c,pch = 21,
           aes(size = 1,fill = FIGURA),
           col = "grey20") +
   scale_size(range = c(1, 10),guide = "none" ) +
   guides(fill = guide_legend(override.aes = list(size = 6),title="")) +
   scale_fill_manual(values = c('#343C6A','#E7B800', '#FC4E07')) +
   labs(title = "",
        sub = "",
        size = "") +
   theme_void() +
   labs(title = "Precandidatos del Frente Amplio en el territorio",
        subtitle = "Desde el 1 marzo al 15 abril",
        x = "",
        y = "",
        color = "",
        caption = "Fuente: Usina de Percepción Ciudadana")+
   dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14)+
   theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
         plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
         plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
         panel.background = element_blank(),
         axis.ticks = element_blank(),
         legend.key = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
   facet_wrap(vars(FIGURA))+
   guides(color = guide_legend(reverse = TRUE))
 
 ggsave(facetas_fa, file="C:/Users/elina/OneDrive/Escritorio/CISOC/RRSS_campaña/Ruteo/facetas_fa.png",
        width = 4100, height = 2500, units = "px")
 
 
 
##Mapas simples: PN
 
 
mapa_pn=ggplot() +
  geom_sf(data = depto, fill = "grey95") +
  geom_sf(
    data = b, aes(shape = FIGURA, color = FIGURA,fill=FIGURA),size = 4.5,
    col = "#343C6A") +
  scale_shape_manual(values = c(18, 24, 21)) + 
  scale_fill_manual(values = c('#343C6A','#56B4E9', '#E69F00')) +
  labs(title = "Precandidatos del Partido Nacional en el territorio",
       subtitle = "Desde el 1 marzo al 16 abril",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana") +
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) +
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        panel.background = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank())


##Mapas simples: FA


mapa_fa=ggplot() +
  geom_sf(data = depto, fill = "grey95") +
  geom_sf(
    data = c, aes(shape = FIGURA, color = FIGURA,fill=FIGURA),size = 4.5,
    col = "#343C6A") +
  scale_shape_manual(values = c(18, 24, 21)) + 
  scale_fill_manual(values = c('#343C6A','#E7B800', '#FC4E07')) +
  labs(title = "Precandidatos del Frente Amplio en el territorio",
       subtitle = "Desde el 1 marzo al 16 abril",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana") +
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) +
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        panel.background = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank())



##Comparo FA y PN

b$partido="Partido Nacional"
c$partido="Frente Amplio"
d=rbind(b,c)


facetas_compara=ggplot() +
  geom_sf(data = depto, fill = "grey95") +
  geom_sf(data = d,pch = 21,
          aes(size = 1,fill = partido),
          col = "grey20") +
  scale_size(range = c(1, 10),guide = "none" ) +
  guides(fill = guide_legend(override.aes = list(size = 6),title="")) +
  scale_fill_manual(values = c('#FC4E07','#343C6A')) +
  labs(title = "",
       sub = "",
       size = "") +
  theme_void() +
  labs(title = "Comparación precandidatos del Frente Amplio y Partido Nacional en el territorio",
       subtitle = "Desde el 1 marzo al 16 abril",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14)+
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")+
  facet_wrap(vars(partido))+
  guides(color = guide_legend(reverse = TRUE))



##Comparar MVD Y Canelones (y cantidad de actividades)

base$region=ifelse(base$DPTO%in%c("Montevideo","Canelones"),"Montevideo y Canelones", "Interior")
base$partido="Partido Nacional"
base_fa$region=ifelse(base_fa$DPTO%in%c("Montevideo","Canelones"),"Montevideo y Canelones", "Interior")
base_fa$partido="Frente Amplio"
base_total=rbind(base,base_fa)
base_total$region=factor(base_total$region,levels=c("Montevideo y Canelones", "Interior"))

activ=base_total %>% 
  group_by(partido,region) %>% 
  summarize(n=n())%>% 
ggplot( aes(x = partido , y = n, fill = region, label = n)) +
  geom_bar(stat = "identity",width = 0.6) +
  geom_text(size = 6, position = position_stack(vjust = 0.5),fontface="bold")+
  scale_fill_manual(values = c('#E7B800','#343C6A')) +
labs(title = "",
     sub = "",
     size = "") +
  ylim(0,115)+
  labs(title = "Cantidad de actividades en territorio del Frente Amplio y Partido Nacional por región",
       subtitle = "Desde el 1 marzo al 16 abril",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14)+
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(family = "Fira Sans Condensed",size = 15, face = "bold"),
        legend.text = element_text(family = "Fira Sans Condensed",size = 15, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank())




