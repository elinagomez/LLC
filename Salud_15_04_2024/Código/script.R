
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(ggdark)



##Cargo Datos

load(file = "Salud_15_04_2024/Datos/datos.RData")

##gobierno_salud

p1=gobierno_salud %>%
  pivot_longer(-desempeno)%>%
  mutate(name=case_when(name %in% "Febrero.de.2023" ~ "Febrero de 2023", 
                        name %in% "Mayo.de.2023" ~ "Mayo de 2023", 
                        name %in% "Setiembre.de.2023" ~ "Setiembre de 2023",
                        TRUE ~ name))%>%
  mutate(name=factor(name,levels=c("Setiembre de 2023","Mayo de 2023", "Febrero de 2023")))%>%
  
  mutate(desempeno=factor(desempeno,levels=c("No sabe/no contesta","Muy malo", "Malo","Regular","Bueno", "Muy bueno")))%>%
  ggplot(aes(x = name, y = value,fill=desempeno)) +
  geom_col() +
  geom_text(aes(label = paste0(round(value*100,0), "%")),size=5,color="black",
            position = position_stack(vjust = 0.5),fontface = "bold") +
  scale_fill_manual(values = c("#bcbcbc","#6E3732","#dd6f64","#eceb55","#b6f1a0","#47613e" ))+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "¿Cómo evalúas el desempeño del gobierno en materia de políticas de salud?",
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
        # legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Fira Sans Condensed",size = 12, face = "bold"),
        #legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")+
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))


##gobierno_salud_region

p2=gobierno_salud_region %>%
  pivot_longer(-desempeno)%>%
  mutate(name=factor(name,levels=c("Interior","Montevideo")))%>%
  
  mutate(desempeno=factor(desempeno,levels=c("No sabe/no contesta","Muy malo", "Malo","Regular","Bueno", "Muy bueno")))%>%
  ggplot(aes(x = name, y = value,fill=desempeno)) +
  geom_col() +
  geom_text(aes(label = paste0(round(value*100,0), "%")),size=5,color="black",
            position = position_stack(vjust = 0.5),fontface = "bold") +
  scale_fill_manual(values = c("#bcbcbc","#6E3732","#dd6f64","#eceb55","#b6f1a0","#47613e" ))+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "¿Cómo evalúas el desempeño del gobierno en materia de políticas de salud? - Región",
       subtitle = "Panel UPC - Set 2023",
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
        # legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Fira Sans Condensed",size = 12, face = "bold"),
        #legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")+
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))

ggsave(p2, file="C:/Users/elina/OneDrive/Escritorio/CISOC/RRSS_campaña/Salud/Plots/p2.png",
       width = 3300, height = 1600, units = "px")



##tiempos espera

p3=tiempo_espera %>%
  pivot_longer(-tiempo)%>%
  mutate(group="uno")%>%
  mutate(tiempo=factor(tiempo,levels=c("No sabe/No contesta","Muy largos", "Largos","Regulares","Cortos", "Muy cortos")))%>%
  ggplot(aes(x = group, y = value,fill=tiempo)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(value*100,0), "%")),size=5,color="black",
            position = position_stack(vjust = 0.5),fontface = "bold") +
  scale_fill_manual(values = c("#bcbcbc","#6E3732","#dd6f64","#eceb55","#b6f1a0","#47613e" ))+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "¿Cuál es tu opinión respecto a los tiempos para conseguir una cita con un profesional de la salud?",
       subtitle = "Panel UPC - Jul 2022",
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
        # legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(family = "Fira Sans Condensed",size = 12, face = "bold"),
        axis.text.y = element_blank(),
        #legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")+
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))



##obstaculos

obstaculos=openxlsx::read.xlsx("C:/Users/elina/OneDrive/Escritorio/CISOC/RRSS_campaña/Salud/obstaculo.xlsx",
                                  detectDates = TRUE)  

p4=obstaculos %>%
  pivot_longer(-obstaculos)%>%
  mutate(group="uno")%>%
  mutate(obstaculos=factor(obstaculos,levels=c("No sabe/No contesta","Otra", "Comprensión de las indicaciones médicas",
                                       "No retorno al médico tratante","Limitante económica", "Coordinación de estudios")))%>%
  ggplot(aes(x = group, y = value,fill=obstaculos)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(value*100,0), "%")),size=5,color="black",
            position = position_stack(vjust = 0.5),fontface = "bold") +
  scale_fill_manual(values = c("#bcbcbc","#66C2A5","#FC8D62","#8DA0CB","#E5C494","#FFD92F" ))+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "¿Cuál crees que es el principal obstáculo para iniciar y/o continuar con un tratamiento?",
       subtitle = "Panel UPC - Jul 2022",
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
        # legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(family = "Fira Sans Condensed",size = 12, face = "bold"),
        axis.text.y = element_blank(),
        #legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")+
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))



##problemas

p6=problemas %>%
  pivot_longer(-problema)%>%
  mutate(name=openxlsx::convertToDate(name, origin = "1900-01-01")) %>%
  mutate(problema=factor(problema,levels=c("Seguridad","Aumento de precios","Desempleo","Salud")))%>%
  ggplot(aes(x = as.Date(name), y = value, colour = problema)) + 
  geom_line(size = 1.2) +
  geom_point(size=2.5)+
  geom_text(aes(y = value, label = paste0(round(value*100,0), "%") ), hjust= 0.5, vjust = -2, size= 5, color= "white",fontface = "bold") +
  
  scale_colour_manual(values = c("#FFD92F","#66C2A5","#FC8D62","#8DA0CB","#E5C494" ))+
  scale_x_date(date_labels = "%m %y")+
  scale_y_continuous(labels = scales::percent,limits = c(0,0.4))+
  labs(title = "¿Cuál crees que es el principal problema del país hoy?",
       subtitle = "Panel UPC",
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
        # legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text= element_text(family = "Fira Sans Condensed",size = 12, face = "bold"),
        #legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")


##problemas

problemas=openxlsx::read.xlsx("C:/Users/elina/OneDrive/Escritorio/CISOC/RRSS_campaña/Salud/problema.xlsx",
                              detectDates = TRUE)  

p6=problemas %>%
  pivot_longer(-problema)%>%
  mutate(name=openxlsx::convertToDate(name, origin = "1900-01-01")) %>%
  mutate(problema=factor(problema,levels=c("Seguridad","Aumento de precios","Desempleo","Salud")))%>%
  ggplot(aes(x = as.Date(name), y = value, colour = problema)) + 
  geom_line(size = 1.2) +
  geom_point(size=2.5)+
  geom_text(aes(y = value, label = paste0(round(value*100,0), "%") ), hjust= 0.5, vjust = -2, size= 5, color= "white",fontface = "bold") +
  
  scale_colour_manual(values = c("#FFD92F","#66C2A5","#FC8D62","#8DA0CB","#E5C494" ))+
  scale_x_date(date_labels = "%b %Y")+
  scale_y_continuous(labels = scales::percent,limits = c(0,0.4))+
  labs(title = "¿Cuál crees que es el principal problema del país hoy?",
       subtitle = "Panel UPC",
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
        # legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text= element_text(family = "Fira Sans Condensed",size = 12, face = "bold"),
        #legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")

##preocupaciones


library(ggrepel)
p7=preocupaciones %>%
  pivot_longer(-preocupacion)%>%
  mutate(name=openxlsx::convertToDate(name, origin = "1900-01-01")) %>%
  mutate(preocupacion=factor(preocupacion,levels=c("Trabajo","Seguridad","Salud","Educación","Otros")))%>%
  ggplot(aes(x = as.Date(name), y = value, colour = preocupacion)) + 
  geom_line(size = 1.2) +
  geom_point(size=2.5)+
  #geom_text(aes(y = value, label = paste0(round(value*100,0), "%") ), hjust= 0.5, vjust = -1.3, size= 5, color= "white",fontface = "bold") +
  geom_text_repel(aes(y = value, label = paste0(round(value*100,0), "%")),size= 5, color= "white",fontface = "bold")+
  scale_colour_manual(values = c("#FFD92F","#66C2A5","#FC8D62","#8DA0CB","#E5C494" ))+
  scale_x_date(date_labels = "%b %Y")+
  scale_y_continuous(labels = scales::percent,limits = c(0,0.6))+
  labs(title = "¿Cuál es tu primera preocupación a futuro?",
       subtitle = "Panel UPC",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 12) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        # legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text= element_text(family = "Fira Sans Condensed",size = 12, face = "bold"),
        #legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")


