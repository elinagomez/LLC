
##Cargo librerías
library(dplyr)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(ggdark)
library(ggrepel)
library(cowplot)
library(magick)


##Cargo base posteos IG de candidatas/os, último año


load(file="RedesSociales_31_03_2024/Datos/base_ig_total.RData")

#Armo gráfico de promedio bimensual de likes en posteos IG

likes=base_ig_total %>%
  mutate(week=floor_date(fecha, unit = "bimonth")) %>%
  group_by(week,ownerUsername)%>%
  summarize(n=round(mean(likesCount,na.rm = TRUE),0))%>%
  mutate(partido =case_when(
    ownerUsername %in% c("andreslima.uy", "cossecarolina", "orsi.yamandu", "bergara_fa") ~ "Frente Amplio",
    ownerUsername %in% c("robertsilvauy", "andresojedaok","ggurmendez","acostaylarag","robertsilvauy") ~ "Partido Colorado",
    ownerUsername %in% c("alvarodelgadouy","jorgegandini", "lauraraffo", "juansartoriuy") ~ "Partido Nacional",
    ownerUsername %in% "guidomanini" ~ "Cabildo Abierto",
    ownerUsername %in% NA ~ "Otro"
  ))%>%
  as.data.frame()

#Plot
likes_p=likes%>%
  mutate(label = if_else(week == max(week), as.character(ownerUsername), NA_character_)) %>%
  ggplot(aes(x = as.Date(week), y = n, group = ownerUsername, colour = partido,fill=partido)) + 
  geom_line(size = 1.2) +
  geom_point(size=2.5)+
  geom_label_repel(aes(label = label,
  ),hjust = "right",fontface="bold",max.time = 3, force = 70, force_pull = 0.5,
  size=3.8, segment.size=0.28, nudge_x=-0.1, direction="y",
  vjust = 0,xlim=c(as.Date("2024-05-01"),NA),
  
  color = "white",
  arrow = arrow(
    length = unit(0.001, "npc"), type = "closed", ends = "first"
  ),  point.padding = NA,
  box.padding = 0.1,max.overlaps = 25)+
  
  coord_cartesian(clip = 'off',expand = TRUE) +
  scale_x_date(labels=scales::date_format("%b-%Y"))+
  scale_fill_manual(values=c("Frente Amplio"="#FFC000","Partido Nacional"="#80BFFF",
                             "Partido Colorado"="#7A0000","Cabildo Abierto"="#454B1B"))+
  scale_colour_manual(values=c("Frente Amplio"="#FFC000","Partido Nacional"="#80BFFF",
                               "Partido Colorado"="#7A0000","Cabildo Abierto"="#454B1B"))+
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm"))  +
  labs(title = "Evolución de promedio bimensual de likes en posteos en Instagram por pre-candidatas/os",
       x = "",
       y = "",
       color = "",
       caption = "Fuente: Usina de Percepción Ciudadana")+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm"),
        plot.title = element_text(family = "Fira Sans Condensed",size = 17),
        plot.background = element_rect(fill = "grey10"),
        plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.5),
        panel.grid.minor = element_line(color = "grey30", size = 0.5),
        legend.background = element_blank(),
        axis.ticks.length.x = unit(.3, units = "cm"),
        axis.ticks.x = element_line(linetype = 2),
        legend.key = element_blank(),
        legend.position = "none")


##Cargo base posteos TikTok de candidatas/os, último año


load("RedesSociales_31_03_2024/Datos/base_tt_total.RData")



#Armo gráfico de promedio bimensual de likes en posteos TT

likes_tt=base_tt_total %>%
    mutate(week=floor_date(fecha, unit = "bimonth")) %>%
    group_by(week,`authorMeta/nickName`)%>%
    summarize(n=round(mean(diggCount,na.rm = TRUE),0))%>%
    mutate(partido =case_when(
      `authorMeta/nickName` %in% c("Andres Lima", "Carolina Cosse", "Yamandú Orsi") ~ "Frente Amplio",
      `authorMeta/nickName` %in% c("Robert Silva", "Andrés Ojeda") ~ "Partido Colorado",
      `authorMeta/nickName` %in% c("Alvaro Delgado","Jorge Gandini", "Laura Raffo", "Juan Sartori") ~ "Partido Nacional",
      `authorMeta/nickName` %in% "Guido Manini Ríos" ~ "Cabildo Abierto",
      `authorMeta/nickName` %in% NA ~ "Otro"
    ))%>%
    as.data.frame()
  
  likes_p_tt=likes_tt%>%
    mutate(label = if_else(week == max(week), as.character(`authorMeta/nickName`), NA_character_)) %>%
    ggplot(aes(x = as.Date(week), y = n, group = `authorMeta/nickName`, colour = partido,fill=partido)) + 
    geom_line(size = 1.2) +
    geom_point(size=2.5)+
    geom_label_repel(aes(label = label,
    ),hjust = "right",fontface="bold",max.time = 3, force = 70, force_pull = 0.5,
    size=4, segment.size=0.30, nudge_x=-0.2, direction="y",
    vjust = 0,xlim=c(as.Date("2024-05-01"),NA),
    
    color = "white",
    arrow = arrow(
      length = unit(0.01, "npc"), type = "closed", ends = "first"
    ),
    # xlim  = x_limits,
    point.padding = NA,
    box.padding = 0.1,max.overlaps = 25)+
    
    coord_cartesian(clip = 'off',expand = TRUE) +
    scale_x_date(labels=scales::date_format("%b-%Y"))+
    scale_fill_manual(values=c("Frente Amplio"="#FFC000","Partido Nacional"="#80BFFF",
                               "Partido Colorado"="#7A0000","Cabildo Abierto"="#454B1B"))+
    scale_colour_manual(values=c("Frente Amplio"="#FFC000","Partido Nacional"="#80BFFF",
                                 "Partido Colorado"="#7A0000","Cabildo Abierto"="#454B1B"))+
    theme(legend.position = 'none',
          plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm"))  +
    labs(title = "Evolución de promedio bimensual de likes en posteos en Tiktok por pre-candidatas/os",
         x = "",
         y = "",
         color = "",
         caption = "Fuente: Usina de Percepción Ciudadana")+
    dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
    theme(plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm"),
          plot.title = element_text(family = "Fira Sans Condensed",size = 17),
          plot.background = element_rect(fill = "grey10"),
          plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.5),
          panel.grid.minor = element_line(color = "grey30", size = 0.5),
          legend.background = element_blank(),
          axis.ticks.length.x = unit(.3, units = "cm"),
          axis.ticks.x = element_line(linetype = 2),
          legend.key = element_blank(),
          legend.position = "none")
  
  
  
##Comparo posteos por candidato/as  entre redes
  
  
load("RedesSociales_31_03_2024/Datos/base_posteos.RData")  
  
posteos=base_posteos %>%
    mutate(candidato=factor(candidato,levels=c("Carolina Cosse","Yamandú Orsi","Andres Lima", "Mario Bergara",
                                               "Alvaro Delgado","Jorge Gandini", "Laura Raffo", "Juan Sartori",
                                               "Robert Silva", "Andrés Ojeda",
                                               "Guido Manini Ríos")))%>%
    ggplot(aes(x = candidato, y = n, fill = red)) +
    geom_bar(position = 'dodge', stat = 'identity',width = 0.8)+
    geom_text(aes(label=n,group = red), vjust=-0.5,size= 5,fontface = "bold",position = position_dodge(.9)) +
    labs(title = "Cantidad de posteos de precandidatos/as en el último año por red social",
         x = "",
         y = "",
         color = "",
         caption = "Fuente: Usina de Percepción Ciudadana")+
    ylim(0, 790)+
    dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
    scale_fill_manual(values=c("#FA7E1E","#397684"))+
    theme(plot.title = element_text(family = "Fira Sans Condensed",size = 17),
          plot.subtitle = element_text(family = "Fira Sans Condensed",size = 9),
          plot.caption = element_text(family = "Fira Sans Condensed",size = 15),
          plot.background = element_rect(fill = "grey10"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.position = "bottom",legend.title = element_blank(),
          axis.text.x = element_blank())
  
  pimage <- axis_canvas(p, axis = 'x')+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/5/5a/Carolina_Cosse.jpg/1280px-Carolina_Cosse.jpg", x = 0.5, scale = 0.8)+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/1/1a/Yamand%C3%BA_Orsi_perfil.png", x = 1.5, scale = 0.8)+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/5/53/Andr%C3%A9s_Lima_Intendente.jpg/1024px-Andr%C3%A9s_Lima_Intendente.jpg", x = 2.5, scale = 0.8)+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/1/1b/Bergara_Pte.jpg", x = 3.5, scale = 0.8)+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/a/aa/AlvaroDelgado-2000x1500_%28cropped%29.jpg/1024px-AlvaroDelgado-2000x1500_%28cropped%29.jpg", x = 4.5, scale = 0.8)+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/9/93/JorgeGandini.png", x = 5.5, scale = 0.8)+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/d/d3/Laura_Raffo_%28cropped%29.jpg", x = 6.5, scale = 0.8)+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/5/5a/Juan_Jos%C3%A9_Sartori.png", x = 7.5, scale = 0.8)+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/3/3e/Robert_Silva_2023_%28cropped%29.png", x = 8.5, scale = 0.8)+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/7/7d/Andres_Ojeda.jpg", x = 9.5, scale = 0.8)+
    #cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/3/35/Gurmendez.jpg/1024px-Gurmendez.jpg", x = 10.5, scale = 0.8)+
    #cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/c/cd/ACOSTA_Y_LARA.jpg", x = 11.5, scale = 0.8)+
    cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/6/60/Gral._Guido_Manini_Rios.png/802px-Gral._Guido_Manini_Rios.png", x = 10.5, scale = 0.8)
  
  
plot=ggdraw(insert_xaxis_grob(posteos, pimage, position = "center"))
  
