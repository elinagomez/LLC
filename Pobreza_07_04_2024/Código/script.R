
##Cargo librerías
library(dplyr)
library(rlang)
library(stringr)
library(openxlsx)
library(dplyr)
library(wrapr)
library(igraph)
library(visNetwork)

##Cargo base
load("Pobreza_07_04_2024/Datos/base.RData")

##Armo nodos y vértices
retweets_network <- base_ret %>% 
  gt_edges(`user/screen_name`, in_reply_to_screen_name, full_text) %>%            
  gt_graph()


retweets_nodes <- igraph::as_data_frame(retweets_network, what = "vertices")
retweets_nodes <- retweets_nodes %>% 
  mutate(id = name) %>% 
  mutate(label = name) %>% 
  mutate(title = name) %>% 
  mutate(degree = degree(retweets_network)) %>% 
  mutate(value = degree)

retweets_edges <- igraph::as_data_frame(retweets_network, what = "edges")
retweets_edges <- retweets_edges %>% 
  mutate(title = full_text)

##Le agrego tipo de actor manual y vuelvo a cargar

load("Pobreza_07_04_2024/Datos/nodos.RData")

##Armo la red interactiva con visNetwork
net=visNetwork(nodes, retweets_edges,height = "700px",width = "100%") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visOptions(highlightNearest = list(enabled=TRUE, labelOnly = TRUE, hover = TRUE,degree = list(from = 1, to = 1)),
             nodesIdSelection = FALSE,
             selectedBy ="group",
             manipulation = TRUE) %>%
  visEdges(arrows = list(
    to = list(enabled = TRUE), 
    from = list(enabled = TRUE)
  )) %>%
  
  visGroups(groupname = "Gobierno", color = "#A8CDF7") %>%    
  visGroups(groupname = "Oposición", color = "#CF8232")   %>%
  visGroups(groupname = "Otro", color = "slategrey")  %>%
  visGroups(groupname = "Medios", color = "#463500")  %>%
  visGroups(groupname = "Técnico", color = "#FFFF66")

#Guardo en HTML
visSave(net, file = 'Pobreza_07_04_2024/Visualizaciones/net.html')



##Análisis de Sentimiento

##Hago análisis de sentimiento en Python con pysentimiento y caego
load("Pobreza_07_04_2024/Datos/sent.RData")


sent$conversation_id_str=as.character(sent$conversation_id_str)
table(sent$sentimiento)

sent$in_reply_to_screen_name

sent_2 = sent %>%
  mutate(in_reply_to_screen_name=tolower(in_reply_to_screen_name))%>%
  left_join(retweets_nodes, by= c("in_reply_to_screen_name" = "name"))%>%
  filter(is.na(group)==F)%>%
  group_by(group, sentimiento)%>%
  summarise(n = n()) %>%
  mutate(per = round((n / sum(n))*100 ,1) )


##Armo gráfico
p1=sent_2 %>%
  mutate(group=factor(group,levels=rev(c("Gobierno", "Oposición","Técnico","Medios", "Otro"))))%>%
  ggplot(aes(x = group, y = per, color = sentimiento)) +
  geom_col() +
  geom_text(aes(label = paste0(per, "%")),size=5,
            position = position_stack(vjust = 0.5),fontface = "bold") +
  scale_color_manual(values = c("#dd6f64", "#eceb55","#b6f1a0"),guide = FALSE)+
  labs(title = "Análisis de sentimiento de comentarios según tipo de actor",
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
        #legend.title = element_blank(),
        legend.position = "bottom")+
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))




