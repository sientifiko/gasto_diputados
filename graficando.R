
library(tidyverse)
library(lubridate)
library(factoextra)
library(FactoMineR)
library(ggfortify)

theme_set(theme_classic())
options(scipen = 999)

gastos <- read.csv2("gastos_operacionales.csv",
                    encoding = "latin1")


dips <- read.csv2("base_diputados.csv",
                  encoding = "latin1")



dips$nombre %>% 
  str_remove("Sr. |Sra. ") -> dips$nombre2

dips$posicion <- case_when(
  
  dips$partido %in% c("PC", "PCS", "FRVS", "PEV", "PAH", "PH", "COMUNES", "RD", "COMUNES") ~ "IZQ",
  dips$partido %in% c("PS", "PPD", "LIBERAL") ~ "CENTRO IZQ",
  dips$partido %in% c("DC", "PR", "PDG", "PRI") ~ "CENTRO",
  dips$partido %in% c("RN", "UDI", "EVOP") ~ "DER",
  dips$partido %in% c("PCC", "PREP") ~ "ULTRA DER",
  T ~ "IND"
)


conso <- gastos %>% 
  filter(anno == 2022) %>% 
  left_join(dips,by = "id")
  
  
conso$gastos <- conso$gastos %>% 
  str_remove_all("\\.") %>% 
  as.numeric()

paste("01", conso$mes, conso$anno, sep = "-") %>% 
  dmy() -> conso$fecha




conso %>% 
  group_by(nombre2, partido) %>% 
  summarise(gastototal = sum(gastos)) %>% 
  arrange(desc(gastototal)) %>% 
  head(15) %>% 
  ggplot() +
  aes(reorder(nombre2, gastototal), gastototal, fill = reorder(partido, -gastototal)) +
  geom_col() +
  geom_text(aes(label = scales::dollar(gastototal)), hjust = 1.5) +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x="",
       y="",
       fill = "Partido",
       title = "Top 15 Diputados con más gastos",
       subtitle = "Gasto total acumulado mar a dic 2022",
       caption = "autor tw: @sientifiko1")


conso %>% 
  group_by(item) %>% 
  summarise(gastototal = sum(gastos)) %>% 
  arrange(desc(gastototal)) %>% 
  head(10) %>% 
  ggplot() +
  aes(reorder(str_wrap(item, 25), gastototal), gastototal, fill = reorder(item, -gastototal)) +
  guides(fill = "none") +
  geom_col() +
  geom_text(aes(label = scales::dollar(gastototal)), hjust = 0) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 4000000000) ) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 7)) +
  labs(x="",
       y="",
       title = "Top 10 Items con más gastos",
       subtitle = "Gasto total acumulado mar a dic 2022",
       caption = "autor tw: @sientifiko1")

conso %>% 
  group_by(partido) %>% 
  summarise(avg = mean(gastos, na.rm =  T),
            diputs = length(unique(id))) %>% 
  na.omit() %>% 
  ggplot() +
  aes(reorder(partido, avg), avg, fill = partido) +
  guides(fill = "none") +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  geom_text(aes(label = paste("diputados:",diputs)), hjust = 1.5, vjust = .3) +
  theme(axis.text.y = element_text(size = 7)) +
  labs(x="",
       y="",
       title = "Partidos que más gastan",
       subtitle = "Gasto medio mar a dic 2022",
       caption = "autor tw: @sientifiko1")

conso %>% 
  group_by(posicion) %>% 
  summarise(avg = mean(gastos, na.rm =  T),
            diputs = length(unique(id))) %>% 
  na.omit() %>% 
  ggplot() +
  aes(reorder(posicion, avg), avg, fill = posicion) +
  guides(fill = "none") +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  geom_text(aes(label = paste("diputados:",diputs)), hjust = 1.5, vjust = .3) +
  theme(axis.text.y = element_text(size = 7)) +
  labs(x="",
       y="",
       title = "Posición ideológica que más gasta",
       subtitle = "Gasto medio mar a dic 2022",
       caption = "autor tw: @sientifiko1")



conso$distritoordinal <- ifelse(conso$distrito < 8,
                                conso$distrito,
                                ifelse(conso$distrito < 15, 8,
                                       conso$distrito-6))


conso %>% 
  group_by(distritoordinal, id) %>% 
  summarise(gastototal = sum(gastos, na.rm = T)) %>% 
  ggplot() +
  aes(distritoordinal, gastototal) +
  geom_jitter()


conso$posicion <- factor(conso$posicion,
                         unique(conso$posicion)[c(1, 6, 2, 3, 5, 4)])

conso %>% 
  filter(item == "TRASLACIÓN") %>% 
  group_by(distritoordinal, posicion, id) %>% 
  summarise(gastototal = sum(gastos, na.rm = T)) %>% 
  ggplot() +
  aes(distritoordinal, gastototal, color = posicion) +
  guides(color = "none") +
  geom_jitter() +
  facet_wrap(.~posicion) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x="Distritos ordinales",
       y = "",
       title = "Regionalidad y gasto anual por Traslación",
       subtitle = "Distritos ordenados de norte a sur. Stgo es un solo distrito",
       caption = "@sientifiko", color = "")



conso$sexo <- ifelse(conso$nombre %>% str_detect("Sra."),
                     "M","H")

conso %>% 
  group_by(id, sexo) %>% 
  summarise(gastototal = sum(gastos, na.rm = T)) %>% 
  ggplot() + 
  aes(log(gastototal), color = sexo) +
  geom_density() +
  labs(x="Gasto log",
       y="",
       title = "Distribución de gasto total por sexo",
       subtitle = "Gasto total acumulado mar a dic 2022",
       caption = "@sientifiko", 
       color = "")

conso %>% 
  group_by(id, sexo) %>% 
  summarise(gastototal = sum(gastos, na.rm = T)) -> sexdat

wilcox.test(sexdat$gastototal ~ sexdat$sexo)



conso %>%
  filter(item == "ACTIVIDADES DESTINADAS A LA INTERACCIÓN CON LA COMUNIDAD") %>% 
  ggplot() +
  aes(log(gastos), color = posicion) +
  geom_density() +
  geom_vline(xintercept = log(mean(conso$gastos, na.rm = T))) +
  guides(color = "none") +
  facet_wrap(.~posicion) +
  labs(title = "Gasto en actividades destinadas a interacción con comunidad",
       subtitle = "Distribución mensual por diputado/a",
       x = "Gasto log",
       y = "Densidad",
       caption = "@sientifiko")



conso$item2 <- ifelse(
  str_detect(conso$item,"\\*"), "TELEFONÍA", conso$item
)


df.pc <- conso %>% 
  na.omit() %>% 
  group_by(id, posicion, item2) %>% 
  summarise(gastototal = sum(gastos, na.rm = T)) %>% 
  spread(item2, gastototal)

pc <- PCA(df.pc[, 3:23], graph = F)

fviz_pca_ind(pc,
             geom.ind = "point",
             col.ind = df.pc$posicion,
             addEllipses = F)

datmat <- df.pc[, 3:23]

dipsclust <- hcut(datmat, 2, stand = T)

fviz_dend(dipsclust, 
          cex = .3, 
          show_labels = T)

df.pc$cluster <- dipsclust$cluster

table(df.pc$posicion,
      df.pc$cluster)



party <- conso %>% 
  na.omit() %>% 
  group_by(partido, item2) %>% 
  summarise(gastototal = sum(gastos, na.rm = T)) %>% 
  spread(item2, gastototal)

datmat <- party[, 2:22]
rownames(datmat) <- party$partido

# hs <- hcut(datmat, 6)
# hsk <-  hkmeans(datmat, 6)
# 
# fviz_dend(hs)
# fviz_dend(hsk)


hs <- hcut(datmat, 4)
# hsk <-  hkmeans(datmat, 4)

fviz_dend(hs,
          main = "Similitud en gasto anual por partidos")
# fviz_dend(hsk)






