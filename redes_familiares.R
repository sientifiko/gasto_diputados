
library(tidyverse)
library(ggwordcloud)
theme_set(theme_classic())

dips <- read.csv2("base_diputados.csv",
                  encoding = "latin1")

colnames(dips)[2] <- "nom.dip"

personal <- read.csv2("gastos_personal.csv", 
                      encoding = "latin1")


colnames(personal)[2] <- "nom.personal"

dips$nom.dip %>% 
  str_remove("Sr. |Sra. ") -> dips$nombre2


dips$posicion <- case_when(
  
  dips$partido %in% c("PC", "PCS", "FRVS", "PEV", "PAH", "PH", "COMUNES", "RD", "COMUNES") ~ "IZQ",
  dips$partido %in% c("PS", "PPD", "LIBERAL") ~ "CENTRO IZQ",
  dips$partido %in% c("DC", "PR", "PDG", "PRI") ~ "CENTRO",
  dips$partido %in% c("RN", "UDI", "EVOP") ~ "DER",
  dips$partido %in% c("PCC", "PREP") ~ "ULTRA DER",
  T ~ "IND"
)



conso <- personal %>% 
  filter(anno == 2022) %>% 
  left_join(dips,by = "id")

conso$sueldo <- conso$sueldo %>% str_remove_all("\\.") %>% as.numeric()

conso$nom.personal2 <- conso$nom.personal %>% 
  str_replace_all(" +", " ")

conso$nom.personal %>% 
  unique() %>% 
  str_count(" ")+1 -> largos 

ggplot() +
  aes(largos) +
  geom_histogram()


conso$posicion <- factor(conso$posicion,
                         unique(conso$posicion)[c(1, 6, 2, 3, 5, 4)])


asesores <- conso %>% 
  group_by(nombre2, posicion, nom.personal2, cargo) %>% 
  summarise(monto = sum(sueldo, na.rm = T),
            meses = length(unique(mes))) %>% 
  na.omit()

asesores$dip_sur <- word(asesores$nombre2, 2)
asesores$pers_sur <- word(asesores$nom.personal2)

sum(asesores$dip_sur == asesores$pers_sur)


asesores %>% 
  group_by(posicion, pers_sur) %>% 
  count() -> surnames

surnames$pers_sur %>% 
  tolower() %>% 
  iconv(from = "UTF-8",to="ASCII//TRANSLIT") -> surnames$pers_sur2

surnames %>% 
  # filter(n > 1) %>% 
  ggplot() +
  aes(label = pers_sur2, size = n*3) +
  geom_text_wordcloud() +
  facet_wrap(~posicion) +
  labs(title = "Apellidos de personal",
       subtitle = "Principales apellidos de personal por posición política")



surnames %>% 
  group_by(pers_sur2) %>% 
  summarise(n = sum(n)) %>% 
  mutate(p = n/sum(n)) -> sur_freq

asesores




