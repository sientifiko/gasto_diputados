library(rvest)
library(tidyverse)


url <- "https://www.camara.cl/diputados/diputados.aspx#mostrarDiputados"

# sesión por rvest
sesion <- session(url)

print("se creó sesión")

# links del perfil de cada diputado
dip.cods <- sesion %>%
  html_nodes("article.grid-2 > h4 > a") %>% 
  html_attr("href") %>% 
  str_extract("\\d+")

print(paste0("se extrajeron codigos: ", dip.cods[1:10]))

nombres <- sesion %>% 
  html_nodes("#ContentPlaceHolder1_ContentPlaceHolder1_pnlDiputadosLista > article > h4") %>% 
  html_text()

print(paste0("se extrajeron nombres: ", nombres[1:10]))

partidos <- sesion %>% 
  html_nodes("#ContentPlaceHolder1_ContentPlaceHolder1_pnlDiputadosLista > article > p:nth-child(4)") %>% 
  html_text() %>% 
  str_remove("Partido: ")

distritos <- sesion %>% 
  html_nodes("#ContentPlaceHolder1_ContentPlaceHolder1_pnlDiputadosLista > article > p:nth-child(3)") %>% 
  html_text() %>% 
  str_remove("Distrito: N°")
  
base_diputados <- data.frame(id = dip.cods,
                             nombre = nombres,
                             partido = partidos,
                             distrito = distritos)


write.csv2(base_diputados, "datos/base_diputados.csv", row.names = F,
           fileEncoding = "latin1")



