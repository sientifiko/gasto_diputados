library(RSelenium)
library(rvest)

driver <- rsDriver(browser = "firefox",
                   chromever = NULL)

remDr <- driver[["client"]]


dips <- read.csv2("base_diputados.csv",
                  encoding = "latin1")

# nrow(dips)
remDr$close()
remDr$open()


lapply(dips$id, function(id){
  
  url <- paste0(
    "https://www.camara.cl/diputados/detalle/gastosoperacionales.aspx?prmId=",
    id,
    "#ficha-diputados"
  )
  
  print(paste(
    "Navegando en",
    dips$nombre[dips$id==id]
  ))
  
  remDr$navigate(url)
  
  Sys.sleep(1+runif(1))
  
  ddlano <- remDr$findElement(using = "css", 
                              "#ContentPlaceHolder1_ContentPlaceHolder1_DetallePlaceHolder_ddlAno")
  
  annos <- ddlano$getElementAttribute("outerHTML")[[1]] %>% 
    read_html() %>% 
    html_nodes("option") %>% 
    html_text()
  
  # ITERAR AÑOS
  lapply(annos, function(a){
    # 
    ddlano <- remDr$findElement(using = "css",
                                "#ContentPlaceHolder1_ContentPlaceHolder1_DetallePlaceHolder_ddlAno")
    ddlano$clickElement()
    
    Sys.sleep(.5)
    
    # extracción de años
    temp.ano <- remDr$findElement(using = "xpath",
                                  paste0("//option[contains(text(), '", a, "')]"))
    temp.ano$clickElement()
    
    Sys.sleep(.5)
    
    # extracción de meses
    ddlmes <- remDr$findElement(using = "css", 
                                "#ContentPlaceHolder1_ContentPlaceHolder1_DetallePlaceHolder_ddlMes")
    ddlmes$clickElement()
    
    
    meses <- ddlmes$getElementAttribute("outerHTML")[[1]] %>% 
      read_html() %>% 
      html_nodes("option") %>% 
      html_text()
    
    print(paste("Año:", a))
    
      # ITERAR MESES
      lapply(meses, function(m){
        
        print(paste("Año:", m))
        temp.mes <- remDr$findElement(using = "xpath", 
                                      paste0("//option[contains(text(), '", m, "')]"))
        
        temp.mes$clickElement()
        
        temp.tabla <- remDr$findElement(using = "class",
                                        "table-responsive")
        
        tabla <- temp.tabla$getElementAttribute("outerHTML")[[1]] %>% 
          read_html() %>% 
          html_table() %>% 
          as.data.frame() %>% 
          {
            if(nrow(.)==0){
              data.frame(item = NA, gastos = NA)
            } else {.}
          }
        
        colnames(tabla)[1:2] <- c("item", "gastos")
        tabla$mes <- m
        
        Sys.sleep(.5)
        
        tabla[["Var.3"]] <- NULL
        
        tabla
      }) %>% 
        do.call("rbind", .) %>% 
        as.data.frame() -> temp.gastos
    
    # temp.gastos[["Columnas Var.3"]] <- NULL
      
    temp.gastos$anno <- a
    print("Asigno el año")
    
    print(
      paste(
        "Columnas",
        colnames(temp.gastos)
      )
    )
    
    temp.gastos
    
  }) %>% 
    do.call("rbind", .) %>% 
    as.data.frame() -> temp.conso
  
  
  temp.conso$id <- id
    
  temp.conso
  
}) %>% 
  do.call("rbind", .) %>% 
  as.data.frame() -> consogastos


write.csv2(conso, "gastos_operacionales.csv",
           fileEncoding = "latin1", row.names = F)


# =============================================
# EXTRAER PERSONAL DE APOYO
# ==========================================
driver <- rsDriver(browser = "firefox",
                   chromever = NULL)


remDr <- driver[["client"]]


dips <- read.csv2("base_diputados.csv",
                  encoding = "latin1")


lapply(dips$id, function(id){
  
  tryCatch({
    url <- paste0(
      "https://www.camara.cl/diputados/detalle/personaldepoyo.aspx?prmId=",
      id,
      "#ficha-diputados"
    )
    url
    },
    error = function(e){
      
      Sys.sleep(5)
      
      remDr$deleteAllCookies()
      
      url <- paste0(
        "https://www.camara.cl/diputados/detalle/personaldepoyo.aspx?prmId=",
        id,
        "#ficha-diputados"
      )
      
      return(url)
    }) -> url
  
  print(paste(
    "Navegando en",
    dips$nombre[dips$id==id]
  ))
  
  remDr$navigate(url)
  
  Sys.sleep(1+runif(1))
  
  ddlano <- remDr$findElement(using = "css", 
                              "#ContentPlaceHolder1_ContentPlaceHolder1_DetallePlaceHolder_ddlAno")
  
  annos <- ddlano$getElementAttribute("outerHTML")[[1]] %>% 
    read_html() %>% 
    html_nodes("option") %>% 
    html_text()
  
  # ITERAR AÑOS
  lapply(annos, function(a){
    # 
    ddlano <- remDr$findElement(using = "css",
                                "#ContentPlaceHolder1_ContentPlaceHolder1_DetallePlaceHolder_ddlAno")
    ddlano$clickElement()
    
    Sys.sleep(.5)
    
    # extracción de años
    temp.ano <- remDr$findElement(using = "xpath",
                                  paste0("//option[contains(text(), '", a, "')]"))
    temp.ano$clickElement()
    
    Sys.sleep(.5)
    
    # extracción de meses
    ddlmes <- remDr$findElement(using = "css", 
                                "#ContentPlaceHolder1_ContentPlaceHolder1_DetallePlaceHolder_ddlMes")
    ddlmes$clickElement()
    
    
    meses <- ddlmes$getElementAttribute("outerHTML")[[1]] %>% 
      read_html() %>% 
      html_nodes("option") %>% 
      html_text()
    
    print(paste("Año:", a))
    
    # ITERAR MESES
    lapply(meses, function(m){
      
      print(paste("Año:", m))
      temp.mes <- remDr$findElement(using = "xpath", 
                                    paste0("//option[contains(text(), '", m, "')]"))
      
      temp.mes$clickElement()
      
      temp.tabla <- remDr$findElement(using = "class",
                                      "table-responsive")
      
      tabla <- temp.tabla$getElementAttribute("outerHTML")[[1]] %>% 
        read_html() %>% 
        html_table() %>% 
        as.data.frame() %>% 
        {
          if(nrow(.)==0){
            data.frame(a=NA, b=NA, c=NA, d=NA, e=NA, f=NA)
          } else {.}
        }
      
      tabla[["Var.2"]] <- NULL
      tabla[["Var.3"]] <- NULL
      
      colnames(tabla) <- c("tipo", "nombre", "cargo", "sueldo", "cargo.eleccion", "cese")
      tabla$mes <- m
      
      Sys.sleep(.5)
      tabla
    }) %>% 
      do.call("rbind", .) %>% 
      as.data.frame() -> temp.gastos
    
    # temp.gastos[["Columnas Var.3"]] <- NULL
    
    temp.gastos$anno <- a
    print("Asigno el año")
    
    print(
      paste(
        "Columnas",
        colnames(temp.gastos)
      )
    )
    
    temp.gastos
    
  }) %>% 
    do.call("rbind", .) %>% 
    as.data.frame() -> temp.conso
  
  
  temp.conso$id <- id
  
  Sys.sleep(1+runif(1))
  temp.conso
  
}) %>% 
  do.call("rbind", .) %>% 
  as.data.frame() -> consoapoyo


write.csv2(consoapoyo, "gastos_personal.csv",
           fileEncoding = "latin1", row.names = F)

remDr$close()
remDr$closeServer()
