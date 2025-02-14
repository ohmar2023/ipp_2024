
rm(list = ls())

source("rutinas/02_cobertura/999_librerias.R")

#--------------------------------------------------------------------------
# LECTURA DE BASES DE DATOS -----------------------------------------------
#--------------------------------------------------------------------------

resultados_muestreo <- read_excel("insumos/04_cobertura/Resultados_muestreo_CAB-SIPP.xlsx") %>% 
  clean_names()

#--------------------------------------------------------------------------
# Depuración de la base de datos: -----------------------------------------
# Se pretende quedarnos con un consolidado de productos solo en 5 columnas
# (producto 5). Funciona para todos los casos, salvo para la empresa con 
# id: 13735118115 que tiene 4 productos pero al momento de correr el codigo
# lo deja solo en dos productos, esto se debe a que el algoritmo va verificando
# variable a variable y si la de la derecha es NA deja la de la izq, en el caso
# de la empresa tiene valor en ambos lados por lo que solo se queda con lo de la
# izq. Modifiqué a mano la base de obertura moviendo los prodctos de la derecha,
# esto es: los prod de la derecha en vez de que aparezcan en prod_1 y prod_2
# ahora aprecen en prod_3 y prod_4. Si fuesen mas casos como este, tocaría 
# modificar el algoritmo, como solo es un caso se procede sin problema a resolverlo
# de esta manera. Estas novedades aparecen en el control de la linea 46 del 
# scrpit "02_novedades".
#--------------------------------------------------------------------------

n_v_1 <- names(resultados_muestreo)
bdd_cober <- resultados_muestreo

for (i in c(46:75)) {
  bdd_cober <- bdd_cober %>% 
    mutate( !!n_v_1[i] :=  ifelse(is.na(.data[[n_v_1[i]]]), .data[[n_v_1[i+30]]], .data[[n_v_1[i]]]))
}

bdd_cober <- bdd_cober %>% select(c(1:75,106,107)) %>% 
  select(!contains("fecha")) %>% 
  select(!contains("precio")) %>% 
  select(!starts_with("producto_")) %>% 
  select(-c(4,5,12:30, 33, 34, 38:43))

v_aux <- c(16:20)
colnames(bdd_cober)[v_aux] <- substr(names(bdd_cober)[v_aux], 1, nchar(names(bdd_cober)[v_aux])-3)

bdd_cober <- bdd_cober %>% 
  mutate(codigo_cpc_11_digitos_producto_1 = gsub(pattern = "[.]", replacement = "", codigo_cpc_11_digitos_producto_1),
         codigo_cpc_11_digitos_producto_2 = gsub(pattern = "[.]", replacement = "", codigo_cpc_11_digitos_producto_2),
         codigo_cpc_11_digitos_producto_3 = gsub(pattern = "[.]", replacement = "", codigo_cpc_11_digitos_producto_3),
         codigo_cpc_11_digitos_producto_4 = gsub(pattern = "[.]", replacement = "", codigo_cpc_11_digitos_producto_4),
         codigo_cpc_11_digitos_producto_5 = gsub(pattern = "[.]", replacement = "", codigo_cpc_11_digitos_producto_5))

#--------------------------------------------------------------------------
# Exportando --------------------------------------------------------------
#--------------------------------------------------------------------------

export(bdd_cober, "intermedios/01_base_cobertura/bdd_cober.xlsx")




