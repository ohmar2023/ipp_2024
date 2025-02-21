
source("rutinas/02_cobertura/999_librerias.R")


#-------------------------------------------------------------------------------
# LECTURA DEL LISTADO DE CLASES
#-------------------------------------------------------------------------------
# Nueva canasta enviada ---
canasta <- read_excel("insumos/02_listado_act/Actividades_CAB-SIPP_12092024.xlsx") %>% 
  mutate(codigo_actividad_eco = str_replace(codigo_actividad_eco,"[.]",""))

#-------------------------------------------------------------------------------
# LECTURA DEL LISTADO DE CLASES Y PRODICTOS
#-------------------------------------------------------------------------------

especif <- read_excel("insumos/04_cobertura/Matriz_nacional_de_especificaciones_CABSIPP_2024_09_01.xlsx") %>% 
  clean_names() %>% 
  filter(ciiu_rev_4_seccion == "C") %>% 
  mutate(ciiu_rev_4_clase = str_replace_all(ciiu_rev_4_clase,"[.]",""),
         cod_articulo_cpc_11_dig = str_replace_all(cod_articulo_cpc_11_dig,pattern = "[.]","")) %>% 
  filter(ciiu_rev_4_clase %in% canasta$codigo_actividad_eco) 

# Productos que aparecen en la canasta y no fueron levantados en la muestra: 46
nov_1 <- especif %>% 
  filter(!cod_articulo_cpc_11_dig %in% bdd_cober_01$productos_cod) %>% 
  select(cod_articulo_cpc_11_dig)

# Productos que no estan en la canasta y aparecen en la muestra: 192
nov_2 <- bdd_cober_01 %>% 
  filter(!is.na(productos_cod)) %>% 
  filter(!productos_cod %in% especif$cod_articulo_cpc_11_dig) %>% 
  select(productos_cod) %>% 
  filter(!duplicated(productos_cod))

# especif tiene 180 productos de los cuales:
# 134 fueron levantados y 46 no fueron levantados
n_distinct(especif$cod_articulo_cpc_11_dig)  

# En la cobertura existen 228 productos levantados.
# 134 fueron levantados
# 94 son productos que no estan en especif
n_distinct(bdd_cober_01$productos_cod)


wb <- createWorkbook("Novedades del listado de productos")
addWorksheet(wb, "prod_no_lev")
addWorksheet(wb, "prod_no_lista")

writeData(wb, sheet = "prod_no_lev", nov_1)
writeData(wb, sheet = "prod_no_lista", nov_2)

saveWorkbook(wb, paste0("intermedios/02_novedades/novedades_listado_canasta.xlsx"), overwrite = T)






