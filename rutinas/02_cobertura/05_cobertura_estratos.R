
# en especif hay 53 codigos a 6 dig 
n_distinct(especif$ciiu_rev_4_clase)
#  en la canasta enviada para la muestra hay 55 codo a 6 digitos
 n_distinct(canasta$codigo_actividad_eco) 

 # en especif estan todos los cod a 6 digitos de la canasta
 especif %>% 
   filter(ciiu_rev_4_clase %in% canasta$codigo_actividad_eco) %>% 
   filter(!duplicated(ciiu_rev_4_clase)) %>% 
   dim()
 
 # en la canasta no están 2 de los cod a 6 dig que estan en especif
 canasta %>% 
   filter(!codigo_actividad_eco %in% especif$ciiu_rev_4_clase) %>% 
   dim()

 #Los 50 cod-6-dig que aparecen en la cobertura están en la canasta
 
 n_distinct(bdd_cober_01$codigo_actividad_eco)
 
 bdd_cober_01 %>% 
   group_by(codigo_actividad_eco) %>% 
   summarise() %>% 
   filter(codigo_actividad_eco %in% canasta$codigo_actividad_eco) %>% 
   dim()
 
 # en la cobertura faltan 5 de los que estan en la canasta
 table(bdd_cober_01$codigo_actividad_eco) 
 
 canasta %>% 
   filter(!codigo_actividad_eco %in% bdd_cober_01$codigo_actividad_eco)

 # ----------------------------------------------------------------------------
 
 table(s1$codigo_actividad_eco) %>% 
   data.frame() %>%
   full_join(table(bdd_cober$codigo_actividad_eco) %>% 
               data.frame() %>% 
               rename(Freq_muestra = Freq)) %>%
   mutate(diferencia = Freq_muestra - Freq, 
          efectividad = Freq/Freq_muestra) %>% 
   View()
 

 s1 <- bdd_cober %>% filter(total_de_productos!=0)

 
 
  