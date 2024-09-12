
tamanio %>% filter(grepl(dominio,pattern = "C") ) %>% 
  ggplot(aes(N,ventas))+
  geom_point(aes(colour = factor(dominio)))

names(tamanio)

marco %>% filter(grepl(dom_2,pattern = "C")) %>% 
  ggplot(aes(ventas_totales))+
  geom_histogram(aes(colour = factor(dom_2))) +
facet_wrap(facets = vars(tamanou_plazas))+
  scale_fre
  
