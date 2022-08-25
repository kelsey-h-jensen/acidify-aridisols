library(tidyverse)

# load in the data
acid <- read.csv("../acidify-aridisols/data/acidcomparison_alldata.csv", header=T)
acid$ring <- as.factor(acid$ring)
acid$rep <- as.factor(acid$rep)
acid$plant <- as.factor(acid$plant)
acid$treatment <- as.factor(acid$treatment)

acid <- acid %>%
  mutate(DA_mgCgsoil = (DA_perC/100)*1000) %>% 
  mutate(DA_gCm2 = (DA_mgCgsoil*bulkdensity)/10000) %>% 
  mutate(fum_mgCgsoil = (fum_perC/100)*1000) %>% 
  mutate(fum_gCm2 = (fum_mgCgsoil*bulkdensity)/10000) %>% 
  mutate(diff_mgC = (fum_mgCgsoil - DA_mgCgsoil)) %>%
  mutate(diff_isoC = (fum_isoC - DA_isoC)) %>%
  mutate(diff_gCm2 = (fum_gCm2 - DA_gCm2)) %>% 
  mutate(rel_diff = (diff_mgC/fum_mgCgsoil)*100) %>% 
  unite(group, c("plant", "treatment"), remove = "FALSE") %>% 
  unite(ID, c("plant","ring","rep"), remove = "FALSE")


save( acid, file = "../acidify-aridisols/data/acid.Rdata" )

##### pivot long, pivot wide

acid_long <- acid %>% select(-bulkdensity, -diff_mgC, -diff_gCm2, -rel_diff) %>% 
  pivot_longer(where(is.numeric), names_to = "response") %>% 
  separate(response, c("method","response"))

save( acid_long, file = "../acidify-aridisols/data/acid_long.Rdata" )


acid_wide <- acid_long %>% 
  pivot_wider( values_from = value, names_from= response)

save( acid_wide, file = "../acidify-aridisols/data/acid_wide.Rdata" )


