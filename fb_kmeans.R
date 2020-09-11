library(cluster)
library(factoextra)
library(forcats)
library(kableExtra)
library(tidyverse)
conversionTable <- read.csv("devaiwomen_files/KAG_conversion_data.csv")
colnames(conversionTable) <- c("id_anuncio", "id_campanha", "id_campanha_fb", "idade", "genero", "interesse", "impressoes", "cliques", "custo", "conversoes_totais", "conversoes_aprovadas")

set.seed(1993)

clustering <- conversionTable %>% 
  select(impressoes:conversoes_aprovadas) %>% 
  mutate(across(is.numeric, ~ sqrt(.x))) %>% 
  mutate(cpi = custo/impressoes,
         cpe = custo/conversoes_totais,
         cpc = custo/conversoes_aprovadas) %>% 
  mutate(across(everything(), ~ ifelse(is.finite(.x), .x, 0)))

summary(clustering)

fviz_nbclust(x = clustering, FUNcluster = kmeans, method = 'silhouette' )


p_conversionTable <- conversionTable %>% 
  select(idade,genero, interesse,	impressoes,	cliques,	custo,	conversoes_totais,	conversoes_aprovadas) %>% 
  pivot_longer(cols = impressoes:conversoes_aprovadas,names_to = "variables", values_to = "value")

ggplot(p_conversionTable, aes(x = genero, y = value, color = genero, fill = genero)) +
  geom_col() +
  facet_wrap(variables ~ ., ncol=5,scale="free_y")

ggplot(p_conversionTable, aes(x = idade, y = value, color = idade, fill = idade)) +
  geom_col() +
  facet_wrap(variables ~ ., ncol=5,scale="free_y")


ready <- kmeans(clustering, 2)
ready$centers
ready$size

clustering_p <- cbind(1:2,ready$centers)
colnames(clustering_p)[1] <- "cluster"
clustering_p <- as.tibble(clustering_p)
p_ready <- clustering_p %>% 
  pivot_longer(cols = impressoes:conversoes_aprovadas,names_to = "variable", values_to = "value")

ggplot() + 
  geom_line(data = p_ready, aes(x = as.factor(cluster), y = value, color = variable, group = variable), size = 1, show.legend = FALSE) +
  facet_wrap(variable ~ ., ncol=5 ,scale="free_y")

p_ready_custo <- clustering_p %>% 
  pivot_longer(cols = cpi:cpc,names_to = "variable", values_to = "value")

ggplot() + 
  geom_line(data = p_ready_custo, aes(x = as.factor(cluster), y = value, color = variable, group = variable), size = 1, show.legend = FALSE) +
  facet_wrap(variable ~ ., ncol=5 ,scale="fixed")


conversionTable_cluster <- cbind(conversionTable, ready$cluster)
colnames(conversionTable_cluster)[12] <- "cluster"
conversionTable_cluster$interesse <- fct_infreq(as.factor(conversionTable_cluster$interesse))
conversionTable_cluster$idade <- fct_infreq(as.factor(conversionTable_cluster$idade))



# interests ---------------------------------------------------------------

plot_interests_conversoes_totais <- conversionTable_cluster %>% 
  group_by(interesse) %>% 
  mutate(n_conv = sum(conversoes_totais)) %>% 
  ungroup() %>% 
  filter(n_conv >= mean(n_conv)) %>% 
  select(-n_conv) %>% 
  mutate(interesse = fct_infreq(as.factor(interesse)),
         cpi = custo/impressoes,
         cpe = custo/conversoes_totais,
         cpc = custo/conversoes_aprovadas) %>% 
  mutate(across(is.numeric, ~ ifelse(is.finite(.x), .x, 0)))

p_conv_1 <- ggplot(plot_interests_conversoes_totais, aes(x = as.factor(cluster), y = conversoes_totais, color = genero, fill = genero)) +
  geom_col(position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5)

p_impr_1 <- ggplot(plot_interests_conversoes_totais, aes(x = as.factor(cluster), y = impressoes, color = genero, fill = genero)) +
  geom_col(position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5)

p_custo_1 <- ggplot(plot_interests_conversoes_totais, aes(x = as.factor(cluster), y = cpi, color = genero, fill = genero)) +
  geom_col(position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5)
p_custo_3 <- ggplot(plot_interests_conversoes_totais, aes(x = as.factor(cluster), y = cpe, color = genero, fill = genero)) +
  geom_col(position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5)
p_custo_5 <- ggplot(plot_interests_conversoes_totais, aes(x = as.factor(cluster), y = cpc, color = genero, fill = genero)) +
  geom_col(position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5)

plot_interests_conversoes_aprov <- conversionTable_cluster %>% 
  group_by(interesse) %>% 
  mutate(n_conv_aprov = sum(conversoes_aprovadas)) %>% 
  ungroup() %>% 
  filter(n_conv_aprov >= mean(n_conv_aprov)) %>% 
  select(-n_conv_aprov) %>% 
  mutate(interesse = fct_infreq(as.factor(interesse)),
         cpi = custo/impressoes,
         cpe = custo/conversoes_totais,
         cpc = custo/conversoes_aprovadas)

p_conv_2 <- ggplot(plot_interests_conversoes_aprov, aes(x = as.factor(cluster), y = conversoes_aprovadas, color = genero, fill = genero)) +
  geom_col(position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5)

p_custo_2 <- ggplot(plot_interests_conversoes_aprov, aes(x = as.factor(cluster), y = cpi, color = as.factor(cluster), fill = as.factor(cluster))) +
  geom_col(position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5)+
  theme_bw()
p_custo_4 <- ggplot(plot_interests_conversoes_aprov, aes(x = as.factor(cluster), y = cpe, color = as.factor(cluster), fill = as.factor(cluster))) +
  geom_col(position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5)+
  theme_bw()
p_custo_6 <- ggplot(plot_interests_conversoes_aprov, aes(x = as.factor(cluster), y = cpc, color = as.factor(cluster), fill = as.factor(cluster))) +
  geom_col(position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5) +
  theme_bw()

gridExtra::grid.arrange(p_conv_1, p_impr_1, p_conv_2)

gridExtra::grid.arrange(p_custo_1, p_custo_3, p_custo_5)

gridExtra::grid.arrange(p_custo_2, p_custo_4, p_custo_6)




# Idade -------------------------------------------------------------------

ggplot(conversionTable_cluster, aes(x = as.factor(cluster), color = genero, fill = genero)) +
  geom_histogram(stat = "count", position = "dodge") +
  facet_wrap(as.factor(idade) ~., ncol=5)

aprovadas <- conversionTable_cluster %>% 
  filter(conversoes_aprovadas > 0)

totais <- conversionTable_cluster %>% 
  filter(conversoes_totais > 0)

p_final_int <- ggplot(aprovadas, aes(x = as.factor(cluster), color = genero, fill = genero)) +
  geom_histogram(stat = "count", position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5) +
  labs(title = "Conversões Finais")

p_final_idade <- ggplot(aprovadas, aes(x = as.factor(cluster), color = genero, fill = genero)) +
  geom_histogram(stat = "count", position = "dodge") +
  facet_wrap(as.factor(idade) ~., ncol=5) +
  labs(title = "Conversões Finais")

p_pre_int <- ggplot(totais, aes(x = as.factor(cluster), color = genero, fill = genero)) +
  geom_histogram(stat = "count", position = "dodge") +
  facet_wrap(as.factor(interesse) ~., ncol=5) +
  labs(title = "Engajamentos")

p_pre_idade <- ggplot(totais, aes(x = as.factor(cluster), color = genero, fill = genero)) +
  geom_histogram(stat = "count", position = "dodge") +
  facet_wrap(as.factor(idade) ~., ncol=5) +
  labs(title = "Engajamentos")

gridExtra::grid.arrange(
  p_final_idade,
  p_pre_idade
)
