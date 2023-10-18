library(tidyverse)
install.packages("igraph")
library(igraph)

# Tableau infos indiv
id <- paste0("0", seq(1,9)) 
annee_naiss <- sample(1950:2010, 9)
annee_dec <- annee_naiss + sample(50:90, 9) 
sexe <- sample(c("F", "H"), 9, replace = T)
lieu <- rep(NA, 9)   
name <- rep(NA, 9)
generationFam <- c(rep("Parents", 3), rep("Enfants", 6))
nodes <- data.frame(id, name, annee_naiss, annee_dec, sexe, lieu, generationFam)

# Tableau des liens 
liens <- data.frame(
  Type = c(rep("Alliance", 3), rep("Filiation", 10)),
  id1 = c("01", "01", "03", "01", "02", "01", "02", "01", "03", "01", "03", "03", "04"), 
  id2 = c("02", "03", "04", "05", "05", "06", "06", "07", "07", "08", "08", "09", "09"),
  parents = c(NA, NA, NA, "P1", "P2", "P1", "P2", "P1", "P2", "P1", "P2", "P1", "P2"),
  enfants = c(NA, NA, NA, "E1", "E1", "E2", "E2", "E1", "E1", "E2", "E2", "E1", "E1"),
  statM = c(sample(c("Mariés", "Divorcés", "Concubains", "Pacsés", "Séparés"), 3), 
            rep(NA, 10)),
  statC = c("C", "NC","C", "C", "C", "C", "C", "C", "NC", "C", "NC", "C", "C"))


# On regle la position des noeuds en fonction des génération : 
nodes2 <- nodes %>%
  mutate(
    y_coord = case_when(
      generationFam == "Parents" ~ 0,
      generationFam == "Enfants" ~ -5,
      generationFam == "Grand-parents" ~ 5))
plot(nodes2$y_coord)

# On regarde qui est le plus central chez les parents :
# Qui a le plus de lien d'alliance
liensAlli <- liens %>%
  filter(Type == "Alliance")
ggAll <- graph_from_edgelist(as.matrix(liensAlli[, 2:3]), directed = F)
tab <- as.data.frame(degree(ggAl))
tab <- tab %>%
  mutate(id = rownames(tab), 
         lienAll = degree(ggAl))
tab <- tab[, 2:3]
# Qui a le plus d'enfants ? 
liensFill <- liens %>%
  filter(Type == "Filiation")
ggFill <- graph_from_edgelist(as.matrix(liensFill[, 2:3]), directed = T)
tab2 <- as.data.frame(degree(ggFill))
tab2 <- tab2 %>%
  mutate(id = rownames(tab2), 
         lienFill = degree(ggFill))
tab2 <- tab2[, 2:3]
tab <- left_join(tab, tab2, by = "id")
rm(tab2)
tab <- tab %>% 
  arrange(-lienFill, -lienAll) %>%
  mutate(x_coord = NA)
# Ici on met l'individu au milieu
tab[1, ]$x_coord <- 0


# On compte le nombre d'adelphies et le nb qui les composent 
tab2 <- liens %>%
  filter(Type == "Filiation") %>%
  select(parents, id1, id2) %>%
  pivot_wider(id_cols = "id2", 
              names_from = "parents", 
              values_from = "id1") %>%
  mutate(couple = str_glue("{P1}, {P2}"))
tab2 <- 




  
filter(row.nam)
degP <- data.frame(
  lienAlli = degree(ggAl), 
  lienFill = %>%
  as.data.frame() %>%
  arrange()

degP <-  degree(ggAl) %>%
  as.data.frame() %>%
  arrange()


liens
liens <- liens %>%
  mutate(case_when(
    
  ))







parents <- sample(nodes$id, 3)
enfants <- nodes$id[!(nodes$id %in% parents)]
enfantsC1 <- enfants %>%
  sample(., sample(1:length(enfants)-1, 1))
enfantsC2 <- enfants[!(enfants %in% enfantsC1)]

couples <- data.frame(
  couple = c(paste0(sample(parents, 2), collapse = ", "), 
             paste0(sample(parents, 2), collapse = ", ")), 
  enfants = c(paste0(enfantsC1, collapse = ", "), 
              paste0(enfantsC2, collapse = ", ")),
  statMatri = sample(c("Mariés", "Divorcés", "Concubains", "Pacsés", "Séparés"), 2))
couples 




links <- data.frame(
  parents = sample(parents, length(enfants)*2, replace = T), 
  enfants = sample(rep(enfants, 2), length(enfants)*2)) %>%
  distinct()



### On fixe cette config
nodes2 <- nodes
filiations <- links

couples <- filiations %>%
  arrange(enfants) 
couples$statut <- rep(c("P1", "P2"), length(enfants))
  pivot_wider(id_cols = enfants, names_from = parents)
  

couples <- data.frame(
  I1 = c("04", "04"), 
  I2 = c("02", "03"), 
  statut = c("M", "D"), 
  date_statut = c("2000", "1992"))


make_graph()
ggCouples <- graph_from_edgelist(as.matrix(couples[, c(1,2)]), directed = F)
centre <- degree(ggCouples) %>%
  as.data.frame() %>%
  filter(. == max(degree(ggCouples))) %>%
  row.names()

nodes2 <- nodes2 %>%
  mutate(x_coord = NA, 
         y_coord = NA) %>%
  mutate(x = case_when(id == centre ~ 0), 
         y = case_when(id == centre ~ 0))

gg <- 

plot(gg)
igra


couples
distinct()
dim(tab)[1]

# Qui est au centre



install.packages("questionr")
library(questionr)
freq(tab[, 2])



