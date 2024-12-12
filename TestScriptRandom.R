FilopResults1 <- read_csv("TestOutput/FilopResults1.csv")
FilopResults1 <- FilopResults1 %>% rename(FiloID = ...1)

JunctionResults <- read_csv("TestOutput/JuncResults1.csv")
JunctionResults <- JunctionResults %>% rename(JuncID = ...1)
TotalJunctionLength <- JunctionResults %>% summarise(TotalLength = sum(Length))
Adjacency <- read_csv("TestOutput/AdjacencyResults.csv")
Adjacency <- Adjacency %>% separate(Label, c("Filo", "Junc"), "_") %>% mutate(FiloID = extract_numeric(Filo)) %>% mutate(JuncID =extract_numeric(Junc))

newData <- left_join(FilopResults1, Adjacency, by="FiloID")

JunctionAngle <- JunctionResults[1]
JunctionAngle$FeretAngle <- JunctionResults$FeretAngle

test <- left_join(newData, JunctionAngle, by="JuncID")
test <- test %>% mutate(Perpendicular = case_when((between(FeretAngle.x, FeretAngle.y-30, FeretAngle.y+30) ~ "yes"), TRUE ~ "no"))

PlotData <- test %>% filter(Perpendicular == "yes" & `Dist min EdgeA-EdgeB` < 5)

# Plots Length ------------------------------------------------------------

FilLength <- ggplot(PlotData, aes(x = Group, y = Length, group = Group, color= Group, fill=Group)) +
  geom_violin(size=0.6, alpha = .5, trim=FALSE)+
  #geom_point(alpha = .5, position = position_jitterdodge()) + 
  geom_boxplot(width=0.05, alpha = 0.5) +
  theme(legend.position = "none") + 
  scale_color_manual(values= CellLineColor) +
  scale_fill_manual(values=CellLineColor) +
  ggtitle("Filopodia length by Cell Type") +
  theme_unique_dark()  
FilLength