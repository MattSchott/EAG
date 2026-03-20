library(dplyr)
library(multcomp)
library(ggplot2)

my_palette <- c(
  "#08306B", "#2171B5", "#6BAED6",
  "#FFD000", "#FFA500", "#D94801",
  "#CB181D", "#67000D"
)

DATA<-read.csv("csv/DM_roach_7007b.csv",header = T)
getwd()

DATA$Animal<-NA
for (i in 1:length(DATA$Area)) {
  DATA$Animal[i]<-strsplit(DATA$File[i],"-")[[1]][1]
}
Animals<-unique(DATA$Animal)

DATA$H_Nhalf = DATA$Height - DATA$Noise/2
table(DATA$File) #each file has 5 peaks

Pentenheight<-NULL
DATA$CorrH<-NA

for (i in 1:length(Animals)){
  Pentenheight<-c(Pentenheight,median(DATA$Height[which(DATA$Animal==Animals[i])][1:5]))
}
CorrH<-median(Pentenheight)
for (i in 1:length(Animals)){
  DATA$CorrH[which(DATA$Animal==Animals[i])] <-CorrH/median(DATA$Height[which(DATA$Animal==Animals[i])][1:5])
}
DATA$CorrHeights<-DATA$Height*DATA$CorrH

(DATA2 = DATA %>% group_by(File) %>% summarise(median = median(H_Nhalf), medianHeight = median(Height), Treatment = Treatment[1], Animal = Animal[1]))


# Extract the first medianHeight value for each animal
first_median_heights <- sapply(Animals, function(animal) {
  DATA2$medianHeight[which(DATA2$Animal == animal)][1]
})

# Compute CorrH using the median of these first values
DATA2$CorrH <- median(first_median_heights)

# Compute correction factor for each animal
for (i in 1:length(Animals)) {
  DATA2$CorrH[which(DATA2$Animal == Animals[i])] <- CorrH / DATA2$medianHeight[which(DATA2$Animal == Animals[i])][1]
}

# Apply the correction to medianHeight
DATA2$CorrHeights <- DATA2$medianHeight * DATA2$CorrH

CorrH<-median(DATA2$medianHeight)
for (i in 1:length(Animals)){
  DATA2$CorrH[which(DATA2$Animal==Animals[i])] <-CorrH/DATA2$medianHeight[which(DATA2$Animal==Animals[i])][1]
}
DATA2$CorrHeights<-DATA2$medianHeight*DATA2$CorrH

DATA2$FileNr <-NA
for(i in 1:nrow(DATA2)){
  DATA2$FileNr[i]<-strsplit(DATA2$File[i],"-")[[1]][2]
  DATA2$FileNr[i]<-strsplit(DATA2$FileNr[i],".A")[[1]][1]
}
DATA2$FileNr <-as.numeric(DATA2$FileNr)


DATA3 <- DATA2 %>%
 group_by(Animal)%>%
  arrange(FileNr) %>%                    # Gruppiere nach Tier und Behandlung
  filter(row_number() < n()) %>% 
  #slice_head(n = nrow() - 1) %>%               # Nimm die ersten 5 Zeilen jeder Gruppe
  ungroup()   %>% 
  arrange(File)

# sicherstellen, dass Treatment & Animal Faktoren sind
DATA3$Treatment <- as.factor(DATA3$Treatment)
DATA3$Animal    <- as.factor(DATA3$Animal)

# für jede Treatment-Gruppe den maximalen y-Wert berechnen
y_pos <- DATA3 %>%
  group_by(Treatment) %>%
  summarise(y = max(medianHeight), .groups = "drop")

# Model
fit2 <- lm(log(abs(medianHeight+1)) ~ Treatment + Animal, data = DATA3)
plot(fit2)

# Tukey Posthoc
tuk2 <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
cld_res2 <- cld(tuk2)
summary(tuk2)

#Model results
summary(fit2)

# Buchstaben extrahieren
letters_df <- data.frame(
  Treatment = names(cld_res2$mcletters$Letters),
  Letters   = cld_res2$mcletters$Letters
)

# für jede Treatment-Gruppe den maximalen y-Wert berechnen
y_pos <- DATA3 %>%
  group_by(Treatment) %>%
  summarise(y = max(medianHeight), .groups = "drop")

# Buchstaben-Dataframe mit y-Positionen verbinden
letters_df <- merge(letters_df, y_pos, by = "Treatment")

# Plot mit Buchstaben direkt über den Boxplots


ggplot(DATA3, aes(x = Treatment, y = medianHeight)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgray",lwd=0.7) + 
  geom_jitter(aes(color = Animal),   # Punkte nach Animal einfärben
              width = 0.07, size = 3, pch = 19,
              show.legend = FALSE) +  
  geom_text(data = letters_df,size = 6,
            aes(x = Treatment,
                y = 2000, 
                label = Letters),
            inherit.aes = FALSE) +
  scale_color_manual(values = my_palette) +
  theme_classic() +   # weißer Hintergrund, kein Gitternetz
  theme(
    axis.line = element_line(color = "black", size = 1.0),
    axis.ticks = element_line(color = "black", size = 1.0),
    # Tick-Beschriftungen
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),# Achsen schwarz
    panel.border = element_blank(),              # kein Rahmen
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 16, color = "black",
                                margin = margin(t = 12)),  # Abstand oben
    axis.title.y = element_text(size = 16, color = "black",
                                margin = margin(r = 12))  # Abstand rechts
  )+
  labs (
    y = "median peak height [nV]",
    x = "treatment"
  )
ggsave("plots/DM_Roach4.png")
dev.off()
