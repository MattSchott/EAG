library(dplyr)
library(ggplot2)
library(car)

my_palette <- c(
  "#08306B", "#2171B5", "#6BAED6",
  "#FFD000", "#FFA500", "#D94801",
  "#CB181D", "#67000D"
)

#Rohdaten einladen
DATA<-read.csv("csv/DM_pos_neg.csv",header = T)
getwd()

#Spalte Animal hinzufügen, Traces anhand des Dateinamens richtigem Animal zuordnen
DATA$Animal<-NA
for (i in 1:length(DATA$Area)) {
  DATA$Animal[i]<-strsplit(DATA$File[i],"-")[[1]][1]
}
#Anzahl der Tiere in Animals speichern
Animals<-unique(DATA$Animal)

#Noisekorrektur der Peakheight
DATA$H_Nhalf = DATA$Height - DATA$Noise/2
table(DATA$File) #each file has 5 peaks

#wird später für Korrektur um Pentenhöhe verwendet
Pentenheight<-NULL
DATA$CorrH<-NA

#für jedes Tier den Median der ersten 5 Peaks in Pentenheight speicher
for (i in 1:length(Animals)){
  Pentenheight<-c(Pentenheight,median(DATA$Height[which(DATA$Animal==Animals[i])][1:5]))
}
#median aller Pentenheights in CorrH speichern
CorrH<-median(Pentenheight)

#for each animal: median of the first 5 heights, assign a correction factor = 1 / median, multiply heights by that correction factor
for (i in 1:length(Animals)){
DATA$CorrH[which(DATA$Animal==Animals[i])] <-CorrH/median(DATA$Height[which(DATA$Animal==Animals[i])][1:5])
}
DATA$CorrHeights<-DATA$Height*DATA$CorrH

# create DATA2: für jedes Treatment die Mediane bei jedem Animal speichern
DATA2 = DATA %>% group_by(File) %>% summarise(median = median(H_Nhalf), medianHeight = median(Height), Treatment = Treatment[1], Animal = Animal[1])


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

#überschreibe CorrH mit dem median aus medianheight --> um Referenzwert für gesamten Datensatz zu erlangen
CorrH<-median(DATA2$medianHeight)
for (i in 1:length(Animals)){
  DATA2$CorrH[which(DATA2$Animal==Animals[i])] <-CorrH/DATA2$medianHeight[which(DATA2$Animal==Animals[i])][1]
}
DATA2$CorrHeights<-DATA2$medianHeight*DATA2$CorrH

#extract the number of each trace per animal
DATA2$FileNr <-NA
for(i in 1:nrow(DATA2)){
  DATA2$FileNr[i]<-strsplit(DATA2$File[i],"-")[[1]][2]
  DATA2$FileNr[i]<-strsplit(DATA2$FileNr[i],".A")[[1]][1]
}
DATA2$FileNr <-as.numeric(DATA2$FileNr)

# sicherstellen, dass Treatment & Animal Faktoren sind
DATA2$Treatment <- as.factor(DATA2$Treatment)
DATA2$Animal    <- as.factor(DATA2$Animal)

# #test for Homoskedastizität
# leveneTest(medianHeight ~ Treatment, data = DATA2)
# 
# # t-Test (gleiches Varianz angenommen)
# t_test_result <- t.test(medianHeight ~ Treatment,
#                         data = DATA2,
#                         var.equal = TRUE)
# 
# t_test_result
# 
# # Welch-Test
# t.test(medianHeight ~ Treatment, data = DATA2)

#Test auf Normalvereteilung 
shapiro.test(DATA2$medianHeight[DATA2$Treatment == "1-penten-3ol 100µg/µl"])

# keine Normalverteilung, deshalb Wilcox
wicox.test_result <- wilcox.test(medianHeight ~ Treatment, data = DATA2)


# extract the p value
pval <- wicox.test_result$p.value

# get the two treatment levels
groups <- levels(DATA2$Treatment)

# decide letters
if (pval < 0.05) {
  letters_df <- data.frame(
    Treatment = groups,
    letter = c("a", "b")
  )
} else {
  letters_df <- data.frame(
    Treatment = groups,
    letter = c("a", "a")
  )
}

# y positions (top of each box)
y_pos <- aggregate(medianHeight ~ Treatment, DATA2, max)

letters_df <- merge(letters_df, y_pos, by = "Treatment")

# small offset so the text is above the box
letters_df$y <- letters_df$medianHeight +
  0.05 * diff(range(DATA2$medianHeight))

# Plot mit Buchstaben direkt über den Boxplots

ggplot(DATA2, aes(x = Treatment, y = medianHeight)) +
  geom_boxplot(outlier.shape = NA,fill = "lightgray",lwd=0.7) +
  geom_jitter(aes(color = Animal),   # Punkte nach Animal einfärben
              width = 0.1, size = 3,pch=19,
              show.legend = FALSE) +
  geom_text(data = letters_df,size = 6,
            aes(x = Treatment,
                y = 2000,
                label = letter),
            inherit.aes = FALSE) +
  scale_color_manual(values = c("#08306B",  # sehr dunkles Navy-Blau
                                "#2171B5",  # kräftiges Mittelblau
                                "#6BAED6",  # helles, aber noch deutliches Blau
                                "#FFD000",  # sehr kräftiges Gelb
                                "#FFA500",  # gelbstichiges, helles Orange
                                "#D94801",  # rötlich-dunkles Orange        
                                "#CB181D",  # kräftiges Rot
                                "#67000D"  ))+ # sehr dunkles Rot
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
ggsave("plots/DM-pos_neg.png")
dev.off()
