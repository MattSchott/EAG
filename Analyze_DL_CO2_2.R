library(dplyr)
library(ggplot2)
library(multcompView)
library(car)

my_palette2 <- c(
  "#2171B5","#FFA500","#67000D"
)

DATA<-read.csv("csv/DL27,28,30_CO2_new.csv",header = T)
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

DATA2$Treatment <- factor(DATA2$Treatment, levels = c("1-penten-3ol vor CO2", "negativ control vor CO2", "1-penten-3ol CO2", "negativ control CO2","1-penten-3ol nach CO2"))

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

# Ursprüngliche Werte (wie sie in DATA2$Treatment vorkommen)
orig_levels <- c(
  "1-penten-3ol vor CO2",
  "1-penten-3ol CO2",
  "1-penten-3ol nach CO2"
)

# Beschriftung mit \n an gewünschten Stellen
new_labels <- c(
  "before",
  "during",
  "after"
)

# subset erstellen nur Treatments mit Penten
DATA_penten <- subset(DATA2, grepl("1-penten-3ol", Treatment))
DATA_penten$Treatment <- factor(DATA_penten$Treatment, levels = orig_levels, labels = new_labels)

leveneTest(medianHeight ~ Treatment, data = DATA_penten)
shapiro.test(DATA2$medianHeight[DATA2$Treatment == "1-penten-3ol vor CO2"])
shapiro.test(DATA2$medianHeight[DATA2$Treatment == "1-penten-3ol CO2"])
shapiro.test(DATA2$medianHeight[DATA2$Treatment == "1-penten-3ol nach CO2"])

# Positivkontrolle vs Treatment1
data1 <- subset(DATA_penten, Treatment %in% c("before", "during"))

# Positivkontrolle vs Treatment2
data2 <- subset(DATA_penten, Treatment %in% c("before", "after"))

# Positivkontrolle vs Treatment1
t_test1 <- t.test(medianHeight ~ Treatment, data = data1)
t_test1

# Positivkontrolle vs Treatment2
t_test2 <- t.test(medianHeight ~ Treatment, data = data2)
t_test2

# p-Werte
p_values <- c(
  before_during = t_test1$p.value,
  before_after = t_test2$p.value
)
p_values

# Adjust p-values using Bonferroni correction
p_adjusted <- p.adjust(p_values, method = "bonferroni")

# Print the original and adjusted p-values
results <- data.frame(
  Comparison = c("before vs during", "before vs after"),
  p_value = p_values,
  p_value_adjusted = p_adjusted
)
print(results)
  
# Behandle alle Gruppen: before, during, after
groups <- c("before", "during", "after")

# Symmetrische Matrix für multcompLetters
p_matrix <- matrix(1, nrow = length(groups), ncol = length(groups))
rownames(p_matrix) <- colnames(p_matrix) <- groups

# fülle die p-Werte
p_matrix["before","during"] <- p_values["before_during"]
p_matrix["during","before"] <- p_values["before_during"]

p_matrix["before","after"] <- p_values["before_after"]
p_matrix["after","before"] <- p_values["before_after"]

# Buchstaben generieren
letters <- multcompLetters(p_matrix, threshold = 0.05)
letters$Letters

letters_df <- data.frame(
  Treatment = names(letters$Letters),
  letter = letters$Letters
)

ggplot(DATA_penten, aes(x = Treatment, y = medianHeight)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgray", color = "black",lwd=0.7) +
  geom_jitter(aes(color = Animal), width = 0.00, size = 3, alpha = 0.8) +
  geom_text(data = letters_df, size = 6, aes(x = Treatment, y = -100 + 0.2, size = 6, label = letter)) +
  ylim(-10000, 0000) +
  labs(
    title = "",
    x = "anesthesia",
    y = "median peak height [nV]",
    color = "Animal"
  ) +
  scale_color_manual(values = my_palette2) +
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
                                margin = margin(r = 12)),  # Abstand rechts
    legend.position = "none"
  )
ggsave("plots/DL27-30_CO2_onlyPenten.png")
dev.off()
