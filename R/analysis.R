# === Analysis ===
# ACM
# CAH
# MDS

# Loading
library("readxl")
# xls file
df <- read_excel("experiments.xlsx")
head(df)

library(FactoMineR)

# === ACM ===
res.mca <- MCA(df)

eig.val <- res.mca$eig
barplot(eig.val[, 2],
        names.arg = 1:nrow(eig.val),
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2],
      type = "b", pch = 19, col = "red")

plot(res.mca, autoLab = "yes")

# faire nuage des individus !

# === CAH ===
#res.hcpc = HCPC(res.mca)




