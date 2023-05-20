
# Import  the bibliometrix librabry for R
library(bibliometrix)

# To run Bibilioshiny - this opens in a browser
biblioshiny()


# file <- c("C:/Users/Admin/Desktop/Econometrics_TEP_benchmark/Paper/Review_Round_1/bibliometrics/WoS - chemical processing plant control.txt")
# 
# M <- convert2df(file = file, dbsource = "wos", format = "plaintext")
# 
# results <- biblioAnalysis(M, sep = ";")
# 
# S <- summary(object = results, k = 10, pause = FALSE)
# 
# plot(x = results, k = 10, pause = FALSE)
# 
# # Create keyword co-occurrences network
# 
# NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
# 
# # Plot the network
# net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
# 
# 
# threeFieldsPlot(M, fields=c("CR","AU","DE"))

