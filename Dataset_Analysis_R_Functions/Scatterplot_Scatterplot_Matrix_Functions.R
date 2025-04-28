

############## OKTAY CAN SEV??MG??N / oktaycansevimgin2022@gmail.com ########

# *********************Scatter plot ************************
# user variables = (varM,varN)
dataset = read.table(file = "C:/Users/Oktay Can/Desktop/Dataset/DatasetNA.txt", header = TRUE)

varM <- dataset$Var1[!is.na(dataset$Var1)]
varM <- as.numeric(gsub(",", ".", varM))

varN <- dataset$Var2[!is.na(dataset$Var2)]
varN <- as.numeric(gsub(",", ".", varN))

plot.new()

min_varM <- varM[1]
max_varM <- varM[1]
for(i in 2:length(varM)) {
  if(varM[i] < min_varM) min_varM <- varM[i]
  if(varM[i] > max_varM) max_varM <- varM[i]
}

min_varN <- varN[1]
max_varN <- varN[1]
for(i in 2:length(varN)) {
  if(varN[i] < min_varN) min_varN <- varN[i]
  if(varN[i] > max_varN) max_varN <- varN[i]
}

varxlim <- c(min_varM, max_varM)
varylim <- c(min_varN, max_varN)

lines(c(varxlim[1], varxlim[2]), c(0, 0), lwd = 2)
lines(c(0, 0), c(varylim[1], varylim[2]), lwd = 2)

plot(varM, varN, xlab = "VarM", ylab = "VarN", main = "Scatterplot")

lines(c(varxlim[1], varxlim[2]), c(0, 0), lwd = 2)
lines(c(0, 0), c(varylim[1], varylim[2]), lwd = 2)

varA <- dataset$Var1[!is.na(dataset$Var1)]
varA <- as.numeric(gsub(",", ".", varA))

varB <- dataset$Var2[!is.na(dataset$Var2)]
varB <- as.numeric(gsub(",", ".", varB))

varC <- dataset$Var4[!is.na(dataset$Var4)]
varC <- as.numeric(gsub(",", ".", varC))

n1 <- length(varA)
n2 <- length(varB)
n3 <- length(varC)

if (n1 != n2 || n1 != n3) {
  stop("Vectors are not the same length.")
}

data <- data.frame(varA, varB, varC)

par(mfrow = c(3, 3))

for (i in 1:ncol(data)) {
  for (j in 1:ncol(data)) {
    plot(data[, i], data[, j], xlab = colnames(data)[i], ylab = colnames(data)[j], main = paste(colnames(data)[i], "vs", colnames(data)[j]))
  }
}

par(mfrow = c(1, 1))

