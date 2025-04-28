
############## OKTAY CAN SEV??MG??N / oktaycansevimgin2022@gmail.com ########

#(Cross-products, Covariance, Correlations)

# Cross Products

# user variables = (x,y)
dataset = read.table(file = "C:/Users/Oktay Can/Desktop/Dataset/DatasetNA.txt", header = TRUE)                          

x <- na.omit(as.numeric(gsub(",", ".", dataset$Var4)))
y <- na.omit(as.numeric(gsub(",", ".", dataset$Var5)))

x <- x[1:6]
y <- y[1:6]

x_matris <- matrix(x, nrow = 6, ncol = 1)
y_matris <- matrix(y, nrow = 1, ncol = 6)

cross_products <- x_matris %*% y_matris

print(cross_products)

# Covariance

# user variables = (varx,vary)
varx <- na.omit(as.numeric(gsub(",", ".", dataset$Var1)))
vary <- na.omit(as.numeric(gsub(",", ".", dataset$Var2)))


total_values <- 0

for(i in seq_along(varx)) {
  
  total_values <- total_values + 1
}

n1<- total_values

total_values <- 0

for(i in seq_along(vary)) {
  
  total_values <- total_values + 1
}

n2<- total_values
if (n1 != n2) {
  stop("Vectors are not the same length!  Please enter two vectors of equal length.") 
}

total <- 0

for (i in seq_along(varx)){
  total <- total + varx[i]
}

mean_varx <- total/n1

total <- 0

for (i in seq_along(vary)){
  total <- total + vary[i]
}

mean_vary <- total/n2

a <-0
n <- n1
for (i in 1:n){
  a <- a + (varx[i] - mean_varx)*(vary[i] - mean_vary)
}

covariance <- a/(n-1)

print(covariance)

# Correlations

# user variables = (varx,vary)

varx <- na.omit(as.numeric(gsub(",", ".", dataset$Var1)))
vary <- na.omit(as.numeric(gsub(",", ".", dataset$Var2)))


total_values <- 0

for(i in seq_along(varx)) {
  
  total_values <- total_values + 1
}


n1<- total_values



total_values <- 0

for(i in seq_along(vary)) {
  
  total_values <- total_values + 1
}

n2<- total_values

if (n1 != n2) {
  stop("Vectors are not the same length! Please enter two vectors of equal length.")
}

total <- 0

for (i in seq_along(varx)){
  total <- total + varx[i]
}

mean_varx <- total/n1

total <- 0

for (i in seq_along(vary)){
  total <- total + vary[i]
}

mean_vary <- total/n2

a <-0

for (i in 1:n){
  a <- a + (varx[i] - mean_varx)*(vary[i] - mean_vary)
}


b <- 0

for (i in seq_along(varx)){
  b <- b + (varx[i] - mean_varx)**2
}

c <- 0

for (i in seq_along(vary)){
  c <- c + (vary[i] - mean_vary)**2
}

d <- b*c

m <- c(d)
tol <- 1e-8 
s <- m / 2  

while(abs(s^2 - m) > tol) {
  s <- (s + m/s) / 2
}

correlations <- a/s

print(correlations)
