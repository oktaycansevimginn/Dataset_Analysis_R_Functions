

############## OKTAY CAN SEVİMGİN / oktaycansevimgin2022@gmail.com ########

#(Cross-products, Covariance, Correlations) 

# SADECE CINSIYET FAKTORLERINE GORE

# Cross Products***************************

# user variables = (x,y)
dataset = read.table(file = "C:/Users/Oktay Can/Desktop/Dataset/DatasetNA.txt", header = TRUE) 

var <- dataset$Var8[!is.na(dataset$Var8)]
var <- as.numeric(gsub(",", ".", var))


gender_values <- c()


for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var8)][i] == "Male") {
    gender_values <- c(gender_values, var[i])
  }
}

varA <- gender_values

var <- dataset$Var6[!is.na(dataset$Var6)]
var <- as.numeric(gsub(",", ".", var))


gender_values <- c()


for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var6)][i] == "Male") {
    gender_values <- c(gender_values, var[i])
  }
}

varB <- gender_values

x <- varA
y <- varB

x <- x[1:6]
y <- y[1:6]

x_matris <- matrix(x, nrow = 6, ncol = 1)
y_matris <- matrix(y, nrow = 1, ncol = 6)

cross_products <- x_matris %*% y_matris

print(cross_products)

# Covariance ****************************************

# user variables = (varx,vary)
var <- dataset$Var8[!is.na(dataset$Var8)]
var <- as.numeric(gsub(",", ".", var))


gender_values <- c()


for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var8)][i] == "Male") {
    gender_values <- c(gender_values, var[i])
  }
}

varA <- gender_values

var <- dataset$Var6[!is.na(dataset$Var6)]
var <- as.numeric(gsub(",", ".", var))


gender_values <- c()


for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var6)][i] == "Male") {
    gender_values <- c(gender_values, var[i])
  }
}

varB <- gender_values

varx <- varA
vary <- varB

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
  stop("Please enter two vectors of equal length.") 
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


# Correlations*******************************************

# user variables = (varx,vary)

var <- dataset$Var7[!is.na(dataset$Var7)]
var <- as.numeric(gsub(",", ".", var))
gender_values <- c()


for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var7)][i] == "Male") {
    gender_values <- c(gender_values, var[i])
  }
}

varA <- gender_values

var <- dataset$Var5[!is.na(dataset$Var5)]
var <- as.numeric(gsub(",", ".", var))


gender_values <- c()


for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var5)][i] == "Male") {
    gender_values <- c(gender_values, var[i])
  }
}

varB <- gender_values
varx <- varA
vary <- varB

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
  stop("Please enter two vectors of equal length.") 
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

# SADECE GRUP FAKTORLERINE GORE

# Crossproducts*************************************

# user variables = (x,y)
var <- dataset$Var3[!is.na(dataset$Var3)]
var <- as.numeric(gsub(",", ".", var))

group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Group[!is.na(dataset$Var3)][i] == "Group3") {
    group_values <- c(group_values, var[i])
  }
}
varC <- group_values

var <- dataset$Var2[!is.na(dataset$Var2)]
var <- as.numeric(gsub(",", ".", var))

group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Group[!is.na(dataset$Var2)][i] == "Group3") {
    group_values <- c(group_values, var[i])
  }
}

varD <- group_values

x <- varC
y <- varD

x <- x[1:6]
y <- y[1:6]

x_matris <- matrix(x, nrow = 6, ncol = 1)
y_matris <- matrix(y, nrow = 1, ncol = 6)
cross_products <- x_matris %*% y_matris

print(cross_products)

# Covariance ***************************************************

# user variables = (varx,vary)
var <- dataset$Var5[!is.na(dataset$Var5)]
var <- as.numeric(gsub(",", ".", var))

group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Group[!is.na(dataset$Var5)][i] == "Group1") {
    group_values <- c(group_values, var[i])
  }
}

varC <- group_values

var <- dataset$Var7[!is.na(dataset$Var7)]
var <- as.numeric(gsub(",", ".", var))
group_values <- c()
for (i in 1:length(var)) {
  if (dataset$Group[!is.na(dataset$Var7)][i] == "Group1") {
    group_values <- c(group_values, var[i])
  }
}


varD <- group_values

varx <- varC
vary <- varD

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
  stop("Please enter two vectors of equal length.") 
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

# Correlations**********************************************

# user variables = (varx,vary)
var <- dataset$Var5[!is.na(dataset$Var5)]
var <- as.numeric(gsub(",", ".", var))

group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Group[!is.na(dataset$Var5)][i] == "Group1") {
    group_values <- c(group_values, var[i])
  }
}

varC <- group_values

var <- dataset$Var7[!is.na(dataset$Var7)]
var <- as.numeric(gsub(",", ".", var))


group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Group[!is.na(dataset$Var7)][i] == "Group1") {
    group_values <- c(group_values, var[i])
  }
}

varD <- group_values

varx <- varC
vary <- varD

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
  stop("Please enter two vectors of equal length.")
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

# HEM CINSIYET HEM GRUP FAKTORLERINE GORE 

# Crossproducts*******************************************

# user variables = (x,y)
var <- dataset$Var1[!is.na(dataset$Var1)]
var <- as.numeric(gsub(",", ".", var))

gender_group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var1)][i] == "Male" && dataset$Group[!is.na(dataset$Var1)][i] == "Group2") {
    gender_group_values <- c(gender_group_values, var[i])
  }
}

varA <- gender_group_values

var <- dataset$Var1[!is.na(dataset$Var1)]
var <- as.numeric(gsub(",", ".", var))

gender_group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var1)][i] == "Male" && dataset$Group[!is.na(dataset$Var1)][i] == "Group2") {
    gender_group_values <- c(gender_group_values, var[i])
  }
}

varB <- gender_group_values

x <- varA
y <- varB

x <- x[1:6]
y <- y[1:6]

x_matris <- matrix(x, nrow = 6, ncol = 1)
y_matris <- matrix(y, nrow = 1, ncol = 6)

cross_products <- x_matris %*% y_matris

print(cross_products)

# Covariance ******************************************

# user variables = (varx,vary)
var <- dataset$Var2[!is.na(dataset$Var2)]
var <- as.numeric(gsub(",", ".", var))

gender_group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var2)][i] == "Male" && dataset$Group[!is.na(dataset$Var2)][i] == "Group2") {
    gender_group_values <- c(gender_group_values, var[i])
  }
}

varA <- gender_group_values

var <- dataset$Var3[!is.na(dataset$Var3)]
var <- as.numeric(gsub(",", ".", var))

gender_group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var3)][i] == "Female" && dataset$Group[!is.na(dataset$Var3)][i] == "Group3") {
    gender_group_values <- c(gender_group_values, var[i])
  }
}

varB <- gender_group_values

varx <- varA
vary <- varB

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
  stop("Please enter two vectors of equal length.")
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

# Correlations*************************************************

# user variables = (varx,vary)

var <- dataset$Var2[!is.na(dataset$Var2)]
var <- as.numeric(gsub(",", ".", var))

gender_group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var2)][i] == "Male" && dataset$Group[!is.na(dataset$Var2)][i] == "Group2") {
    gender_group_values <- c(gender_group_values, var[i])
  }
}

varA <- gender_group_values

var <- dataset$Var3[!is.na(dataset$Var3)]
var <- as.numeric(gsub(",", ".", var))

gender_group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var3)][i] == "Female" && dataset$Group[!is.na(dataset$Var3)][i] == "Group3") {
    gender_group_values <- c(gender_group_values, var[i])
  }
}
varB <- gender_group_values

varx <- varA
vary <- varB

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
  stop("Please enter two vectors of equal length.") 
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
