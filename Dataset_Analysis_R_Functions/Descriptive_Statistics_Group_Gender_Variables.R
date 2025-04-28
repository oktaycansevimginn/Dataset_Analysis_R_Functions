

############## OKTAY CAN SEVİMGİN / oktaycansevimgin2022@gmail.com ########

#(Number of observations, Minimum, Maximum, Range, Sum, Mean, Median, Sum of squares, Variance, Standard deviation)  

# "Calculations made in the first option of question 2 of the combination, considering only gender factors."

# user variable = var                                
# In these calculations, the variables we are working with are vectors called var. 
#Whenever the user creates a new var vector in each calculation step, the results printed will change based on the values in these newly created var vectors.                                  

# Number of observations
dataset = read.table(file = "C:/Users/Oktay Can/Desktop/Dataset/DatasetNA.txt", header = TRUE)  

var <- dataset$Var1[!is.na(dataset$Var1)]
var <- as.numeric(gsub(",", ".", var))

gender_values <- c()

for (i in 1:length(var)) {
  if (dataset$Gender[(!is.na(dataset$Var1))][i] == "Male") {
    gender_values <- c(gender_values, var[i])
  }
}

var <- gender_values 

# Hesaplama Kismi

total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

print(total_values)

# Minimum 

var_min <- var[1]
for (i in var) {
  if (i < var_min) {
    var_min <- i
  }
}
print(var_min)

# Maximum

var_max <- var[1]
for (i in var) {
  if (i > var_max) {
    var_max <- i
  }
}
print(var_max)

# Range

var_min <- var[1]
for (i in var) {
  if (i < var_min) {
    var_min <- i
  }
}

var_max <- var[1]
for (i in var) {
  if (i > var_max) {
    var_max <- i
  }
}

var_range <- c(var_min,var_max)
print(var_range)

# Sum

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}

print(total)


# Mean

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}

total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values

mean <- total/n
print(mean)

# Median

for (i in 1:length(var)) {
  for (j in i:length(var)) {
    if (var[j] < var[i]) {
      temp <- var[i]
      var[i] <- var[j]
      var[j] <- temp
    }
  }
}

n <- length(var)
if (n %% 2 == 0) {
  median <- mean(var[(n/2):(n/2+1)], na.rm = TRUE)
} else {
  median <- var[(n+1)/2]
}

print(median)

# Sum of squares

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}

total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values

mean_var <- total/n
mean_var            

Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}
print(Sumofsquares)


# Variance

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}

total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values

mean_var <- total/n

Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}

variance <-(Sumofsquares/(n-1))
print(variance)

# Standard deviation

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}

total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}


n<- total_values

mean_var <- total/n

Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}

variance <-(Sumofsquares/(n-1))

a <- c(variance)
tol <- 1e-8 
b <- a / 2  

while(abs(b^2 - a) > tol) {
  b <- (b + a/b) / 2
}

standard_deviation <- b

print(standard_deviation)

# SADECE GRUP FAKTORLERINE GORE


# Number of observations

var <- dataset$Var1[!is.na(dataset$Var1)]
var <- as.numeric(gsub(",", ".", var))


group_values <- c()


for (i in 1:length(var)) {
  if (dataset$Group[!is.na(dataset$Var1)][i] == "Group3") {
    group_values <- c(group_values, var[i])
  }
}


var <- group_values

# Hesaplama Kismi

total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

print(total_values)

# Minimum 

var_min <- var[1]
for (i in var) {
  if (i < var_min) {
    var_min <- i
  }
}
print(var_min)

# Maximum

var_max <- var[1]
for (i in var) {
  if (i > var_max) {
    var_max <- i
  }
}
print(var_max)

# Range

var_min <- var[1]
for (i in var) {
  if (i < var_min) {
    var_min <- i
  }
}

var_max <- var[1]
for (i in var) {
  if (i > var_max) {
    var_max <- i
  }
}

var_range <- c(var_min,var_max)
print(var_range)

# Sum

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}

print(total)

# Mean

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}

total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values


mean <- total/n
print(mean)

# Median

for (i in 1:length(var)) {
  for (j in i:length(var)) {
    if (var[j] < var[i]) {
      temp <- var[i]
      var[i] <- var[j]
      var[j] <- temp
    }
  }
}

n <- length(var)
if (n %% 2 == 0) {
  median <- mean(var[(n/2):(n/2+1)], na.rm = TRUE)
} else {
  median <- var[(n+1)/2]
}

print(median)

# Sum of squares

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}


total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values

mean_var <- total/n
mean_var              

Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}
print(Sumofsquares)


# Variance

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}

total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values

mean_var <- total/n

Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}

variance <-(Sumofsquares/(n-1))
print(variance)

# Standard deviation

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}


total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values

mean_var <- total/n

Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}

variance <-(Sumofsquares/(n-1))

a <- c(variance)
tol <- 1e-8 
b <- a / 2  

while(abs(b^2 - a) > tol) {
  b <- (b + a/b) / 2
}

standard_deviation <- b

print(standard_deviation)


# HEM CINSIYET HEM GRUP FAKTORLERINE GORE 

# Number of observations

var <- dataset$Var1[!is.na(dataset$Var1)]
var <- as.numeric(gsub(",", ".", var))

gender_group_values <- c()

for (i in 1:length(var)) {
  if (dataset$Gender[!is.na(dataset$Var1)][i] == "Male" && dataset$Group[!is.na(dataset$Var1)][i] == "Group2") {
    gender_group_values <- c(gender_group_values, var[i])
  }
}

var <- gender_group_values

# Hesaplama Kismi

total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

print(total_values)

# Minimum

var_min <- var[1]
for (i in var) {
  if (i < var_min) {
    var_min <- i
  }
}
print(var_min)

# Maximum

var_max <- var[1]
for (i in var) {
  if (i > var_max) {
    var_max <- i
  }
}
print(var_max)

# Range

var_min <- var[1]
for (i in var) {
  if (i < var_min) {
    var_min <- i
  }
}

var_max <- var[1]
for (i in var) {
  if (i > var_max) {
    var_max <- i
  }
}

var_range <- c(var_min,var_max)
print(var_range)

# Sum

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}
print(total)

# Mean

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}
total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

print(total_values)
n<- total_values
mean <- total/n
print(mean)

# Median

for (i in 1:length(var)) {
  for (j in i:length(var)) {
    if (var[j] < var[i]) {
      temp <- var[i]
      var[i] <- var[j]
      var[j] <- temp
    }
  }
}

n <- length(var)
if (n %% 2 == 0) {
  median <- mean(var[(n/2):(n/2+1)], na.rm = TRUE)
} else {
  median <- var[(n+1)/2]
}

print(median)


# Sum of squares

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}
total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values


mean_var <- total/n
mean_var              
Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}
print(Sumofsquares)

# Variance

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}
total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values


mean_var <- total/n


Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}
variance <-(Sumofsquares/(n-1))
print(variance)
# Standard deviation

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}
total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values
mean_var <- total/n


Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}

variance <-(Sumofsquares/(n-1))
a <- c(variance)
tol <- 1e-8 
b <- a / 2  

while(abs(b^2 - a) > tol) {
  b <- (b + a/b) / 2
}

standard_deviation <- b

print(standard_deviation)
