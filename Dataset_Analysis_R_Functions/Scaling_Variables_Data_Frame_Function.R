

############## OKTAY CAN SEV??MG??N / oktaycansevimgin2022@gmail.com ########

# ******************Scale******************
# user variables = (varX, varY, VarZ)

dataset <- read.table(file = "C:/Users/Oktay Can/Desktop/Dataset/DatasetNA.txt", header = TRUE)

remove_na <- function(variable) {

  cleaned_variable <- variable[!is.na(variable)]
  return(cleaned_variable)
}

replace_comma_with_dot <- function(variable) {

  for (i in 1:length(variable)) {
    if (grepl(",", variable[i])) {
      variable[i] <- gsub(",", ".", variable[i])
    }
  }
  return(variable)
}

manual_min <- function(variable) {
  min_value <- variable[1]
  for (i in 2:length(variable)) {
    if (variable[i] < min_value) {
      min_value <- variable[i]
    }
  }
  return(min_value)
}

manual_max <- function(variable) {
  max_value <- variable[1]
  for (i in 2:length(variable)) {
    if (variable[i] > max_value) {
      max_value <- variable[i]
    }
  }
  return(max_value)
}

varX <- remove_na(dataset$Var1)
varX <- replace_comma_with_dot(varX)

varY <- remove_na(dataset$Var2)
varY <- replace_comma_with_dot(varY)

varZ <- remove_na(dataset$Var4)
varZ <- replace_comma_with_dot(varZ)

df <- data.frame(varX, varY, varZ)

scale_variable <- function(variable, scale_factor = 1, offset = 0) {
  min_value <- manual_min(variable)
  max_value <- manual_max(variable)

  scaled_variable <- (variable - min_value) / (max_value - min_value)
  scaled_variable <- scaled_variable * scale_factor + offset
  return(scaled_variable)
}

varX_scaled <- scale_variable(as.numeric(varX), scale_factor = 1/4)
varY_scaled <- scale_variable(as.numeric(varY), scale_factor = 10)
varZ_scaled <- scale_variable(as.numeric(varZ), scale_factor = 2, offset = -1)

df$varX_scaled <- varX_scaled
df$varY_scaled <- varY_scaled
df$varZ_scaled <- varZ_scaled

print(df)
