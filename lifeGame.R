
SIZE = readline("Size(default: 10): ")
if (SIZE == "") {
  SIZE = 10 
} else {
  SIZE = as.numeric(SIZE)
}
BIRTH = 1
STABLE = 2

deadOrAlive <- function(M, x, y, rule) {
  child = 0
  for (i in -1:1) {
    for (j in -1:1) {
      if (i == 0 && j == 0) next
      tmp_x = x + i
      tmp_y = y + j
      if (x+i == 0)  tmp_x = SIZE
      else if (x+i == SIZE+1) tmp_x = 1
      if (y+j == 0)  tmp_y = SIZE
      else if (y+j == SIZE+1) tmp_y = 1

      if (M[tmp_x, tmp_y] == 1) child = child + 1
      if (child > max(rule[[1]], rule[[2]]))  break
    }
    if (child > max(rule[[1]], rule[[2]]))  break
  }
  if (M[x,y] == BIRTH-1 && length(rule[[BIRTH]][1]) != 0) {
    for (i in 1:length(rule[[BIRTH]])) {
      if (child == rule[[BIRTH]][i]) return(1)
    }
  } else if (M[x,y] == STABLE-1 && length(rule[[STABLE]][1]) != 0) {
    for (i in 1:length(rule[[STABLE]])) {
      if (child == rule[[STABLE]][i]) return(1)
    }
  }
  return(0)
}


print("Type: {fill, empty, random}={f, e, r}")
type <- readline("Type?: ")
if (type == "f") {
  A = matrix(1, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, col="green")
} else if (type == "e") {
  A = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, col="white")
} else {
  rand = as.integer(runif(SIZE^2, min=0, max=2))
  A = matrix(rand, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, col=c("white","green"))
}

count <- readline("Make Count: ")
if (count == "") {
  count = 0
} else {
  count <- as.numeric(count)
}
for (i in 1:count) {
  if (count == 0) break
  print(i)
  input <- as.numeric(locator(1))
  pos <- as.numeric(input) + 0.05
  len <- 1.1 / SIZE
  pos <- as.integer(pos/len) + 1
  if (A[pos[1], pos[2]] == 1) A[pos[1], pos[2]] = 0
  else  A[pos[1], pos[2]] = 1

  flag = FALSE
  for (j in 1:SIZE) {
    for (k in 1:SIZE) {
      if (A[j,k] == 0) flag = TRUE
    }
  }
  if (flag) image(A, col=c("white","green"))
  else  image(A, col=c("green"))
}


title = ""
print("nomal: B3/S23")
print("day and night: B3678/S34678")
print("HighLife: B36/S23")
print("Replicator: B1357/S1357")
print("2x2: B36/S125")
rule <- readline("Input Pattern: ")
if (rule == "nomal" || rule == "") {
  title = "nomal"
  rule_born <- c(3)
  rule_survives <- c(2,3)
} else if (rule == "day and night") {
  title = "day and night"
  rule_born <- c(3,6,7,8)
  rule_survives <- c(3,4,6,7,8)
} else if (rule == "HighLife") {
  title = "HighLife"
  rule_born <- c(3,6)
  rule_survives <- c(2,3)
} else if (rule == "Replicator") {
  title = "Replicator"
  rule_born <- c(1,3,5,7)
  rule_survives <- c(1,3,5,7)
} else if (rule == "2x2") {
  title = "2x2"
  rule_born <- c(3,6)
  rule_survives <- c(1,2,5)
} else {
  rule <- unlist(strsplit(rule, " "))
  rule_born <- as.numeric(unlist(strsplit(rule[1], ",")))
  rule_survives <- as.numeric(unlist(strsplit(rule[2], ",")))
}
if (rule_born[[1]] == 0) {
  rule <- list(c(), rule_survives)
} else if (rule_survives[[1]] == 0) {
  rule <- list(rule_born, c())
} else {
  rule <- list(rule_born, rule_survives)
}

convergence_flag = FALSE
if (readline("Convergence-Mode: ") == "y")  convergence_flag = TRUE

generation = 1
convergence = TRUE
B = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
repeat {
  readline()
  for (i in 1:SIZE) {
    for (j in 1:SIZE) {
      B[i,j] = deadOrAlive(A, i, j, rule)
    }
  }
  if (convergence_flag) {
    for (i in 1:SIZE^2) {
      if (as.vector(A)[i] != as.vector(B)[i]) {
        convergence = FALSE
      }
    }
    if (convergence)  break
    else  convergence = TRUE
  }
  A = B
  image(A, main=paste(title, generation, sep=": "), col=c("white", "green"), axes = TRUE, pch=21)
  generation = generation + 1
}

