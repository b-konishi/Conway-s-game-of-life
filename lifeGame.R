
SIZE = readline("Size(default: 10): ")
if (SIZE == "") {
  SIZE = 10 
} else {
  SIZE = as.numeric(SIZE)
}
BORN = 1
SURVIVES = 2

DEATH = BORN-1
ALIVE = SURVIVES-1

weightingAdder <- function(M) {
  TMP = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
  for (i in 1:SIZE) {
    for (j in 1:SIZE) {
      if (M[i,j] == DEATH)  next
      for (k in -1:1) {
        for (l in -1:1) {
          if (k == 0 && l == 0) next
          x = i + k
          y = j + l
          if (i+k == 0)  x = SIZE
          else if (i+k == SIZE+1) x = 1
          if (j+l == 0)  y = SIZE
          else if (j+l == SIZE+1) y = 1

          TMP[x,y] = TMP[x,y] + 1
        }
      }
    }
  }
  return(TMP)
}

cat('Type: {fill, empty, random}={f, e, r}\n')
type <- readline("Type: ")
if (type == "f") {
  A = matrix(1, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, col="green")
} else if (type == "e") {
  A = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, col="black")
} else {
  rand = as.integer(runif(SIZE^2, min=0, max=2))
  A = matrix(rand, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, col=c("black","green"))
}

count <- readline("Make Count: ")
if (count == "") {
  count = 0
} else {
  count <- as.numeric(count)
}
for (i in 1:count) {
  if (count == 0) break
  print(i, quote=FALSE)
  input <- as.numeric(locator(1))
  pos <- as.numeric(input) + 0.05
  len <- 1.1 / SIZE
  pos <- as.integer(pos/len) + 1
  if (A[pos[1], pos[2]] == 1)
    A[pos[1], pos[2]] = 0
  else
    A[pos[1], pos[2]] = 1

  if (length(A[A==1]) == length(A))
    image(A, col=c("green"))
  else
    image(A, col=c("black", "green"))
}


title = ""
cat("nomal: B3/S23\n")
cat("day and night: B3678/S34678\n")
cat("HighLife: B36/S23\n")
cat("Replicator: B1357/S1357\n")
cat("2x2: B36/S125\n")
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

counter = readline("generation num: ")

generation = 1
convergence = TRUE
B = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
repeat {
  if (counter == "")  readline()
  TMP = weightingAdder(A)
  for (x in 1:SIZE) {
    for (y in 1:SIZE) {
      if (A[x,y] == DEATH && length(rule[[BORN]][1]) != 0) {
        if (length(rule[[BORN]][rule[[BORN]] == TMP[x,y]]) == 1)  B[x,y] = 1
        else  B[x,y] = 0
      } else if (A[x,y] == ALIVE && length(rule[[SURVIVES]][1]) != 0) {
        if (length(rule[[SURVIVES]][rule[[SURVIVES]] == TMP[x,y]]) == 1)  B[x,y] = 1
        else  B[x,y] = 0
      }
    }
  }
  if (length((A==B)[(A==B)==TRUE]) == SIZE^2) break

  A = B
  image(A, main=paste(title, generation, sep=": "), col=c("black", "green"), axes = TRUE, pch=21)
  if (counter != "" && generation == as.numeric(counter))  break
  generation = generation + 1
}

