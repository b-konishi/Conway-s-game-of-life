
SIZE = readline("Size(default: 10): ")
SIZE = if(SIZE == "") 10 else as.numeric(SIZE)

BORN = 1
SURVIVES = 2

DEATH = BORN-1
ALIVE = SURVIVES-1

weighter <- function(x, M) {
  TMP = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
  y <- x[1]
  x <- x[2]
  if (M[x,y] == 1) {
    m <- -1:1
    n <- -1:1
    if (x == 1) {
      m <- c(0, 1, SIZE-1)
    } else if (x == SIZE) {
      m <- c(-1, 0, -(SIZE-1))
    }
    if (y == 1) {
      n <- c(0, 1, SIZE-1)
    } else if (y == SIZE) {
      n <- c(-1, 0, -(SIZE-1))
    }
    o <- expand.grid(m,n)

    TMP[x+o[[1]], y+o[[2]]] = TMP[x+o[[1]], y+o[[2]]] + 1
  }
  return(TMP)
}

weightingAdder <- function(M) {
  TMP = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
  cell = expand.grid(1:SIZE, 1:SIZE)
  cell = c(cell[[1]], cell[[2]])
  cell <- matrix(cell, nrow=2, ncol=SIZE^2, byrow=T)
  total <- (apply(cell, 2, weighter, M))
  TMP = apply(total, 1, sum)
  TMP <- t(matrix(TMP, nrow=SIZE, ncol=SIZE, byrow=T))

  return(TMP)
}

# Deprecated function
opt <- function(M) {
  return(t(apply(M,2,rev)))
}

title = "Conway's game of life"
cat('Type: {fill, empty, random}={f, e, r}\n')
type <- readline("Type: ")
if (type == "f") {
  A = matrix(1, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, main=title, col="green", axes=FALSE)
} else if (type == "e") {
  A = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, main=title, col="black", axes=FALSE)
} else {
  rand = as.integer(runif(SIZE^2, min=0, max=2))
  A = matrix(rand, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, main=title, col=c("black","green"), axes=FALSE)
}


makePattern <- function(M) {
  count <- readline("Make Count: ")
  count = if (count == "") 0 else as.numeric(count)

  for (i in 1:count) {
    if (count == 0) break
    print(i, quote=FALSE)
    input <- as.numeric(locator(1))
    pos <- as.numeric(input) + 0.05
    len = if(SIZE<=10) 1.1/SIZE else 1.05/SIZE
    pos <- as.integer(pos/len) + 1
    
    M[pos[1], pos[2]] = if(M[pos[1],pos[2]]==1) 0 else 1

    image(M, col = if(length(M[M==1])==length(M)) "green" else c("black","green"), axes=FALSE)
  }
  return(M)
}

A = makePattern(A)

library('stringr')
patternCompiler <- function(pattern) {
  rule_born = c()
  rule_survives = c()
  pattern = str_split(pattern, "")[[1]]
  for (i in 1:length(pattern)) {
    if (pattern[i] == "B") FLAG = FALSE
    if (pattern[i] == "S") FLAG = TRUE

    if (str_detect(pattern[i], "[0-9]")) {
      if (!FLAG) {
        rule_born <- c(rule_born, as.numeric(pattern[i]))
      } else {
        rule_survives <- c(rule_survives, as.numeric(pattern[i]))
      }
    }
  }
  return (list(rule_born, rule_survives))
}

title = ""
cat('\n')
cat("nomal: B3/S23\n")
cat("day and night: B3678/S34678\n")
cat("HighLife: B36/S23\n")
cat("Replicator: B1357/S1357\n")
cat("2x2: B36/S125\n")
cat('\nPattern: [B3/S34]\n')
rule <- readline("Input Pattern: ")
if (rule == "nomal" || rule == "") {
  title = "nomal"
  rule = "B3/S23"
} else if (rule == "day and night") {
  title = "day and night"
  rule = "B3678/S34678"
} else if (rule == "HighLife") {
  title = "HighLife"
  rule = "B36/S23"
} else if (rule == "Replicator") {
  title = "Replicator"
  rule = "B1357/S1357"
} else if (rule == "2x2") {
  title = "2x2"
  rule = "B36/S125"
} else {
  title = rule
}
rule <- patternCompiler(rule)

counter = readline("generation num: ")

generation = 1
convergence = TRUE
B = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
repeat {
  if (counter == "")  request = readline()
  if (request == "add") {
    A = makePattern(A)
    readline()
  }
  TMP = weightingAdder(A)
  for (x in 1:SIZE) {
    for (y in 1:SIZE) {
      if (A[x,y] == DEATH) {
        if (is.null(rule[[BORN]]) || length(rule[[BORN]][rule[[BORN]] == TMP[x,y]]) == 0)
          B[x,y] = 0
        else
          B[x,y] = 1
        next
      } 
      if (A[x,y] == ALIVE) {
        if (is.null(rule[[SURVIVES]])||length(rule[[SURVIVES]][(rule[[SURVIVES]])==TMP[x,y]-1])==0)
          B[x,y] = 0
        else
          B[x,y] = 1
        next
      } 
    }
  }
  if (length((A==B)[(A==B)==TRUE]) == SIZE^2) break

  A = B
  image(A, main=paste(title,generation,sep=": "), col=if(length(A[A==1])==length(A)) "green" else c("black","green"), axes=FALSE)
  if (counter != "" && generation == as.numeric(counter))  counter = ""
  generation = generation + 1
}
