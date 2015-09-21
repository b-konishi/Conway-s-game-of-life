
SIZE = readline("Size(default: 30): ")
SIZE = if(SIZE == "") 30 else as.numeric(SIZE)
BG_COLOR = "black"
FW_COLOR = "green"

BORN = 1
SURVIVES = 2

DEATH = BORN-1
ALIVE = SURVIVES-1

weightingAdder <- function(M) {
  TMP = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
  for (i in 1:SIZE) {
    for (j in 1:SIZE) {
      if (M[i,j] == 0)  next
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

# Deprecated function
opt <- function(M) {
  return(t(apply(M,2,rev)))
}

title = "Conway's game of life"
cat('Type: {fill, empty, random}={f, e, r}\n')
type <- readline("Type: ")
if (type == "f") {
  A = matrix(1, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, main=title, col=FW_COLOR, axes=FALSE)
} else if (type == "e") {
  A = matrix(0, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, main=title, col=BG_COLOR, axes=FALSE)
} else {
  rand = as.integer(runif(SIZE^2, min=0, max=2))
  A = matrix(rand, nrow=SIZE, ncol=SIZE, byrow=T)
  image(A, main=title, col=c(BG_COLOR,FW_COLOR), axes=FALSE)
}


makePattern <- function(M) {
  count <- readline("Make Count: ")
  count = if (count == "") 0 else as.numeric(count)

  for (i in 1:count) {
    if (count == 0) break
    print(i, quote=FALSE)
    input <- as.numeric(locator(1))
    pos <- as.numeric(input) + 0.05
    if (SIZE <= 10) 
      len <- 1.1/SIZE
    else if (SIZE > 30)
      len <- 1.1/SIZE

    pos <- as.integer(pos/len) + 1
    
    M[pos[1], pos[2]] = if(M[pos[1],pos[2]]==1) 0 else 1

    image(M, col = if(length(M[M==1])==length(M)) FW_COLOR else c(BG_COLOR,FW_COLOR), axes=FALSE)
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

printPattern <- function() {
  cat('\n')
  cat("normal: B3/S23\n")
  cat("day and night: B3678/S34678\n")
  cat("HighLife: B36/S23\n")
  cat("Replicator: B1357/S1357\n")
  cat("2x2: B36/S125\n")
  cat('\nPattern: [B3/S34]\n')
}
inputPattern <- function(rule) {
  rule <- readline("Input Pattern: ")
  if (rule == "normal" || rule=="n" || rule == "") {
    title = "normal"
    rule = "B3/S23"
  } else if (rule == "day and night" || rule=="d") {
    title = "day and night"
    rule = "B3678/S34678"
  } else if (rule == "HighLife" || rule=="h") {
    title = "HighLife"
    rule = "B36/S23"
  } else if (rule == "Replicator" || rule=="r") {
    title = "Replicator"
    rule = "B1357/S1357"
  } else if (rule == "2x2" || rule=="2") {
    title = "2x2"
    rule = "B36/S125"
  } else {
    title = rule
  }
  
  return(paste(title, rule, sep=","))
}

title = ""
printPattern()
rule <- inputPattern(rule)
title <- strsplit(rule, ",")[[1]][1]
rule <- strsplit(rule, ",")[[1]][2]
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
  } else if (request == "fwcolor") {
    cat('Color: {white, red, green, blue, lightblue, purple, yellow, gray, black}\n')
    FW_COLOR = readline("Color: ")
  } else if (request == "bgcolor") {
    cat('Color: {white, red, green, blue, lightblue, purple, yellow, gray, black}\n')
    BG_COLOR = readline("Color: ")
  } else if (request == "rule") {
    printPattern()
    rule <- inputPattern(rule)
    title <- strsplit(rule, ",")[[1]][1]
    rule <- strsplit(rule, ",")[[1]][2]
    rule <- patternCompiler(rule)
  } else if (request == "quit" || request == "q") {
    if (readline("Are you sure you want to quit? [y or n]: ") == "y") break
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
        if (is.null(rule[[SURVIVES]]) || length(rule[[SURVIVES]][rule[[SURVIVES]]==TMP[x,y]])==0)
          B[x,y] = 0
        else
          B[x,y] = 1
        next
      } 
    }
  }
  if (length((A==B)[(A==B)==TRUE]) == SIZE^2) break

  A = B
  image(A, main=paste(title,generation,sep=": "), col=if(length(A[A==1])==length(A)) FW_COLOR else c(BG_COLOR,FW_COLOR), axes=FALSE)
  if (counter != "" && generation == as.numeric(counter))  counter = ""
  generation = generation + 1
}

