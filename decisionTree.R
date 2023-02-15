data = read.table("data.txt", col.names = c('x1', 'x2', 'y'))
D = data # use all data for training

MakeSubtree = function(D) {
  C = DetermineCandidateSplits(D)
  N = list(
    classLabel = numeric(),
    split = list(),
    children = list()
  )
  if (stopTree(D, C)) {
    N$classLabel = makeClassLabel(D) # leaf node
  } else {
  # internal node
    S_IG_e = FindBestSplit(D, C)
    S = S_IG_e[[1]]
    N$split = S
    D1 = D[D[[ S[[ 1 ]] ]] - S[[ 2 ]] >= 0, ]
    D2 = D[D[[ S[[ 1 ]] ]] - S[[ 2 ]] <  0, ]
    N$children[[1]] = MakeSubtree(D1)
    N$children[[2]] = MakeSubtree(D2)
  }
  return(N) # return sub-tree rooted at N
}

DetermineCandidateSplits = function(D) {
  C = list()
  K1 = ncol(D)
  K = K1 - 1 # class labels assumed to be in last column, others are features.
  for (i in 1:K) {
    C = c(C, DetermineCandidateNumericSplits(D,i))
  }
  return(C)
}

DetermineCandidateNumericSplits = function(D, i) {
  K1 = ncol(D)
  K = K1 - 1 # class labels assumed to be in the last column, others are features.
  C = list()
  v = D[, i]
  D = D[order(v), ]
  M = nrow(D) - 1
  for (j in 1:M) {
    # if ( D[j, K1] != D[j + 1, K1] ) { # implement according to lecture slides
    if ( TRUE ) { # implement according to homework 2 pdf: use all splits
      # C = c(C, list(list(i, (D[j, i] + D[j + 1, i]) / 2))) # implement according to lecture slides
      C = c(C, list(list(i, D[j, i]))) # implement according to homework 2 pdf
    }
  }
  return(C)
}

FindBestSplit = function(D,C) {
  K1 = ncol(D) # last col assumed to be class labels
  Y = D[, K1]
  Py = table(Y) / length(Y)
  HY = -1 * sum(Py * log(Py, base=2))
  HYC = as.numeric()
  for (s in C) {
    i = s[[1]] # feature of the split
    thresh = s[[2]] # threshold of the split
    X = D[, i]
    Z = X >= thresh
    Py0 = table(Y[Z == 0]) / sum(Z == 0)
    Py1 = table(Y[Z == 1]) / sum(Z == 1)
    HYX0 = -1 * sum(Py0 * log(Py0, base=2))
    HYX1 = -1 * sum(Py1 * log(Py1, base=2))
    HYX = (HYX0 * sum(Z == 0) + HYX1 * sum(Z == 1)) / (length(Z)) # * (-1) # i don't think the -1 is supposed to be here.
    HYC = c(HYC, HYX)
  }
  InfoGain = HY - HYC
  whichMax = which(InfoGain == max(InfoGain))
  whichMax = whichMax[1] # in case of a tie, just take the first one
  BestSplit = C[[whichMax]]
  return(list(BestSplit, InfoGain, HYC))
}

stopTree = function(D, C) {
  toler = 10e-4 # close to zero
  trueFalse = FALSE
  trueFalse = trueFalse | nrow(D) == 0
  SIG = FindBestSplit(D, C)
  IG = SIG[[2]]
  entropy = SIG[[3]]
  trueFalse = trueFalse | all(IG < toler)
  trueFalse = trueFalse | any(entropy < toler)
  return(trueFalse)
}

makeClassLabel = function(D) {
  K1 = ncol(D) # last column assumed to be class labels
  Y = D[, K1]
  s1 = sum(Y == 1)
  s0 = sum(Y == 0)
  out = as.numeric(s1 >= s0)
  return(out)
}
  