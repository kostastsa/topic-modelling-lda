library("Matrix")

# matrix of Word-Document counts. Entry (w,d) is the number of times word w in vocabulary appears in document d
c.wd = readMM("/Users/kostastsampourakis/Desktop/work/study/wsu/Stat 536/Project/bbc/bbc.mtx")
D = 2000 # Total number of training documents
W = dim(c.wd)[1]  # Number of unique words in the corpus (vocabulary)
train.docs = sort(sample(1:dim(c.wd)[2], D)) 
c.wd.train = c.wd[,train.docs] # remove documents at random for training
c.wd.test = c.wd[,-train.docs] # test docs 
M = sum(c.wd.train)       # Total number of words in all documents (with repeated words)
N_d = colSums(c.wd.train) # vector of document lengths. entry i is the length of document i 
K = 5


#initialize word id list
doc.word.ta = list()
doc.word.id = list()
for (d in 1:D) {
  doc.word.ta[[d]] = rep(0,N_d[d])
  doc.word.id[[d]] = rep(0,N_d[d])
}


for (d in 1:D) {
  doc = c.wd.train[,d]
  max.count = max(doc)
  w.doc = c()
  for (count in max.count:1) {
    words.at.count = which(doc %in% count)
    for (word in words.at.count) {
      w.doc = c(w.doc, rep(word, count))
    }
  }
  doc.word.id[[d]] = w.doc
}




c.wt = matrix(0, nrow = W, ncol = K)
c.dt = matrix(0, nrow = D, ncol = K)

# initialize topic assignments
for (d in 1:D) {
  for (w in 1:N_d[d]) {
    # assign topic to word w of document d
    doc.word.ta[[d]][w] = sample(1:5,1) 
    
    k = doc.word.ta[[d]][w] # assigned topic
    w.id = doc.word.id[[d]][w]  # index of w^th  word of doc d
    c.wt[w.id, k] = c.wt[w.id, k] + 1
  }
  for (j in 1:K) {
    c.dt[d,j] = sum(doc.word.ta[[d]] == j)
  }
}


## Gibbs sampling procedure
nsim = 10
alpha = 10
eta = 0.1


for (n in 1:nsim) {
  print(n)
  for (d in 1:D) {
    for (w in 1:N_d[d]) {
      zi.old = doc.word.ta[[d]][w]
      wi = doc.word.id[[d]][w]
      
      c.dt[d, zi.old] = c.dt[d, zi.old] - 1
      c.wt[wi ,zi.old] = c.wt[wi, zi.old] - 1
      
      p = c()
      for (j in 1:K) {
        # subtract 1 here
        t1 = ( c.wt[wi,j] + eta ) / ( sum(c.wt[,j]) + W * eta )
        t2 = ( c.dt[d,j] + alpha ) / ( sum(c.dt[d,]) + K * alpha )
        p[j] = t1 * t2
      }
      p = p / sum(p)
      
      zi = sample(1:K, 1, prob = p )                  
      doc.word.ta[[d]][w] = zi
      
      c.dt[d, zi] = c.dt[d, zi] + 1
      c.wt[wi ,zi] = c.wt[wi, zi] + 1
    }
  }
}

s = colSums(c.wt)

f = ( c.wt + eta ) / ( s + W*eta )
