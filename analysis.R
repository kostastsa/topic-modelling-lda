c.wd = readMM("/Users/kostastsampourakis/Desktop/work/study/wsu/Stat 536/Project/bbc/bbc.mtx")
c.wt = read.csv("/Users/kostastsampourakis/Desktop/work/study/wsu/Stat 536/Project/wt.csv")[,-1]
c.dt = read.csv("/Users/kostastsampourakis/Desktop/work/study/wsu/Stat 536/Project/dt.csv")[,-1]
traindocs = read.csv("/Users/kostastsampourakis/Desktop/work/study/wsu/Stat 536/Project/traindocs.csv")$x
bbc = read.csv("/Users/kostastsampourakis/Desktop/work/study/wsu/Stat 536/Project/bbc.csv")
true.topics = classes$V2

K = 5
W = dim(c.wd)[1] 
D = length(traindocs)
N_d = colSums(c.wd)
N_d.t = N_d[traindocs]
alpha = 10
eta = 0.1
c.wd.test = c.wd[,-traindocs] 
true.top.test = true.topics[-traindocs]


n.dot.top = c() # number of assigments per topic
for (j in 1:K){
  n.dot.top[j] = sum(c.wt[,j])
}


# column k of matrix "topics" is the k^th topic (distribution over words)
topics = matrix(nrow = W, ncol = K)
for (j in 1:K) {
  topics[,j] = ( c.wt[,j] + eta ) / ( n.dot.top[j] +W*eta )
}

# element d,k of matrix "props" is the proportion of topic k in doc d
props = matrix(nrow = D, ncol = K)
for (d in 1:D) {
  for (j in 1:K){
  props[d,j] = ( c.dt[d,j] + alpha ) / ( N_d.t[d] + K * alpha )
  }
}


# this is a list of the dominant topic in doc d (classification)
doc.topics = c()
for (d in 1:D) {
  doc.topics[d] = which.max(props[d,])
}



top = list()
for (k in 1:K) {
  v <- topics[,k]
  vec <- v
  N <- 10
  sorted = sort(v, decreasing = T)
  sorted = sorted[1:N]
  indices = sort(which(vec >= sorted[N]), decreasing = T)[1:200]
  vec[indices]
  top[[k]] = bbc[indices,1]
}


# column k of top.200 is a list of the 200 most common words in topic k
top.10 = data.frame("1"=top[[1]],"2"=top[[2]],"3"=top[[3]],"4"=top[[4]],"5"=top[[5]])

################################### Test doc classification

## Given document d we want to recover its topic proportions, where the topics are the ones we found during training. 


#initialize word id list
doc.word.ta = list()
doc.word.id = list()
Dt = 225
N_d.test = N_d[-traindocs]
nsim = 10


for (d in 1:Dt) {
  doc.word.ta[[d]] = rep(0,N_d.test[d])
  doc.word.id[[d]] = rep(0,N_d.test[d])
}


for (d in 1:Dt) {
  doc = c.wd.test[,d]
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


c.dt.test = matrix(0, nrow = Dt, ncol = K)

# initialize topic assignments
for (d in 1:Dt) {
  for (w in 1:N_d.test[d]) {
    # assign topic to word w of document d
    doc.word.ta[[d]][w] = sample(1:5,1) 
    
    k = doc.word.ta[[d]][w] # assigned topic
    w.id = doc.word.id[[d]][w]  # index of w^th  word of doc d
  }
  for (j in 1:K) {
    c.dt.test[d,j] = sum(doc.word.ta[[d]] == j)
  }
}


### Gibbs sampling procedure 

nsim = 500
alpha = 10
eta = 0.1


for (n in 1:nsim) {
  print(n)
  for (d in 1:Dt) {
    for (w in 1:N_d.test[d]) {
      zi.old = doc.word.ta[[d]][w]
      wi = doc.word.id[[d]][w]
      
      c.dt.test[d, zi.old] = c.dt[d, zi.old] - 1
      p = topics[wi,]
      p = p
      zi = sample(1:K, 1, prob = p )                  
      doc.word.ta[[d]][w] = zi
      
      c.dt.test[d, zi] = c.dt[d, zi] + 1
    }
  }
}


props.test = matrix(nrow = Dt, ncol = K)
for (d in 1:Dt) {
  for (j in 1:K){
    props.test[d,j] =  c.dt.test[d,j] + alpha 
  }
  props.test[d,] = props.test[d,] / sum(props.test[d,])
}





true.topics.train = true.topics[traindocs]
true.topics.test = true.topics[-traindocs]
counts = table(true.topics.train)
renamed = c(rep(1,as.vector(counts["0"])),rep(3,as.vector(counts["1"])),rep(5,as.vector(counts["2"])),rep(4,as.vector(counts["3"])),rep(2,as.vector(counts["4"])))

tab = table(renamed, doc.topics)
tab
1-(sum(diag(tab))/sum(tab))


doc.cat.test = read.csv("/Users/kostastsampourakis/Desktop/work/study/wsu/Stat 536/Project/doc_topics_test.csv")$x







