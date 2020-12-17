
# Continuity corrections
ContCorr<-function(data1,ContC=c("Constant","TAC","Empirical05","No"),cc=0.5){
a=data1$ai;b=data1$bi;c1=data1$ci;d=data1$di; n1=data1$ni1; n2=data1$ni2
nstud = dim(data1)[1]
  
if(ContC=="Constant"){
  for (l in 1:nstud){
    if (a[l]==0 | b[l]==0 | c1[l]==0 |d[l]==0) {
      a[l] = a[l]+cc; b[l] = b[l]+cc; c1[l] = c1[l]+cc; d[l] = d[l]+cc ; n1[l]=n1[l]+2*cc ; n2[l]=n2[l]+2*cc 
    }}}
if(ContC=="TAC"){
  for (l in 1:nstud){
    if (a[l]==0 | b[l]==0 | c1[l]==0 |d[l]==0) {
      a[l] = a[l]+(cc/(c1[l]+d[l]));  b[l] = b[l]+(cc/(c1[l]+d[l])); 
      c1[l] = c1[l]+(cc/(a[l]+b[l])); d[l] = d[la]+(cc/(a[l]+b[l])) ; 
      n1[l]=n1[l]+2*(cc/(c1[l]+d[l])) ; n2[l]=n2[l]+2*(cc/(a[l]+b[l]))
    }}}
if(ContC=="Empirical05"){
  Cond<- a !=0 & b!=0 & c1 !=0 & d!=0
  aa=a[Cond] ; bb=b[Cond] ; cc=c1[Cond] ; dd=d[Cond]
  OR = (aa*dd)/(cc*bb)
  sigma = 1/aa + 1/bb + 1/cc + 1/dd
  w=1/sigma
  POR = sum(w*OR)/sum(w)
  alloc = n1/n2
  kt = POR / (POR+alloc) ; kc = alloc / (POR+alloc)
  for (l in 1:nstud){
    if (a[l]==0 | b[l]==0 | c1[l]==0 |d[l]==0) {
      a[l] = a[l]+kt[l]; b[l] = b[l]+kt[l]; c1[l] = c1[l]+kc[l]; d[l] = d[l]+kc[l] ; n1[l]=n1[l]+(kt[l]+kc[l]) ; n2[l]=n2[l]+(kt[l]+kc[l])
    }}}
if(ContC=="No"){
  for (l in 1:nstud){
    if (a[l]==0 | b[l]==0 | c1[l]==0 |d[l]==0) {
      a[l] = a[l]; b[l] = b[l]; c1[l] = c1[l]; d[l] = d[l] ; n1[l]=n1[l] ; n2[l]=n2[l]
    }}}
data1$ai<-a ; data1$bi<-b ; data1$ci<-c1 ; data1$di<-d ; data1$ni1<-n1 ; data1$ni2<-n2
return(data1)
}