sieve=function(n){
primes=rep(T,n)
primes[1]=F
p=2
for(i in p:floor(sqrt(n))){
primes[seq(2*p,n,p)]=F
p=p+min(which(primes[(p+1):n]))
}
which(primes)
}

listOfPrimes=sieve(40000)

p1=167 #sample(listOfPrimes,1)
p2=229 #sample(listOfPrimes,1)
productPrime=p1*p2
secret=(p1-1)*(p2-1)
publicKey=22691 #sample(listOfPrimes[listOfPrimes<secret & (secret/listOfPrimes)%%1!=0],1)

egcd=function(a,b){
x=0;y=1;u=1;v=0
while(a!=0){
q=floor(b/a);r=b%%a
m=x-u*q;n=y-v*q
b=a;a=r;x=u;y=v;u=m;v=n}
x}

modInverse=function(a,m) egcd(a,m)%%m

decryptKey=modInverse(publicKey,secret)

message="Be sure to drink your Ovaltine"
possibleCharacters=c(LETTERS,letters,rep(" ",23))

code=function(m) {
m1=unlist(strsplit(m,""))
m2=c()
for(i in 1:length(m1)) {
m2i=which(possibleCharacters==m1[i])
m2[i]=m2i[sample(length(m2i),1)]
}
m2}

codedMessage=code(message)

modx=function(a,b,n){
x=a
for(i in 2:b) x=(x*a)%%n
x}
encryptedSequence=modx(codedMessage,publicKey,productPrime)
decryptedSequence=modx(encryptedSequence,decryptKey,productPrime)

paste(possibleCharacters[decryptedSequence],collapse="")


