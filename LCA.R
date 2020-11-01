set.seed(1234)
install.packages("poLCA")
library(poLCA)
install.packages("cluster.datasets")
library(cluster.datasets)
install.packages("dplyr")


f<-cbind(socact1_group,socact2_group,socact3_group,socact4_group,socact5_group,socact6_group,socact7_group,socact8_group,socact9_group,socact10_group,socact11_group,socact12_group)~1
LCA2r<-poLCA(f,LCA_data1urban,nclass = 2)
LCA3r<-poLCA(f,LCA_data1urban,nclass = 3)
LCA4r<-poLCA(f,LCA_data1urban,nclass = 4)
LCA5r<-poLCA(f,LCA_data1urban,nclass = 5)
LCA6r<-poLCA(f,LCA_data1urban,nclass = 6)
LCA7r<-poLCA(f,LCA_data1urban,nclass = 7)
LCA8r<-poLCA(f,LCA_data1urban,nclass = 8)

poLCA.entropy(LCA2r)
poLCA.entropy(LCA3r)
poLCA.entropy(LCA4r)
poLCA.entropy(LCA5r)
poLCA.entropy(LCA6r)
poLCA.entropy(LCA7r)
##calculating 0-1 relative entropy 
nume.E2<--sum(LCA2r$posterior*log(LCA2r$posterior))
deno.E2<-1917*log(2)
entro2<-1-(nume.E2/deno.E2)
entro2
nume.E3<--sum(LCA3r$posterior*log(LCA3r$posterior))
deno.E3<-1917*log(3)
entro3<-1-(nume.E3/deno.E3)
entro3
nume.E4<--sum(LCA4r$posterior*log(LCA4r$posterior))
deno.E4<-1917*log(4)
entro4<-1-(nume.E4/deno.E4)
entro4
nume.E5<--sum(LCA5r$posterior*log(LCA5r$posterior))
deno.E5<-1917*log(5)
entro5<-1-(nume.E5/deno.E5)
entro5
nume.E6<--sum(LCA6r$posterior*log(LCA6r$posterior))
deno.E6<-1917*log(6)
entro6<-1-(nume.E6/deno.E6)
entro6


y2=LCA4r$predclass
LCA_data2rural=cbind(LCA_data1rural,y2)

table(LCA_data2rural$y2)

LCA2u<-poLCA(f,LCA_data1urban,nclass = 2)
LCA3u<-poLCA(f,LCA_data1urban,nclass = 3)
LCA4u<-poLCA(f,LCA_data1urban,nclass = 4)
LCA5u<-poLCA(f,LCA_data1urban,nclass = 5)
LCA6u<-poLCA(f,LCA_data1urban,nclass = 6)
LCA7u<-poLCA(f,LCA_data1urban,nclass = 7)
LCA8u<-poLCA(f,LCA_data1urban,nclass = 8)
??output
poLCA.entropy(LCA2u)
poLCA.entropy(LCA3u)
poLCA.entropy(LCA4u)
poLCA.entropy(LCA5u)
poLCA.entropy(LCA6u)
poLCA.entropy(LCA7u)


