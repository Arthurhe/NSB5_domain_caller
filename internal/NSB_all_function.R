
arrowlize=function(input_matrix){
  ptm=proc.time()
  rownum=nrow(input_matrix)
  arrow_matrix=matrix(0,rownum,rownum)
  for(i in 1:(ceiling(rownum/2))){
    new_row=input_matrix[i,]
    new_row[i:(i*2-1)]=new_row[i:1]
    arrow_matrix[i,]=(input_matrix[i,]-new_row)/(input_matrix[i,]+new_row)
  }
  for(i in (ceiling(rownum/2)+1):rownum){
    new_row=input_matrix[i,]
    new_row[i:rownum]=new_row[i:(2*i-rownum)]
    if(length(i:rownum)!=length(i:(2*i-rownum))){stop("shit")}
    arrow_matrix[i,]=(input_matrix[i,]-new_row)/(input_matrix[i,]+new_row)
  }
  arrow_matrix[is.na(arrow_matrix)]=0
  cat(paste("arrowlizing:",timetaken(ptm),"\n"))
  return(arrow_matrix)
}

upper_lower_diff=function(arrow_matrix,largest_domain_allowed){
  ptm=proc.time()
  rownum=nrow(arrow_matrix)
  sumdif_matrix=matrix(0,rownum,rownum)
  for(i in 1:(rownum-2)){
    prevUPsum=0
    prevLOWsum=0
    for(x in (i+1):min(rownum-1,i+largest_domain_allowed)){
      if((2*x-i)<=rownum){
        prevUPsum=prevUPsum+sum(arrow_matrix[i:floor((i+x-1)/2),x],na.rm = T)
        totLOW=ceiling((x-i+1)/2)*(x-i+2+(x-i)%%2)
        prevLOWsum=prevLOWsum+sum(arrow_matrix[x,(x+1):(2*x-i)],na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x],na.rm = T)
      }else{
        totLOW=ceiling((rownum-i+1)/2)*(rownum-i+2+(rownum-i)%%2)
        prevLOWsum=prevLOWsum+sum(arrow_matrix[x,(x+1):rownum],na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x],na.rm = T)
      }
      totUP=ceiling((x-i+1)/2)*(x-i+(x-i)%%2)
      sumdif_matrix[i,x]=prevUPsum-prevLOWsum
      sumdif_matrix[i,x]=prevUPsum/totUP-prevLOWsum/totLOW
    }
  }
  cat(paste("ULdiff of arrowhead matrix:",timetaken(ptm),"\n"))
  return(sumdif_matrix)
}

upper_lower_diff_sign=function(arrow_matrix,largest_domain_allowed){
  ptm=proc.time()
  rownum=nrow(arrow_matrix)
  sumdif_matrix=matrix(0,rownum,rownum)
  arrow_matrix[arrow_matrix>0]=1
  arrow_matrix[arrow_matrix<0]=-1
  for(i in 1:(rownum-2)){
    prevUPsum=0
    prevLOWsum=0
    for(x in (i+1):min(rownum-1,i+largest_domain_allowed)){
      if((2*x-i)<=rownum){
        prevUPsum=prevUPsum+sum(arrow_matrix[i:floor((i+x-1)/2),x],na.rm = T)
        totLOW=ceiling((x-i+1)/2)*(x-i+2+(x-i)%%2)
        prevLOWsum=prevLOWsum+sum(arrow_matrix[x,(x+1):(2*x-i)],na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x],na.rm = T)
      }else{
        totLOW=ceiling((rownum-i+1)/2)*(rownum-i+2+(rownum-i)%%2)
        prevLOWsum=prevLOWsum+sum(arrow_matrix[x,(x+1):rownum],na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x],na.rm = T)
      }
      totUP=ceiling((x-i+1)/2)*(x-i+(x-i)%%2)
      sumdif_matrix[i,x]=prevUPsum-prevLOWsum
      sumdif_matrix[i,x]=prevUPsum/totUP-prevLOWsum/totLOW
    }
  }
  cat(paste("ULdiff of signed_arrowhead matrix:",timetaken(ptm),"\n"))
  return(sumdif_matrix)
}

upper_lower_var=function(arrow_matrix,largest_domain_allowed){
  ptm=proc.time()
  rownum=nrow(arrow_matrix)
  varsum_matrix=matrix(0,rownum,rownum)
  for(i in 1:(rownum-2)){
    prevUPsum=0;prevUPsum2=0;prevLOWsum=0;prevLOWsum2=0
    for(x in (i+2):min(rownum,i+largest_domain_allowed)){
      prevUPsum=prevUPsum+sum(arrow_matrix[i:floor((i+x-1)/2),x],na.rm = T)
      prevUPsum2=prevUPsum2+sum(arrow_matrix[i:floor((i+x-1)/2),x]^2,na.rm = T)
      if((2*x-i)<=rownum){
        totLOW=ceiling((x-i+1)/2)*(x-i+2+(x-i)%%2)/2
        prevLOWsum=prevLOWsum+sum(arrow_matrix[x,(x+1):(2*x-i)],na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x],na.rm = T)
        prevLOWsum2=prevLOWsum2+sum(arrow_matrix[x,(x+1):(2*x-i)]^2,na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x]^2,na.rm = T)
      }else if(x!=rownum){
        totLOW=ceiling((rownum-i+1)/2)*(rownum-i+2+(rownum-i)%%2)/2
        prevLOWsum=prevLOWsum+sum(arrow_matrix[x,(x+1):rownum],na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x],na.rm = T)
        prevLOWsum2=prevLOWsum2+sum(arrow_matrix[x,(x+1):rownum]^2,na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x]^2,na.rm = T)
      }else{
        totLOW=2
        prevLOWsum=0
        prevLOWsum2=0
      }
      totUP=ceiling((x-i+1)/2)*(x-i+(x-i)%%2)/2
      totnum=totUP+totLOW
      prevsum=prevUPsum+prevLOWsum
      prevsum2=prevUPsum2+prevLOWsum2
      varsum_matrix[i,x]=(prevsum2-prevsum^2/totnum)/(totnum-1)
    }
  }
  cat(paste("ULVarSum of arrowhead matrix:",timetaken(ptm),"\n"))
  return(varsum_matrix)
}

upper_lower_var_sep=function(arrow_matrix,largest_domain_allowed){
  ptm=proc.time()
  rownum=nrow(arrow_matrix)
  varsum_matrix=matrix(0,rownum,rownum)
  for(i in 1:(rownum-2)){
    prevUPsum=0;prevUPsum2=0;prevLOWsum=0;prevLOWsum2=0
    for(x in (i+2):min(rownum,i+largest_domain_allowed)){
      prevUPsum=prevUPsum+sum(arrow_matrix[i:floor((i+x-1)/2),x],na.rm = T)
      prevUPsum2=prevUPsum2+sum(arrow_matrix[i:floor((i+x-1)/2),x]^2,na.rm = T)
      if((2*x-i)<=rownum){
        totLOW=ceiling((x-i+1)/2)*(x-i+2+(x-i)%%2)/2
        prevLOWsum=prevLOWsum+sum(arrow_matrix[x,(x+1):(2*x-i)],na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x],na.rm = T)
        prevLOWsum2=prevLOWsum2+sum(arrow_matrix[x,(x+1):(2*x-i)]^2,na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x]^2,na.rm = T)
      }else if(x!=rownum){
        totLOW=ceiling((rownum-i+1)/2)*(rownum-i+2+(rownum-i)%%2)/2
        prevLOWsum=prevLOWsum+sum(arrow_matrix[x,(x+1):rownum],na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x],na.rm = T)
        prevLOWsum2=prevLOWsum2+sum(arrow_matrix[x,(x+1):rownum]^2,na.rm = T)-sum(arrow_matrix[ceiling((i+x)/2):x,x]^2,na.rm = T)
      }else{
        totLOW=2
        prevLOWsum=0
        prevLOWsum2=0
      }
      totUP=ceiling((x-i+1)/2)*(x-i+(x-i)%%2)/2
      varsum_matrix[i,x]=(prevUPsum2-prevUPsum^2/totUP)/(totUP-1)+(prevLOWsum2-prevLOWsum^2/totLOW)/(totLOW-1)
    }
  }
  cat(paste("ULVar_SepSum of arrowhead matrix:",timetaken(ptm),"\n"))
  return(varsum_matrix)
}

total_S=function(upper_lower_diff,upper_lower_var,var_filter_limit=0.2){
  upper_lower_diff[upper_lower_var>=var_filter_limit]=0
  S=upper_lower_diff
  return(S)
}

sum_internal=function(input_matrix,largest_domain_allowed){
  ptm=proc.time()
  rownum=nrow(input_matrix)
  internal_sum_matrix=matrix(0,rownum,rownum)
  for(i in 1:(rownum-1)){
    prevSums=0
    internal_sum_matrix[i,(i+1):min(rownum,i+largest_domain_allowed)]=sapply((i+1):min(rownum,i+largest_domain_allowed),function(x){
      prevSums<<-prevSums+sum(input_matrix[i:(x-1),x])
      return(prevSums)
    })
  }
  cat(paste("InternalSum of arrowhead matrix:",timetaken(ptm),"\n"))
  return(internal_sum_matrix)
}

norm_by_gamma=function(input_matrix,gamma=1){
  rownum=nrow(input_matrix)
  normed_matrix=matrix(0,rownum,rownum)
  normed_matrix[1,2:rownum]=input_matrix[1,2:rownum]/(1:(rownum-1))^gamma
  for(i in 2:(rownum-1)){
    normed_matrix[i,(i+1):rownum]=input_matrix[i,(i+1):rownum]/(1:(rownum-i))^gamma
    normed_matrix[i,i]=(normed_matrix[i,i+1]+normed_matrix[i-1,i])/2
  }
  return(normed_matrix)
}

matrixconverter=function(weight_matrix,domain1,domain2=NULL,strength=1){
  domain1=data.frame(domain1)
  weight_matrix=log10(weight_matrix+1)
  themin=strength*(min(weight_matrix)-max(weight_matrix))
  for(i in 1:nrow(domain1)){
    weight_matrix[domain1[i,2],domain1[i,2]:domain1[i,3]]=themin
    weight_matrix[domain1[i,2]:domain1[i,3],domain1[i,3]]=themin
  }
  if(!is.null(domain2)){
    domain2=data.frame(domain2)
    for(i in 1:nrow(domain2)){
      weight_matrix[domain2[i,2]:domain2[i,3],domain2[i,2]]=themin
      weight_matrix[domain2[i,3],domain2[i,2]:domain2[i,3]]=themin
    }
  }
  return(weight_matrix)
}


boundary_identifier=function(arrow_matrix,smallest_domain_allowed=5){
  ptm=proc.time()
  rownum=nrow(arrow_matrix)
  midpoint=ceiling(smallest_domain_allowed/2)
  vicinityp=matrix(1,2,rownum)
  for(i in 3:(rownum-smallest_domain_allowed+1)){
    if(length(unique(arrow_matrix[i,]))>2){
      tryCatch({
        vicinityp[1,i]=t.test(c(arrow_matrix[i-2,(i+1):(i+smallest_domain_allowed)],arrow_matrix[i-1,(i+1):(i+smallest_domain_allowed)]),
                              c(arrow_matrix[i,(i+1):(i+smallest_domain_allowed)],arrow_matrix[i+1,(i+2):(i+smallest_domain_allowed)]),
                              alternative = "less")$p.value
      },warning=function(war){cat("warning in t.test")},error=function(err){0})
    }
  }
  for(i in (smallest_domain_allowed+1):(rownum-2)){
    if(length(unique(arrow_matrix[,i]))>2){
      tryCatch({
        vicinityp[2,i]=t.test(c(arrow_matrix[(i-smallest_domain_allowed):(i-2),i-1],arrow_matrix[(i-smallest_domain_allowed):(i-1),i]),
                              c(arrow_matrix[(i-smallest_domain_allowed):(i-1),i+1],arrow_matrix[(i-smallest_domain_allowed):(i-1),i+2]),
                              alternative = "greater")$p.value
      },warning=function(war){cat("warning in t.test")},error=function(err){0})
    }
  }
  vicinityp[1,1]=0.05
  vicinityp[2,rownum]=0.05
  vicinityp[is.na(vicinityp)]=0
  rownames(vicinityp)=c("forwardp","bkwardp")
  cat(paste("Doundary identifying:",timetaken(ptm),"\n"))
  return(vicinityp)
}

domain_identifier=function(weight_matrix,sumdif_sign_matrix,varsum_matrix,internalsum_matrix,boundaryP=NA,smallest_domain_allowed=5,largest_domain_allowed=500,gammas=seq(-2,2,0.2)){
  ptm=proc.time()
  cat("domain identifying:\n")
  #1st find domains
  #all_domain=bestGammaScan(weight_matrix,boundaryP,gammas)
  all_domain=localmaxima(weight_matrix,largest_domain_allowed)
  cat(paste("\t-> domain finding:",timetaken(ptm),"\n"));ptm=proc.time()
  #clustering
  domain_dist=dist(all_domain[,1:2,with=F],method = "manhattan",upper = F)
  domain_htree=hclust(domain_dist,method="complete")
  domain_cluster=as.vector(cutree(domain_htree, h=smallest_domain_allowed))
  all_domain[,cluster:=domain_cluster]
  if(!is.na(boundaryP[1])){ #in case there are multiple max(N)
    all_domain=all_domain[,.(x=x[which.min(boundaryP[1,x]*boundaryP[2,y])],y=y[which.min(boundaryP[1,x]*boundaryP[2,y])]),by=cluster]
  }
  all_domain=all_domain[,cluster:=NULL]
  cat(paste("\t-> domain clustering:",timetaken(ptm),"\n"));ptm=proc.time()
  #filtering using var and sign_sum p-val
  all_domain=filterOnVarSumNSignSum(all_domain,sumdif_sign_matrix,varsum_matrix)
  cat(paste("\t-> domain filtering:",timetaken(ptm),"\n"));ptm=proc.time()
  #finetune the remaining domains
  #all_domain=domain_finetuning(sumdif_sign_matrix,varsum_sep_matrix,all_domain)
  #cat(paste("\t-> domain fintuing:",timetaken(ptm),"\n"));ptm=proc.time()
  #filtering using internal sum pval
  #all_domain=domain_filtering_internalsum(internalsum_matrix,all_domain)
  #all_domain=domain_filtering_varsumsep(varsum_sep_matrix,all_domain)
  cat(paste("\t-> domain filtering 2nd:",timetaken(ptm),"\n"));ptm=proc.time()
  all_domain=elemental_domain(all_domain,smallest_domain_allowed)
  cat(paste("\t-> elemental domain:",timetaken(ptm),"\n"));ptm=proc.time()
  return(all_domain)
}

bestdomain=function(weight_matrix,boundaryP=NA){
  if(!is.na(boundaryP[1])){
    boundaryP[boundaryP>0.1]=1
    weight_matrix=(1-boundaryP[1,])%*%t(1-boundaryP[2,])*weight_matrix
    #the former one will be treat as a vertical vector, the later one as a horizontal one
  }
  weight_matrix[(row(weight_matrix)-col(weight_matrix))==-1]=0
  rownum=nrow(weight_matrix)
  bestscore=rep(0,rownum)
  bestdomaindivision=c()
  bestdomaindivision[[1]]=matrix(c(1,1),1,2)
  for(i in 2:rownum){
    scorelist=(bestscore[1:i]+weight_matrix[1:i,i])#/c(1,sapply(bestdomaindivision,length)+1)
    themax=which.max(scorelist)
    bestscore[i]=scorelist[themax]
    if(themax==i){
      bestdomaindivision[[i]]=rbind(bestdomaindivision[[which.max(scorelist[-i])]],c(i,i))
    }else if(themax!=1){
      bestdomaindivision[[i]]=rbind(bestdomaindivision[[themax]],c(themax,i))
    }else{
      bestdomaindivision[[i]]=matrix(c(1,i),1,2)
    }
  }
  return(bestdomaindivision[[rownum]])
}

bestGammaScan=function(weight_matrix,boundaryP=NA,gammas){
  domain_list=c()
  for(i in 1:length(gammas)){
    domain_list[[i]]=bestdomain(norm_by_gamma(weight_matrix,gammas[i]),boundaryP)
  }
  all_domain=do.call(rbind,domain_list)
  all_domain=data.table(all_domain)[,.N,by=c("V1","V2")][N>=max(2,length(gammas)/5)] #filter by showup numbers
  all_domain=all_domain[abs(V2-V1)>1,]
  return(all_domain)
}

shiftbkward <- function(x, k, fillin=NA) {
  return(c(rep(fillin, k), x)[1:length(x)])
}

shiftforward <- function(x, k, fillin=NA) {
  return(c(x,rep(fillin, k))[-(1:k)] )
}

localmaxima=function(weight_matrix,largest_domain_allowed){
  rownum=nrow(weight_matrix)
  xycandicate=matrix(0,ceiling(rownum*largest_domain_allowed/49),2)
  colnames(xycandicate)=c("x","y")
  i=1
  for(x in 1:(rownum-1)){
    tagrow=weight_matrix[x,x:min(x+largest_domain_allowed,rownum)]
    ycandicate=which(tagrow-shiftbkward(tagrow,1,0) > 0)
    ycandicate=ycandicate[which(tagrow[ycandicate]-shiftforward(tagrow,1,0)[ycandicate] > 0)]
    ycandicate=ycandicate+x-1
    for(y in ycandicate){
      if(weight_matrix[x,y]==max(weight_matrix[x,max(y-3,1):min(y+3,rownum)])){
        compmatrix=weight_matrix[x,y]-weight_matrix[max(x-3,1):min(x+3,rownum),max(y-3,1):min(y+3,rownum)]
        if(sum(compmatrix<0)==0 & sum(compmatrix==0)==1){
          xycandicate[i,]=c(x,y)
          i=i+1
        }
      }
    }
  }
  xycandicate=xycandicate[1:(i-1),]
  xycandicate=data.table(xycandicate)
  xycandicate=xycandicate[abs(y-x)>1,]
  return(xycandicate)
}

domain_finetuning=function(sumdif_sign_matrix,varsum_sep_matrix,all_domain){
  #all_domain is data.table
  all_domain=data.frame(all_domain)
  rownum=nrow(sumdif_sign_matrix)
  varsum_matrix[abs(row(varsum_sep_matrix)-col(varsum_sep_matrix))<2]=0
  for(i in 1:nrow(all_domain)){
    cordif=Inf
    prevcenter=all_domain[i,1:2]
    while(cordif>0){
      x=all_domain[i,1]
      y=all_domain[i,2]
      wcontex=sumdif_sign_matrix[seq(max(x-1,1),min(x+1,rownum)),seq(max(y-1,1),min(y+1,rownum))]
      vcontex=varsum_sep_matrix[seq(max(x-1,1),min(x+1,rownum)),seq(max(y-1,1),min(y+1,rownum))]
      min_center=which(vcontex==min(vcontex),arr.ind = T)
      if(nrow(min_center)>1){
        min_center=min_center[which(wcontex[min_center]==max(wcontex[min_center])),]
        if(length(min_center)>2){min_center=min_center[which.max(rowSums(min_center)),]}
      }
      all_domain[i,1:2]=c(max(x-1,1),max(y-1,1))+min_center-1
      cordif=sum(all_domain[i,1:2]-prevcenter)
      prevcenter=all_domain[i,1:2]
    }
  }
  all_domain=unique(all_domain)
  all_domain=all_domain[abs(all_domain[,2]-all_domain[,1])>1,]
  all_domain=data.table(all_domain)
  return(all_domain)
}

filterOnVarSumNSignSum=function(all_domain,sumdif_sign_matrix,varsum_matrix){
  all_domain=as.matrix(all_domain)
  all_domain=cbind(all_domain,sumdif_sign_matrix[all_domain],varsum_matrix[all_domain])
  colnames(all_domain)=c("x","y","sumdifsign","varsum")
  all_domain=data.table(all_domain)
  sumdif_kmean=kmeans(all_domain$sumdifsign,c(min(all_domain$sumdifsign),max(all_domain$sumdifsign)))
  varsum_kmean=kmeans(all_domain$varsum,c(min(all_domain$varsum),max(all_domain$varsum)))
  all_domain[,':='(sumdifAssign=pnorm(all_domain$sumdifsign,sumdif_kmean$centers[2],sd(all_domain$sumdifsign[sumdif_kmean$cluster==2])),
                   varsumAssign=pnorm(all_domain$varsum,varsum_kmean$centers[2],sd(all_domain$varsum[varsum_kmean$cluster==2])))]
  all_domain=all_domain[sumdifAssign>0.005 & varsumAssign>0.005]
  #all_domain=all_domain[sumdif_kmean$cluster==2 & varsum_kmean$cluster==2]
  all_domain[,':='(sumdifsign=NULL,varsum=NULL,sumdifAssign=NULL,varsumAssign=NULL)]#
  return(all_domain)
}

domain_filtering_varsumsep=function(varsum_sep_matrix,all_domain,threshold=0.05){ #old filtering
  all_domain=data.frame(all_domain)
  rownum=nrow(varsum_sep_matrix)
  all_domain=all_domain[order(all_domain[,1],all_domain[,2]),]
  varsum_p=rep(1,nrow(all_domain))
  for(i in 1:nrow(all_domain)){
    x=all_domain[i,1]
    y=all_domain[i,2]
    domain_length_half=round((y-x)/2)
    if(x>2 & (y-x)>4){
      varsum_p[i]=t.test(varsum_sep_matrix[x,(x+2):y],varsum_sep_matrix[max(x-2,1):(x-1),(x+2):y],alternative = "less")$p.value
    }
  }
  all_domain=cbind(all_domain,varsum_p)
  all_domain=data.table(all_domain)
  #all_domain=all_domain[rowMins(varsum_p)<threshold,]
  return(all_domain)
} # can not differetiate boundary and corner

domain_filtering_internalsum=function(internalsum_matrix,all_domain,threshold=0.05){ #old filtering
  all_domain=data.frame(all_domain)
  rownum=nrow(internalsum_matrix)
  all_domain=all_domain[order(all_domain[,1],all_domain[,2]),]
  intnlsum_p=matrix(1,nrow(all_domain),2)
  for(i in 1:nrow(all_domain)){
    x=all_domain[i,1]
    y=all_domain[i,2]
    domain_length_half=round((y-x)/2)
    intersum_contextl=internalsum_matrix[col(internalsum_matrix)-row(internalsum_matrix)==y-x][max(x-max(domain_length_half,3),0):(x-1)]
    #intersum_contextl=intersum_context[which(!is.na(intersum_context))]
    intersum_contextr=internalsum_matrix[col(internalsum_matrix)-row(internalsum_matrix)==y-x][(x+1):min(x+max(domain_length_half,3),rownum-y+x+1)]
    #intersum_contextr=intersum_context[which(!is.na(intersum_context))]
    intnlsum_p[i,1]=tryCatch(1-pnorm(internalsum_matrix[x,y],mean = mean(intersum_contextr),sd=sd(intersum_contextr))
                             ,warning=function(war){0},error=function(err){0})
    intnlsum_p[i,2]=tryCatch(1-pnorm(internalsum_matrix[x,y],mean = mean(intersum_contextl),sd=sd(intersum_contextl))
                             ,warning=function(war){0},error=function(err){0})
  }
  intnlsum_p=rowMaxs(intnlsum_p,na.rm = T)
  intnlsum_p_kmean=kmeans(intnlsum_p,c(min(intnlsum_p),max(intnlsum_p)))
  all_domain=data.table(all_domain)
  all_domain=all_domain[intnlsum_p_kmean$cluster==1,]
  return(all_domain)
}

MergeNegRegion = function(bed,pass_list,region_limit){
  decisionlist=which(c(pass_list[1],pass_list-c(pass_list[-1],0))!=0)
  region_mat=matrix(decisionlist[-c(1,length(decisionlist))],length(decisionlist)/2-1,2,byrow = T)
  region_mat[,2]=region_mat[,2]-1
  #now create the matrix for return
  returnmatrix=data.frame(matrix(0,nrow(region_mat),ncol(bed)))
  colnames(returnmatrix)=colnames(bed)
  for(i in 1:nrow(region_mat)){
    #check if the start and end are diff
    if(region_mat[i,1]!=region_mat[i,2]){
      insertion_at=i
      returnmatrix[insertion_at,1]=bed[region_mat[i,1],1]
      #check whether the contiguous bins are continuous (there may be break between them)
      for(k in region_mat[i,1]:(region_mat[i,2]-1)){
        if(bed[k,2]==bed[k+1,1]){
          returnmatrix[insertion_at,2]=bed[k+1,2]
        }else{
          returnmatrix=rbind(returnmatrix,bed[k+1,])
          insertion_at=nrow(insertion_at)
        }
      }
    }else{
      returnmatrix[i,]=bed[region_mat[i,1],]
    }
  }
  returnmatrix=returnmatrix[returnmatrix[,2]-returnmatrix[,1]>=region_limit,]
  return(returnmatrix)
}

elemental_domain=function(all_domain,smallest_domain_allowed){
  #initialize created mark list
  all_domain[,created:=F]
  #find the one that don't overlap with any smaller one
  elements=1:max(all_domain$y)#a list showing whether a region has been covered or now
  elemental=rep(F,nrow(all_domain))
  all_domain=all_domain[order(all_domain$y-all_domain$x,decreasing = F)]
  for(i in 1:nrow(all_domain)){
    tag=elements[elements>all_domain$x[i] & elements<all_domain$y[i]] #all position that overlap except boundary
    if(length(tag)==all_domain$y[i]-all_domain$x[i]-1){
      elements=setdiff(elements,c(tag,all_domain$x[i],all_domain$y[i])) #remove internal and boundary position from elemental list
      elemental[i]=T
    }
  }
  all_domain[,elemental:=elemental]
  #create new bed
  boundary=sort(unique(c(all_domain$x,all_domain$y)))
  new_domain=matrix(c(boundary[-length(boundary)],boundary[-1]),length(boundary)-1,2)
  colnames(new_domain)=c("x","y")
  #discard all new domain that overlap with elemental domain
  keep=rep(F,nrow(new_domain))
  for(i in 1:nrow(new_domain)){
    tag=elements[elements>=new_domain[i,1] & elements<=new_domain[i,2]]
    if(length(tag)>0){
      elements=setdiff(elements,tag)
      keep[i]=T
    }
  }
  new_domain=new_domain[keep,]
  #merge the remaining domain if they are smaller than the smallest domain allowed
  size_pass=(new_domain[,2]-new_domain[,1])>=smallest_domain_allowed
  new_domain=rbind(new_domain[size_pass,],MergeNegRegion(new_domain,size_pass,smallest_domain_allowed))
  #finish the markers for new_domain
  new_domain=data.table(new_domain)
  new_domain[,':='(created=T,elemental=T)]
  all_domain=rbind(all_domain,new_domain)
  all_domain=all_domain[order(all_domain$x),]
  return(all_domain)
}

NSB_domain_call=function(input_manifest,bin_size,smallest_domain_allowed,largest_domain_allowed,output_prefix){
  smallest_domain_allowed=ceiling(smallest_domain_allowed/bin_size)
  largest_domain_allowed=ceiling(largest_domain_allowed/bin_size)
  all_domain=list()
  for(jb in 1:nrow(input_manifest)){
    input_matrix = suppressWarnings (as.matrix (fread (input_manifest[jb,2], sep="\t", stringsAsFactors=F, data.table=F)))
    if(nrow(input_matrix)!=ncol(input_matrix)){stop("row num must identical to col num")}
    chr=input_manifest[jb,1]
    ptm <- proc.time()
    cat(paste("now processing;",chr,"\t",input_manifest[jb,2],"\n"))
    arrow_matrix=arrowlize(input_matrix)
    sumdif_matrix=upper_lower_diff(arrow_matrix,largest_domain_allowed)
    sumdif_sign_matrix=upper_lower_diff_sign(arrow_matrix,largest_domain_allowed)
    varsum_matrix=upper_lower_var(arrow_matrix,largest_domain_allowed)
    #varsum_sep_matrix=upper_lower_var_sep(arrow_matrix,largest_domain_allowed)
    internalsum_matrix=sum_internal(input_matrix,largest_domain_allowed)
    boundaryP=boundary_identifier(arrow_matrix,5)
    all_domain[[chr]]=domain_identifier(sumdif_matrix,sumdif_sign_matrix,varsum_matrix,internalsum_matrix,boundaryP,smallest_domain_allowed,largest_domain_allowed,gammas=seq(-2,2,0.1))
    #all_domain=domain_filtering(varsum_sep_matrix,internalsum_matrix,all_domain)
    all_domain[[chr]][,':='(chr=chr,size=1)]
    setcolorder(all_domain[[chr]],c("chr","x","y","size","elemental","created"))
    all_domain[[chr]][,':='(x=(x-1)*bin_size,y=y*bin_size,size=(y-x+1)*bin_size)]
    cat(paste("all done:",timetaken(ptm),"\n"))
    options(scipen=100)
    write.table(all_domain[[chr]],file=paste0(output_prefix,"_",chr,"_domains_",bin_size,".bed"),sep="\t",col.names = T, row.names = F,quote = F)
  }
  
  tot_domain=do.call(rbind,all_domain)
  tot_domain=as.data.frame(tot_domain)
  tot_ele_domain=tot_domain[tot_domain$elemental,]
  options(scipen=100)
  write.table(tot_domain,file=paste0(output_prefix,"_all_domains_",bin_size,".bed"),sep="\t",col.names = T, row.names = F,quote = F)
  write.table(tot_ele_domain,file=paste0(output_prefix,"_elemental_domains_",bin_size,".bed"),sep="\t",col.names = T, row.names = F,quote = F)
  return(tot_ele_domain)
}

KR_normalization=function(tagMatrix,ignore_row=0,ignore_col=0,sparse_limit=0.95,outname){
  #place argument
  if (ignore_row>0) {ignore_row=seq(1,ignore_row)}else{ignore_row=0}
  if (ignore_col>0) {ignore_col=seq(1,ignore_col)}else{ignore_col=0}
  #read table
  A=as.matrix(fread(tagMatrix,sep="\t",stringsAsFactors=FALSE,skip=ignore_row,drop=ignore_col,data.table=F))
  
  #removing sparse row
  A_0number=rowSums(A==0)
  rowtoSave=A_0number<nrow(A)*as.numeric(sparse_limit)
  
  Anew=A[rowtoSave,rowtoSave]
  Anew_sum=sum(Anew)
  A_rownum=nrow(Anew)
  
  coverge_threshold=1e-15*A_rownum^2
  purtubing_val=1e-15
  r=rep(1,A_rownum)
  c=r
  d=t(Anew)%*%r+purtubing_val*sum(r)
  while(1){
    c=1/d
    r=1/(Anew%*%c+purtubing_val*sum(c))
    d=t(Anew)%*%r+purtubing_val*sum(r)
    if(sum(abs(c*d-1))<=coverge_threshold) break
  }
  
  Anew=diag(as.vector(r))%*%Anew%*%diag(as.vector(c))
  A[rowtoSave,rowtoSave]=Anew*Anew_sum/sum(Anew)
  A=A*nrow(A)^2/sum(A)
  
  write.table(A,file=outname,row.names = F,col.names = F,quote = F,sep="\t")
}

domain_refine_prefilteringRegionCreation=function(domains,obinSize,outname){
  options(scipen=100) #must set, or 45000000 will be converted to 45E6 in string
  chrs=gtools::mixedsort(unique(domains[,1]))
  tempout=list()
  for(i in 1:length(chrs)){
    chr=chrs[i]
    tags=which(domains[,1]==chr)
    tempin=domains[tags,]
    domains=domains[-tags,]
    tempin=tempin[order(tempin[,2],tempin[,3]),]
    tempout[[i]]=data.frame(chr=chr,sta=0,sto=rep(0,nrow(tempin)-1),stringsAsFactors = F)
    for(k in 1:(nrow(tempin)-1)){
      tempout[[i]][k,2]=round(min(tempin[k,3],tempin[k+1,2])-obinSize*1.25)
      tempout[[i]][k,3]=round(max(tempin[k,3],tempin[k+1,2])+obinSize*1.25)
    }
  }
  out=do.call(rbind,tempout)
  write.table(out,file=outname,row.names = F,col.names = F,quote = F,sep="\t")
  return(out)
}

region_expanding_fixedSize=function(widsize,expnum,loops_table,outname){
  options(scipen=100)
  expandsize=widsize*expnum
  start=loops_table[,2]
  end=loops_table[,3]
  division=ceiling((end-start)/widsize)
  totExpandPerRegion=division+2*expnum
  temp=lapply(seq(1:length(start)),function(x){  #sapply(simplify=..)
    starts=seq(start[x]-expandsize,end[x]+expandsize-1,widsize)
    ends=seq(start[x]-expandsize+widsize,end[x]+expandsize+widsize-1,widsize)
    out=cbind(rep(loops_table[x,1],totExpandPerRegion[x]),starts,ends,rep(x,totExpandPerRegion[x]))  #this is fast because all columns are converted to characters..., sothat rbind is fast
    return(out)
  })
  bed=do.call(rbind,temp)
  bed[,2][as.numeric(bed[,2])<=0]=0
  bed[,3][as.numeric(bed[,3])<=1]=1
  write.table(bed,file=outname,row.names = F,col.names = F,quote = F,sep="\t")
  return(bed)
}


NSB_domain_refine=function(domains,obinSize,readcountPlus,readcountMinus,outname){
  options(scipen=100) #must set, or 45000000 will be converted to 45E6 in string
  readcountPlus=fread(readcountPlus,sep="\t",stringsAsFactors = F,data.table=F)
  readcountMinus=fread(readcountMinus,sep="\t",stringsAsFactors = F,data.table=F)
  
  colnames(readcountPlus) <- c('chr','start','end','id','counts')
  colnames(readcountMinus) <- c('chr','start','end','id','counts')
  readcountPlus=GRanges(seqnames=Rle(readcountPlus$chr),
                        ranges=IRanges(readcountPlus$start, readcountPlus$end),
                        score=readcountPlus$count,
                        id=readcountPlus$id)
  readcountMinus=GRanges(seqnames=Rle(readcountMinus$chr),
                         ranges=IRanges(readcountMinus$start, readcountMinus$end),
                         score=readcountMinus$count,
                         id=readcountMinus$id)
  
  #sort the file first and deal with each boundary by considering the two neighboring domains
  chrs=gtools::mixedsort(unique(domains[,1]))
  tempout=list()
  for(i in 1:length(chrs)){
    chr=chrs[i]
    tags=which(domains[,1]==chr)
    tempin=domains[tags,]
    tempin=tempin[order(tempin[,2],tempin[,3]),]
    #deal with the boundary in this chromosome
    for(k in 1:(nrow(tempin)-1)){
      sta=round(min(tempin[k,3],tempin[k+1,2])-obinSize*1.25)
      sto=round(max(tempin[k,3],tempin[k+1,2])+obinSize*1.25)
      #GRange interset
      CurrentRegion=GRanges(seqnames=chr, IRanges(sta, sto))
      CurrentRegion_splited_plus=subsetByOverlaps(readcountPlus, CurrentRegion)
      CurrentRegion_splited_minus=subsetByOverlaps(readcountMinus, CurrentRegion)
      dscore=CurrentRegion_splited_plus$score-CurrentRegion_splited_minus$score
      dscore=dscore+c(dscore[-1],0)+c(dscore[-c(1,2)],0,0)
      dscore=dscore[1:(length(dscore)-2)]
      if(min(dscore)<0){
        boundaryBwd=end(CurrentRegion_splited_plus)[which.min(dscore)+2]+1250
      }else{
        boundaryBwd=start(CurrentRegion_splited_plus)[1]
      }
      if(max(dscore)>0){
        boundaryFwd=start(CurrentRegion_splited_plus)[max(which(dscore==max(dscore)))]-1250
      }else{
        boundaryFwd=end(CurrentRegion_splited_plus)[length(CurrentRegion_splited_plus)]
      }    
      tempin[k,3]=boundaryBwd
      tempin[k+1,2]=boundaryFwd
    }
    tempout[[i]]=tempin
  }
  out=do.call(rbind,tempout)
  #filter out too small and inverted domain
  notpass=which(out[,3]-out[,2]<=25000)
  if(length(notpass)>0){out=out[-notpass,]}
  write.table(out,file=outname,row.names = F,col.names = F,quote = F,sep="\t")
  return(out)
}