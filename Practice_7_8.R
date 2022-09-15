####################################################################
### 0. Installing packages from Bioconductor, CRAN, or GitHub
####################################################################
if (!requireNamespace("BiocManager")) install.packages("BiocManager")
BiocManager::install(c("pheatmap","RColorBrewer", "edgeR","DESeq2","msigdbr","fgsea","xlsx"))

####################################################################
### 1. Reading/writing data sets in R
####################################################################
wdir = "E:/Rstudy/data"	# a directory where data sets are located
list.files(wdir)

sinfo = read.csv(file=sprintf("%s/sample_info.csv",wdir), header=T, check.names=F, stringsAsFactors=F) #1840 x 29
#1840 CCLs x 29 features, cell line information

rcm = read.csv(file=sprintf("%s/CCLE_expression_proteincoding_genes_expected_count.csv",wdir), header=T, row.names=1, check.names=F) 
rcm=t(rcm)	#19221 genes x 1406 CCLs, read count data for DEG analysis
rownames(rcm) = gsub(" \\(.*","",rownames(rcm))
rcm=round(rcm,digits=0)

tpm = read.csv(file=sprintf("%s/CCLE_expression.csv",wdir), header=T, row.names=1, check.names=F) 
tpm=t(tpm)	#19221 genes x 1406 CCLs, normalized TPM data for clustering analysis
rownames(tpm)=gsub(" \\(.*","",rownames(tpm))

identical(colnames(rcm),colnames(tpm))
identical(rownames(rcm),rownames(tpm))

ds = read.csv(file=sprintf("%s/Drug_sensitivity_AUC_(CTD^2).csv",wdir)
	, header=T, check.names=F, stringsAsFactors=F, row.names=1) #815 CCLs x 543 treatments, drug response data
colnames(ds)=gsub(" \\(CTRP.*","",colnames(ds))

icell = intersect( colnames(rcm), rownames(ds) )	#799
rcm=rcm[,match(icell, colnames(rcm))]	#19221 genes x 799 CCLs
tpm=tpm[,match(icell, colnames(tpm))]	#19221 genes x 799 CCLs
ds=ds[match(icell, rownames(ds)),]		#799 CCLs x 543 treatments
sinfo = sinfo[match(icell, sinfo$DepMap_ID),] #799 CCLs x 29 features

save(sinfo, rcm, tpm, ds, file=sprintf("%s/CCLE_CTRP_Data.Rdata",wdir))

####################################################################
### 2. Distribution of gene expression and AUC values
####################################################################
wdir = "E:/Rstudy/data"
load(file= sprintf("%s/CCLE_CTRP_Data.Rdata",wdir)) #sinfo, rcm, tpm, ds

ri = sample(1:ncol(rcm),9)
par(mfrow=c(3,3))
for(i in ri){
	hist(log2(rcm[,i]),breaks=30 , main=colnames(rcm)[i], xlab="Read count")
}

rcm_sd = apply(log2(rcm),1,sd)
hist(rcm_sd, breaks=20, col="skyblue", xlab="SD of read count",main="")

ri = sample(1:ncol(tpm),9)
par(mfrow=c(3,3))
for(i in ri){
	hist(tpm[,i],breaks=30 , xlim=c(0,10), main=colnames(tpm)[i], xlab="log2(TPM+1)")
}
tpm_sd = apply(tpm,1,sd)
hist(tpm_sd, breaks=20, col="skyblue", xlab="SD of TPM",main="")

ri = sample(1:ncol(ds),9)
par(mfrow= c(3,3))
for(i in ri){
	hist(ds[,i],breaks=30 , xlim=c(0,20), main=colnames(ds)[i], xlab="AUC")
}
auc_sd = apply(ds,2,sd, na.rm=T)
hist(auc_sd, breaks=20, col="skyblue", xlab="SD of AUC",main="")

####################################################################
### 3. Choosing a drug with the highest variance in AUC
####################################################################
auc_sd = apply(ds,2,sd, na.rm=T)
which(auc_sd == max(auc_sd))	# oligomycin A
dsv= ds[,"oligomycin A"] 		  # dsv=ds[,which(sds==max(sds))]
names(dsv) = rownames(ds)
dsv=dsv[!is.na(dsv)]			    #758 CCLs
# dsv=dsv[!is.na(dsv) & !(sinfo$lineage %in% c("blood","lymphocyte","unknown"))]

hist(dsv,breaks=30, col="orange", xlim=c(0,18),xlab="AUC", main="oligomycin A", prob=T)
lines(density(dsv,na.rm=T), col = "black") # Overlay density curve
q2=quantile(dsv,prob=c(0.01,0.99))
abline(v=q2,col="black",lty=3,lwd=2)
text(x=q2[2], y=0.13, "< R", adj=0, col="red", xpd=T)
text(x=q2[1], y=0.13, "S <", adj=1, col="blue", xpd=T)

cinfo=rbind( data.frame(Response="S",CCL=names(dsv)[dsv<=q2[1]])
			, data.frame(Response="R",CCL=names(dsv)[dsv>=q2[2]]) )
cinfo=cbind(cinfo,sinfo[match(cinfo$CCL, sinfo$DepMap_ID),c(13,18,2)])
stpm=tpm[,match(cinfo$CCL, colnames(tpm)) ]
srcm=rcm[,match(cinfo$CCL, colnames(rcm)) ]
save(stpm, srcm, cinfo,file=sprintf("%s/Oligomycin_SR_transcriptome.Rdata",wdir))


####################################################################
### 4. transcriptome-based clustering - PCA, correlation
####################################################################
library(pheatmap)
library(RColorBrewer)
wdir = "E:/Rstudy/data"
load(file=sprintf("%s/Oligomycin_SR_transcriptome.Rdata",wdir)) #stpm, srcm, cinfo

sds=apply(stpm,1,sd)
hist(sds, breaks=30,col="skyblue")
xx = stpm[which(sds>=0.5),]	#13435 x 16

cl=c("darkgreen","darkorange")
names(cl)=c("S","R")
cls=cl[cinfo$Response]

pca=prcomp( t(xx), scale=TRUE)
# pca$sdev[1]/sum(pca$sdev)*100 #12.3%
# pca$sdev[1]/sum(pca$sdev)*100 #10.2%

plot(pca$x[,1], pca$x[,2] ,col=cls ,pch=19 , cex=1, xlab="PC1", ylab="PC2"
	, main=sprintf("PCA - %d genes",nrow(xx)), cex.main=1.2, xlim=c(-80,100))
text(pca$x[,1],pca$x[,2]+3, cinfo$primary_disease, col=cls,cex=1)
legend("topright",names(cl), col=cl, pch=19)

cm=cor(xx, method="pearson")
annot=cinfo[,c(1,3,4,5)]
rownames(annot)=cinfo$CCL
hcls=colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100)
pheatmap(cm, col=hcls, annotation=annot[,c(1,3)]
	, clustering_distance_rows = "correlation", clustering_distance_cols = "correlation"
	, labels_col=annot$cell_line_name, labels_row=annot$cell_line_name)

####################################################################
### 5. Differential gene expression (DGE) analysis
####################################################################
library(edgeR)
library(DESeq2)
grp=factor(cinfo$Response, levels=c("S","R") )
keep = filterByExpr(srcm, group=grp)
srcm=srcm[which(keep),]			#filtering genes by expression levels

coldata=data.frame(Group=grp)	#column info for DGE analysis
rownames(coldata)= colnames(srcm)

dds=DESeqDataSetFromMatrix(countData = srcm,  #read count
                           colData = coldata, #sample info
                           design = ~ Group)  #group info
dds=DESeq(dds)
degm=results(dds, pAdjustMethod = "fdr",independentFiltering=F)
degm=as.data.frame(degm)
#identical(rownames(degm), rownames(srcm))
degm=data.frame(srcm, degm, check.names=F)
degm=degm[order(degm$padj),]

par(mfrow=c(2,3))
for(g in rownames(degm)[1:6]){
	plotCounts(dds, gene=g, intgroup="Group")
}

save(degm, cinfo, file=sprintf("%s/DESeq2_DEG.Rdata",wdir)) 
write.table(degm, file=sprintf("%s/DESeq2_DEG.txt",wdir), col.names=T, sep="\t",quote=F)
####################################################################
### 6.Plotting differentially expressed genes (DEGs)
####################################################################
degm$logFDR= -log10(degm$padj)
plot(degm$log2FoldChange,degm$logFDR, xlab="log2FC", ylab="log10FDR"
	, xlim=c(-10,10), col="black",bg="darkgrey", pch=21)
abline(v=c(-log2(1.5),log2(1.5)),lty=3,col="red")
abline(h=2,lty=3,col="red")
uidx = degm$log2FoldChange >= log2(1.5) & degm$logFDR >=2
didx = degm$log2FoldChange <= -log2(1.5) & degm$logFDR >=2
points(x=degm$log2FoldChange[uidx],y=degm$logFDR[uidx], col="black",bg="red", pch=21,cex=1.2)
points(x=degm$log2FoldChange[didx],y=degm$logFDR[didx], col="black",bg="blue", pch=21,cex=1.2)
legend("topleft",c("Up-DEGs","Down-DEGs"),pt.bg=c("red","blue"), pch=21, pt.cex=1.2)

uidx = degm$log2FoldChange >= log2(1.5) & degm$logFDR >=3
didx = degm$log2FoldChange <= -log2(1.5) & degm$logFDR >=3
text(x=degm$log2FoldChange[didx],y=degm$logFDR[didx]+0.3,rownames(degm)[didx],col="blue",cex=.7)
text(x=degm$log2FoldChange[uidx],y=degm$logFDR[uidx]+0.3,rownames(degm)[uidx],col="red",cex=.7)

degs=rownames(degm)[uidx|didx]
xx=stpm[rownames(stpm) %in%degs,]
annot=cinfo[,c(1,3,4,5)]
rownames(annot)=cinfo$CCL
hcls=colorRampPalette(rev(brewer.pal(n = 7, name ="RdYlBu")))(100)

pheatmap(xx, col=hcls, scale ="row", annotation=annot[,c(1,3)]
	, clustering_distance_rows = "correlation"
	, clustering_distance_cols = "correlation"
	, labels_col=annot$cell_line_name, fontsize=8)

####################################################################
### 7.Performing gene set enrichment analysis (GSEA) 
####################################################################
library(msigdbr)
library(fgsea)
library(xlsx)
wdir = "E:/Rstudy/data"
load(file=sprintf("%s/DESeq2_DEG.Rdata",wdir)) #degm
rk=degm$stat
names(rk)= rownames(degm)

gsets = msigdbr(species = "Homo sapiens", category="H") #load
gdf= as.data.frame(gsets)
gl= split(x = gdf$gene_symbol, f = gdf$gs_name)

res = fgsea(pathways =gl, stats = rk, minSize=10, maxSize=500, nperm=10000)
res = as.data.frame(res)
res=res[which(res$pval <0.05),]
res=res[order(res$pval),]
res$leadingEdge = sapply(res$leadingEdge , paste, collapse=", ")
write.xlsx(res, file =sprintf("%s/MSigDB_GSEA.xlsx",wdir),sheetName="H",row.names=F)


gsets = msigdbr(species = "Homo sapiens", category="C2",subcategory="KEGG")
gdf= as.data.frame(gsets)
gl= split(x = gdf$gene_symbol, f = gdf$gs_name)

res = fgsea(pathways =gl, stats = rk, minSize=10, maxSize=500, nperm=10000)
res=as.data.frame(res)
res=res[which(res$pval <0.05),]
res=res[order(res$pval),]
res$leadingEdge = sapply(res$leadingEdge , paste, collapse=", ")
write.xlsx(res, file =sprintf("%s/MSigDB_GSEA.xlsx",wdir),sheetName="C2_KEGG",row.names=F,append=T)


####################################################################
### 8.plotting top-enriched pathways
####################################################################
score=sign(res$NES)*-log10(res$pval)
names(score)=res$pathway
names(score)=gsub("_"," ",gsub("KEGG_"," ",names(score)))

ups=score[res$NES>0][1:7]
dns=score[res$NES<0][1:7]

cl=colorRampPalette(c("red","white","blue"))(20)[c(6,14)]

bp=barplot(rev(ups), xlim = c(0,5), horiz = TRUE, xlab = 'Enrichment Score',yaxt = 'n'
	,  names.arg=NA, main="", width = 1, border = NA , col =cl[1])
abline(v=0,lty=1)
text(x=0.1, y=bp[1:length(ups)], labels= names(ups)[length(ups):1],col="black", xpd=TRUE, cex=0.9, adj=0)
title("Upregulated pathways")

bp=barplot(rev(-dns), xlim = c(0,6), horiz = TRUE, xlab = 'Enrichment Score',yaxt = 'n'
	,  names.arg=NA, main="", width = 1, border = NA , col =cl[2])
abline(v=0,lty=1)
text(x=0.1, y=bp[1:length(dns)], labels= names(dns)[length(dns):1],col="black", xpd=TRUE, cex=0.9, adj=0)
title("Downregulated pathways")


tiff(file=sprintf("%s/barplot1.tiff",wdir), width=12, height=10, units="cm", res=300)
bp=barplot(rev(-dns), xlim = c(0,6), horiz = TRUE, xlab = 'Enrichment Score',yaxt = 'n'
	,  names.arg=NA, main="", width = 1, border = NA , col =cl[2])
abline(v=0,lty=1)
text(x=0.1, y=bp[1:length(dns)], labels= names(dns)[length(dns):1],col="black", xpd=TRUE, cex=0.9, adj=0)
title("Downregulated pathways")
dev.off()

