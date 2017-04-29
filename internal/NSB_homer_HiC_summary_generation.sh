#!/bin/bash
tagdir=$1

#reads filter and formatting
cat ${tagdir}/chr*.tags.tsv > ${tagdir}/all.tags.tsv
awk 'BEGIN{OFS="\t";FS="\t"}$2==$7 && $8-$3>5000 && $8-$3<1000000 {for (i = 1; i <= $5; ++i) print $5,$2,$3,$4,$7,$8,$9}' ${tagdir}/all.tags.tsv > ${tagdir}/HiC_summary.txt
cat ${tagdir}/HiC_summary.txt | awk 'BEGIN{OFS="\t";FS="\t"}{printf "%s\t%s\t%s\t\t\t%s\t%s\n%s\t%s\t%s\t\t\t%s\t%s\n",$2,$3,$3+1,"+",$1,$2,$6,$6+1,"-",$1}' | sort -V -k1.4 -k2 > ${tagdir}/HiC_reads_for_intersection.txt
rm ${tagdir}/all.tags.tsv