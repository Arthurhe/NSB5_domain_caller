#!/bin/bash
tagdir=$1
Gsize=$2

#bed region creation
cat ${tagdir}/prefilteringRegionList_splited.bed | sort -V -k1.4 -k2 | awk 'BEGIN{OFS="\t";FS="\t"}{print $0,"","+"}' > ${tagdir}/temp; mv ${tagdir}/temp ${tagdir}/prefilteringRegionList_splited.bed

#bedtools intersect
bedtools intersect -sorted -wa -c -s -a ${tagdir}/prefilteringRegionList_splited.bed -b ${tagdir}/HiC_reads_for_intersection.txt -g $Gsize | awk 'BEGIN{OFS="\t";FS="\t"}{print $1,$2,$3,$4,$7}' > ${tagdir}/BoundaryRegionCount_plus.bed
bedtools intersect -sorted -wa -c -S -a ${tagdir}/prefilteringRegionList_splited.bed -b ${tagdir}/HiC_reads_for_intersection.txt -g $Gsize | awk 'BEGIN{OFS="\t";FS="\t"}{print $1,$2,$3,$4,$7}' > ${tagdir}/BoundaryRegionCount_minus.bed
