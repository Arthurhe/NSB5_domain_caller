#!/bin/bash

TAGSDIR=$1
M=$2
R=$3

TAGSDIR=${TAGSDIR%/}
PKGDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKDIR=${TAGSDIR%/}/tmp

mkdir $WORKDIR

echo "============================================";
echo "Begin Pre-processing...";
echo "============================================";
for f in $(ls $TAGSDIR/*.tags.tsv); do
    echo "Pre-Processing $f";
    time python $PKGDIR/homertsv_to_juicebox.py -i $f > ${f%%.tags.tsv}.txt;
    echo "Sorting the preprocessed file...";
    time sort -k3,3d -k7,7d ${f%%.tags.tsv}.txt > ${f%%.tags.tsv}_sorted.txt;
    rm ${f%%.tags.tsv}.txt
    mv ${f%%.tags.tsv}_sorted.txt $WORKDIR
done

echo "============================================";
echo "Begin Juicebox pre...";
echo "============================================";
for f in $(ls $WORKDIR/*_sorted.txt); do
    echo "Juicebox pre of $f";
    time java -Xms512m -Xmx24g -jar /usr/bin/juicebox_tools.7.5.jar pre $f ${f%%_sorted.txt}.hic hg19;
done

echo "============================================";
echo "Begin Juicebox arrowhead...";
echo "============================================";
for f in $(ls $WORKDIR/*.hic); do
    echo "Juicebox arrowhead of $f";
    num=${f##*chr};
    num=${num%%.hic};
    time java -Xms512m -Xmx24g -jar /usr/bin/juicebox_tools.7.5.jar arrowhead -c $num -m $M -r $R -k KR $f $WORKDIR/chr${num}_m${M}_r${R}_boundaries;
done

echo "============================================";
echo "Begin converting juicebox domains to .bed format";
echo "============================================";
for d in $WORKDIR/*/; do
    echo $d;
    pushd $d;
    python $PKGDIR/jb_to_bed.py $(ls) >> $TAGSDIR/juicebox_domains_res_m${M}_r${R}.bed
    popd;
done

echo "============================================";
echo "Cleaning up temporary files...";
echo "============================================";
rm -rf $WORKDIR
