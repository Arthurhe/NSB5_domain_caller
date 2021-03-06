<p align="center">
  <img src ="https://Arthurhe.github.com/NSB5_domain_caller/images/NSB5logo.png" width="400" alt="NSB5 logo" />
</p>

# NSB5 domain caller V0.2

A domain caller working on HOMER tag directory of HiC: NSB5_homer (the current version relies on HOMER to run matrix construction)

A HiC matrix and domain plotter: NSB5_heatmap

## Main Features

**Efficient domain calling.** 

The following analysis were performed on GM12878 and K562, 25Kb resolution (ENCODE data, 2.5 and 2.1 billion reads respectively) 

<img src="https://Arthurhe.github.com/NSB5_domain_caller/images/domain_calling_comparison1.png" width="900" alt="domain compare" />

**Better CTCF directionality and cohesin enrichment on domain boundary.**

CTCF_both: CTCF ChIP-seq peak that has both "+" & "-" direction motifs

CTCF_plus: CTCF ChIP-seq peak that has only "+" direction motifs

CTCF_minus: CTCF ChIP-seq peak that has only "-" direction motifs

SMC: Cohesin (SMC3) ChIP-seq peak

**GM12**

<img src="https://Arthurhe.github.com/NSB5_domain_caller/images/GM12_domain_calling_SC_QC.png" width="900">

**K562**

<img src="https://Arthurhe.github.com/NSB5_domain_caller/images/K562_domain_calling_SC_QC.png" width="900">

**More comprehensive genome coverage, finer domain resolution and less overlap.**

| GM12878  | Genome coverage | Domain number | Average domain size | Region overlapping percentage |
| ------------- | -------- | ------- | ---------- | ---------- |
| NSB5| 81.7% | 7504 | 346.5 Kb |  2.7% |
| Juicebox | 54.5%  |6741 |370.0 Kb |31.1% |
| Armatus | 42.3% |37123 | 35.3 Kb| 0% |
| DI-HMM | 82.4% |3103 | 822.2 Kb|0% |

| K562  | Genome coverage | Domain number | Average domain size | Region overlapping percentage |
| ------------- | -------- | ------- | ---------- | ---------- |
| NSB5  | 83.7% | 6342 | 425.7 Kb |  4.1% |
| Juicebox | 50.7%  |6242 |362.0 Kb |29.4% |
| Armatus | 38.0% |35597 | 33.0 Kb| 0% |
| DI-HMM | 85.2% |3396 | 776.2 Kb|0% |


## Getting Started

Copy paste the folder to wherever you want.

### Prerequisites

The program only works in linux bash.

1.R v3.3.2

2.HOMER v4.9

3.bedtools v2.25.0

4.7 R packages: 
 "optparse", "data.table", "matrixStats", "GenomicRanges", "fitdistrplus", "gplots", "Matrix"

### Installing

copy paste all the script to a folder. 

make sure you have R, bedtools 2 and HOMER installed.

Or you can run NSB5_installer to install the 6 R packages for you. The installer script will modify or create ~/.Rprofile & ~/.Renviron to download R package from R's US server and let R recognize your/path/to/NSB5/Rlib as a library repo. It will then install any of the 6 packages that's not in your R library repo. The script will also add NSB5 executable files to your PATH.

```
your/path/to/NSB5/NSB5_installer
```
### How to use the main program: NSB5_homer
Example usage: 
```
NSB5_homer -t /home/ahe/Analysis/201704_DN2DN3/data/HiC/dn3.mm10 -s /home/ahe/Analysis/genomeFiles/GenomeSize_mm10_core.txt -r 50000
```

Use -h to see the main program's help page
```
NSB5_homer -h
# package check
# package loading
# package check and loading finished
Usage: /home/ahe/tools/NSB5/NSB5_homer [options]

Options:
	-t CHARACTER, --tagDir=CHARACTER
		tag directory path (without the last slash: "/"). e.g. /home/ahe/Analysis/dn3.mm10 [required]

	-r NUMERIC, --resolution=NUMERIC
		resolution of matrix. [required]

	-s CHARACTER, --genomeSize=CHARACTER
		genome size file. [required]

	-c NUMERIC, --chrNum=NUMERIC
		number of autosomes should we consider. e.g. 19 (for mouse) [default: determine from tag directory]

	-O, --overwrite
		overwrite all existing files of previous NSB5 run [default: not overwrite]

	--overwriteM
		overwrite existing Matrix files of previous NSB5 run [default: not overwrite]

	--overwriteD
		overwrite existing domain candidate files of previous NSB5 run [default: not overwrite]

	--overwriteH
		overwrite existing HiC summary of previous NSB5 run [default: not overwrite]

	-h, --help
		Show this help message and exit
```

### How to use the plotting program: NSB5_heatmap

The program can be used to plot HiC interaction matrix, accepted input format are: full matrix and sparse matrix. Optionally, it can also plot at most 2 domain sets on the output figure.

Example usage of full matrix:
```
NSB5_heatmap -m full.HiC.matrix.file -d 1st.domain.bed --domain2=2nd.domain.bed -r 50000 -c chr10 -s 40000000 -e 70000000 -o output.pdf [other options]
```
Example usage of sparse matrix:
```
NSB5_heatmap -m sparse.HiC.matrix.file -f sparse --gsize=two.columns.genome.size.file -r 50000 -c chr10 -s 40000000 -e 70000000 -o output.pdf [other options]
```

Example image with domain plotted on it:

<img src="https://Arthurhe.github.com/NSB5_domain_caller/images/homer_heatmap_example.png" width="500">

Use -h to see the plotting program's help page
```
heatmap -h
Usage: /home/ahe/tools/NSB5/NSB5_heatmap [options]

Options:
	-m CHARACTER, --mat=CHARACTER
		matrix file. [required]

	-d CHARACTER, --domain1=CHARACTER
		domain file in bed format [required]

	--domain2=CHARACTER
		additional domain file in bed format [optional]

	-r NUMERIC, --res=NUMERIC
		resolution of matrix [required]

	-c CHARACTER, --chr=CHARACTER
		chromosome name [required]

	-o CHARACTER, --outname=CHARACTER
		output pdf name [required]

	--igrow=NUMERIC
		when loading matrix, ignore first k row, e.g. 1 (for HOMER matrix) [default:0]

	--igcol=NUMERIC
		when loading matrix, ignore first k col, e.g. 2 (for HOMER matrix) [default:0]

	-s NUMERIC, --sta=NUMERIC
		start of genomic position [default: 1]

	-e NUMERIC, --sto=NUMERIC
		end of genomic position [default: 1000000]

	--contrast=NUMERIC
		contrast of the image [default:4]

	--outformat=CHARACTER
		output format, option available:pdf,png [default: pdf]

	--outwidth=NUMERIC
		width of output image in inches [default: 13]

	--outheight=NUMERIC
		height of output image in inches [default: 13]

	-h, --help
		Show this help message and exit
```

## Authors

* **Zhaoren (Arthur) He** - *Initial work* - [Arthurhe](https://github.com/Arthurhe)
* **Brian Sutjiadi** - *comparison to other domain calling algorithm* - [bsutjiadi](https://github.com/bsutjiadi)

## License

This project is licensed under the Apache License 2.0- see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Thanks **Kees Murre** for supporting the development of the script
* **MURRE Lab @ UCSD** http://labs.biology.ucsd.edu/murre/index.html 
