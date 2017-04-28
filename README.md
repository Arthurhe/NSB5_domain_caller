# NSB5_domain_caller V0.1

A domain caller working on HOMER tag directory of HiC: NSB5_homer (the current version relies on HOMER to run matrix construction)

A HiC matrix and domain plotter: NSB5_heatmap

## Main Features

Efficient domain calling. Better than Directionality index-HMM, Juicebox and Armartus.
![My image](https://raw.githubusercontent.com/Arthurhe/NSB5_domain_caller/images/CTCF on domain boundaries.png)

## Getting Started

Copy paste the folder lol, for now.

### Prerequisites

The program only works in linux bash.

1.R 3.2+

2.HOMER

3.bedtools 2

4.six R packages: 
 "optparse", "data.table", "matrixStats", "GenomicRanges", "fitdistrplus", "gplots"

### Installing

copy paste all the script to a folder. 

make sure you have R, bedtools 2 and HOMER installed.

Or you can run NSB5_installer to install the 6 R packages for you. The installer script will modify or creat ~/.Rprofile & ~/.Renviron to download R package from R's US server and let R recognize your/path/to/NSB5/Rlib as a library repo, and install the 6 packages for you (if your R doesn't have them). The script will also add NSB5 executable files to your PATH.

```
your/path/to/NSB5/NSB5_installer
```
### How to use the main program: NSB_homer
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

### How to use the main program: NSB_homer
Example usage:
```
NSB5_heatmap -m /home/ahe/Analysis/201704_DN2DN3/data/HiC/dn3.mm10/Matrix_50000/dn3.mm10_chr10_matrix_KR_50000.txt -d /home/ahe/Analysis/201704_DN2DN3/data/HiC/dn3.mm10/domain_50000/dn3.mm10_refined_domains.bed -r 50000 -o chr10_visualization.pdf -c chr10 -s 20000000 -e 90000000
```

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

	-e STO, --sto=STO
		end of genomic position [default: 1000000]

	--contrast=NUMERIC
		contrast of the image [default:4]

	--outformat=OUTFORMAT
		output format, option available:pdf,png [default: pdf]

	-h, --help
		Show this help message and exit
```
## Authors

* **Zhaoren (Arthur) He** - *Initial work* - [Arthurhe](https://github.com/Arthurhe)
* **Brian Sutjiadi** - *comparison to other domain calling algorithm* - [bsutjiadi](https://github.com/bsutjiadi)

## License

This project is licensed under the xx- see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Thanks Kees Murre for supporting the development of the script
