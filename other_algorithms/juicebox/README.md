# Juicebox (Aiden lab) Domain Calling

This directory contains scripts that will use the Juicebox Arrowhead algorithm to call domains from HOMER HiC data.

## Usage

Example usage:
```
./juicebox_script <tags_dir> <m> <r>
```

#### Notes
Please ensure that juicebox_script.sh is executable.
The script was tested using an absolute path for the <tags_dir>.

## Requirements

This script requires that the juicebox .jar binary (juicebox_tools.7.5.jar) is installed and accessible from /usr/local/bin
All other preprocessing scripts are included within this package and should not need configuring.
