#!/usr/bin/env python

import argparse
import random
import string
import Queue


def main():
    # parse input arguments
    parser = argparse.ArgumentParser()
    required = parser.add_argument_group("required argument")
    required.add_argument("-i", "--infile", required=True, help="The homer .tsv file to convert")
    args = parser.parse_args()

    tsvfile = open(args.infile, 'r')
    #outfile = open("converted.txt", 'w')

    illegal_chrs = ['chrX', 'chrY', 'chrM', 'chrMT']

    samechr = Queue.Queue()

    for line in tsvfile:
        #randstr = ''.join(random.choice(string.lowercase) for x in range(15))
        randstr = 'a'
        sline = line.split("\t")
        # Handle different chromosomes
        if sline[1] not in illegal_chrs and sline[6] not in illegal_chrs and int(sline[1].split("chr")[1]) <= int(sline[6].split("chr")[1]):
            if int(sline[2]) < int(sline[7]):
                nline = "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}".format(randstr,
                        sline[3], sline[1].split("chr")[1], sline[2], "0", sline[8], sline[6].split("chr")[1],
                        sline[7], "1", "11", "11")
                print(nline)
                #outfile.write("{}\n".format(nline))

        # Handle same chromosome
        #if sline[1] not in illegal_chrs and sline[6] not in illegal_chrs and int(sline[1].split("chr")[1]) == int(sline[6].split("chr")[1]):
        #    nline = "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}".format(randstr,
        #            sline[3], sline[1], sline[2], "10", sline[8], sline[6],
        #            sline[7], "10", "11", "11")
        #    samechr.put(nline)

    # Filter out reversed entries (start pos must be less than end pos)
    #while not samechr.empty():
    #    nline = samechr.get()
    #    if int(nline.split("\t")[3]) < int(nline.split("\t")[7]):
    #        outfile.write("{}\n".format(nline))


if __name__ == "__main__":
    main()
