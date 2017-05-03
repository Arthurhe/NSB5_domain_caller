#!/usr/bin/env python3

import argparse


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("infile", help="juicebox domain file filepath")
    args = parser.parse_args()

    juiceboxDomFile = open(args.infile, 'r')

    counter = 0
    for line in juiceboxDomFile:
        if counter == 0:
            # print("chr\tx\ty\tsize")
            pass
        else:
            line = line.rstrip()
            line = line.split("\t")
            size = int(line[2]) - int(line[1])
            print("chr{}\t{}\t{}\t{}".format(line[0], line[1], line[2], size))
        counter += 1
    pass


if __name__ == "__main__":
    main()
