#!/bin/bash

OUTPUT_DIR=../../../temp
GENO_BASENAME="genotype_data"

#Calculate genetic relatedness matrix
./gcta64 --bfile ${OUTPUT_DIR}/${GENO_BASENAME} --autosome --make-grm-bin --thread-num 4 --out ${OUTPUT_DIR}/${GENO_BASENAME}
