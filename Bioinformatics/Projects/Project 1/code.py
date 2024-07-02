#INTRODUCTION------------------------------------------------------------------------------#


#Python3 project1.py sequence_chr1.fasta genes_chr1.gtf
import sys
import re


#Read File "sequence_chr1.fasta"
def read_file1(filename):
    with open(filename, "r") as fh:
        #Read the file
        next(fh)
        lines = fh.readlines()
        seq = ""
        for l in lines:
            seq += l.replace("\n","")

        #We only want the first 30k bps
        seq = seq[:30000]

        #Negative strand
        neg_seq = ''
        for i in seq:
            if i == 'C':
                neg_seq += 'G'
            elif i == 'A':
                neg_seq += 'T'
            elif i == 'T':
                neg_seq += 'A'
            elif i == 'G':
                neg_seq += 'C'

        neg_seq = neg_seq[::-1]

    return seq, neg_seq


#Read File "genes_chr1.gtf"
def read_file2(filename):
    exons = []
    with open(filename, 'r') as f:
        for line in f:
            if line.startswith('chrI'):
                fields = line.strip().split('\t')
                if fields[2] == 'exon':
                    start = int(fields[3])
                    end = int(fields[4])
                    gene_id = fields[-1].split('"')[1]
                    strand = fields[6]
                    exons.append((start, end, gene_id,strand))
    return exons


#A: GET STATISTICS-------------------------------------------------------------------------#


#1: Length of the Sequence
def length(seq):
    length_seq = len(seq)

    return length_seq


#2: Frequency of Each Letter
def frequency(seq):
    a = 0
    c = 0
    g = 0
    t = 0
    for i in seq:
        if i == 'A':
            a = a+1
        if i == 'C':
            c = c+1
        if i == 'G':
            g = g+1
        if i == 'T':
            t = t+1
    a = round(((a/len(seq))*100), 2)
    c = round(((c/len(seq))*100), 2)
    g = round(((g/len(seq))*100), 2)
    t = round(((t/len(seq))*100), 2)

    return a, c, g, t


#3: GC Content
def gc_content(seq):
    a, c, g, t = frequency(seq)
    gc = round(g+c, 2)

    return gc


#4: Number of Start (AUG) Codons Found
def number_start_codons(seq):
    start_codons = 0
    for i in range(len(seq)-2):
        if seq[i:i+3] == "ATG":
             start_codons += 1

    return start_codons


#5: Number of Stop (UAA, UAG, UGA) Codons Found
def number_stop_codons(seq):
    stop_codons = 0
    for i in range(len(seq)-2):
        if seq[i:i+3] == "TAA" or seq[i:i+3] == "TAG" or seq[i:i+3] == "TGA":
             stop_codons += 1

    return stop_codons


#6: Most and Least Frequent Codons
def most_least_codons(seq):
    codon_dict = {}
    for i in range(0, len(seq)-2, 3):
        if seq[i:i+3] in codon_dict:
            codon_dict[seq[i:i+3]] += 1
        else:
            codon_dict[seq[i:i+3]] = 1

    most_codon = max(codon_dict, key=codon_dict.get)
    least_codon = min(codon_dict, key=codon_dict.get)

    return most_codon, least_codon


#B: GET ORFS-------------------------------------------------------------------------------#

#Get ORFs
def get_orfs(seq):
    orfs = []
    atg_flag=0
    counter=0

    for i in range(0,len(seq)-2,3):
        if seq[i:i+3] == "ATG" and atg_flag==0:
            start=i
            atg_flag=1
        if atg_flag==1 and (seq[i:i+3] == "TAA" or seq[i:i+3] == "TAG" or seq[i:i+3] == "TGA"):
            end = i+3
            counter += 3
            atg_flag=0
            if(counter>=150):
                orfs.append((seq[start:end],start,end))
                counter=0
            else:
                counter=0
        if atg_flag==1:
            counter += 3

    for i in range(1,len(seq)-2,3):
        if seq[i:i+3] == "ATG" and atg_flag==0:
            start=i
            atg_flag=1
        if atg_flag==1 and (seq[i:i+3] == "TAA" or seq[i:i+3] == "TAG" or seq[i:i+3] == "TGA"):
            end = i+3
            counter += 3
            atg_flag=0
            if(counter>=150):
                orfs.append((seq[start:end],start,end))
                counter=0
            else:
                counter=0
        if atg_flag==1:
            counter += 3

    for i in range(2,len(seq)-2,3):
        if seq[i:i+3] == "ATG" and atg_flag==0:
            start=i
            atg_flag=1
        if atg_flag==1 and (seq[i:i+3] == "TAA" or seq[i:i+3] == "TAG" or seq[i:i+3] == "TGA"):
            end = i+3
            counter += 3
            atg_flag=0
            if(counter>=150):
                orfs.append((seq[start:end],start,end))
                counter=0
            else:
                counter=0
        if atg_flag==1:
            counter += 3

    return orfs


#seq = "ATGGCAGCGACACCAGCGGCGATTGAAGTTAATTTGACCATTGTATTTGTTTTGTTTGTTAGTGCTGATATAAGCTTAACAGGAAAGGAAAGAATAAAGACATATTCTCAAAGGCATATAGTTGAAGCAGCTCTATTTATACCCATTCCCTCATGGGTTGTTGCTATTTAA"
#print(get_orfs2(seq))


#7: File With All Protein Sequences
def proteins(orfs):
    #Translate the ORFs to proteins
    genetic_code = {
        "TTT": "F", "TTC": "F", "TTA": "L", "TTG": "L",
        "TCT": "S", "TCC": "S", "TCA": "S", "TCG": "S",
        "TAT": "Y", "TAC": "Y", "TAA": "*", "TAG": "*",
        "TGT": "C", "TGC": "C", "TGA": "*", "TGG": "W",
        "CTT": "L", "CTC": "L", "CTA": "L", "CTG": "L",
        "CCT": "P", "CCC": "P", "CCA": "P", "CCG": "P",
        "CAT": "H", "CAC": "H", "CAA": "Q", "CAG": "Q",
        "CGT": "R", "CGC": "R", "CGA": "R", "CGG": "R",
        "ATT": "I", "ATC": "I", "ATA": "I", "ATG": "M",
        "ACT": "T", "ACC": "T", "ACA": "T", "ACG": "T",
        "AAT": "N", "AAC": "N", "AAA": "K", "AAG": "K",
        "AGT": "S", "AGC": "S", "AGA": "R", "AGG": "R",
        "GTT": "V", "GTC": "V", "GTA": "V", "GTG": "V",
        "GCT": "A", "GCC": "A", "GCA": "A", "GCG": "A",
        "GAT": "D", "GAC": "D", "GAA": "E", "GAG": "E",
        "GGT": "G", "GGC": "G", "GGA": "G", "GGG": "G"
    }

    proteins = []
    for orf in orfs:
        protein_sequence = ''
        for i in range(0, len(orf), 3):
            if len(orf[i:i+3]) == 3:
                amino_acid = genetic_code.get(orf[i:i+3], '')
                if amino_acid == '*':
                    break
                protein_sequence += amino_acid
        proteins.append(protein_sequence)

    #Create file with proteins
    with open('all_potential_proteins.txt', 'w') as f:
        for i in range(len(proteins)):
            f.write(proteins[i] + '\n')

    return proteins


#8: File With All Genomic Coordinates
def orf_coords(orfs, indexs):
    with open('orf_coordinates.txt', 'w') as f:
        for i in range(len(orfs)):
            start, end = indexs[i]
            string = "ORF"+str(i+1)
            f.write(str(start)+', '+str(end)+', '+string+'\n')


#C: OVERLAP WITH ANNOTATION----------------------------------------------------------------#


#Calculate Overlap
def overlap(seq, neg_seq, exons):
    orfs = get_orfs(seq)
    neg_orfs = get_orfs(neg_seq)
    overlap = 0
    exon_values = []
    exon_names = []
    for exon in exons:
        sequence_orfs = seq[exon[0]:exon[1]]
        denom = len(sequence_orfs)
        max_overlap=0
        final_value=0
        if exon[3] == "+":
            for orf in orfs:
                overlap = min(exon[1], orf[2]) - max(exon[0], orf[1])
                if(overlap > 0): max_overlap = max(max_overlap,overlap)
                overlap=0
        elif exon[3] == "-":
            for orf in neg_orfs:
                overlap = min(exon[1], orf[2]) - max(exon[0], orf[1])
                if(overlap > 0): max_overlap = max(max_overlap,overlap)
                overlap=0

        denom = len(sequence_orfs)
        if denom != 0:
            final_value = round((max_overlap / denom) * 100)
        else:
            final_value = 0

        exon_values.append(final_value)
        exon_names.append(exon[2])

    return exon_values, exon_names


#CONCLUSION--------------------------------------------------------------------------------#


seq, neg_seq = read_file1(sys.argv[1])
#print("(+) Sequence:", seq)
#print("(-) Sequence:", neg_seq)


exons = read_file2(sys.argv[2])
#print("Exons:", exons)


pos_length_seq = length(seq)
neg_length_seq = length(neg_seq)
print("(+) Length of the Sequence:", pos_length_seq)
print("(-) Length of the Sequence:", neg_length_seq)


pos_freq_seq = frequency(seq)
neg_freq_seq = frequency(neg_seq)
print("(+) Frequency (%) of Each Letter (A, C, G, T, respectively):", pos_freq_seq)
print("(-) Frequency (%) of Each Letter (A, C, G, T, respectively):", neg_freq_seq)


pos_gcc = gc_content(seq)
neg_gcc = gc_content(neg_seq)
print("(+) GC Content (%):", pos_gcc)
print("(-) GC Content (%):", neg_gcc)


pos_start_codons = number_start_codons(seq)
neg_start_codons = number_start_codons(neg_seq)
print("(+) Number of Start (ATG) Codons Found:", pos_start_codons)
print("(-) Number of Start (ATG) Codons Found:", neg_start_codons)


pos_stop_codons = number_stop_codons(seq)
neg_stop_codons = number_stop_codons(neg_seq)
print("(+) Number of Stop (TAA, TAG, TGA) Codons Found:", pos_stop_codons)
print("(-) Number of Stop (TAA, TAG, TGA) Codons Found:", neg_stop_codons)


pos_most_codon, pos_least_codon = most_least_codons(seq)
neg_most_codon, neg_least_codon = most_least_codons(neg_seq)
print("(+) Most and Least Frequent Codons, respectively:", pos_most_codon, pos_least_codon)
print("(-) Most and Least Frequent Codons, respectively:", neg_most_codon, neg_least_codon)


pos_orfs = get_orfs(seq)
neg_orfs = get_orfs(neg_seq)
orfs = pos_orfs+neg_orfs
#print("ORFs:", orfs)


new_orfs = []
for orf in orfs:
    new_orfs.append(orf[0])
new_indexs = []
for orf in orfs:
    new_indexs.append([orf[1], orf[2]])
protein_sequence = proteins(new_orfs)
#print("Proteins:", protein_sequence)


orf_coords(new_orfs, new_indexs)


print("Overlaps:")
exon_values, exon_names = overlap(seq, neg_seq, exons)
for i in range(len(exon_names)):
    print(exon_names[i]+' '+str(exon_values[i])+'%')


#------------------------------------------------------------------------------------------#
