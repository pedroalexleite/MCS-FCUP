#INTRODUCTION--------------------------------------------------------------------------------------#


#Pedro Leite - 201906697 - Bioinformática - M:CC
#Pedro Carvalho - 201906291 - Bioinformática - M:CC


#Python3 build_phylotree.py P68871
#pip install -r requirements.txt
import sys
import os
import Bio
import requests
import uniprot #https://github.com/boscoh/uniprot/tree/master
from unipressed import UniprotkbClient, UniparcClient #https://github.com/multimeric/Unipressed
from Bio.Blast import NCBIWWW, NCBIXML
from Bio import SeqIO
from Bio import AlignIO, Phylo
from Bio.Phylo.TreeConstruction import DistanceCalculator, DistanceTreeConstructor
from matplotlib.figure import Figure
from matplotlib.backends.backend_agg import FigureCanvasAgg


id = sys.argv[1]


#DATA COLLECTION-----------------------------------------------------------------------------------#


def data_collection(id):

    #is the input a uniprot id?
    if(uniprot.is_uniprot(id) == False):
        print("Invalid Uniprot ID.")
        return

    #get data and sequence
    data = UniprotkbClient.fetch_one(id)
    sequence = data['sequence']['value']

    #write the fasta file
    with open("sequence.fasta", 'w') as file:
            file.write(f'>{id}\n')
            file.write(sequence)
    print("'sequence.fasta' created successfully!")

    return sequence


data_collection(id)


#BLAST ANALYSIS------------------------------------------------------------------------------------#


def blast_analysis(file):

    #read file
    with open(file, 'r') as f:
        sequence = f.readlines()
    sequence = [line.strip() for line in sequence if line.strip()]

    #perform blast agains the NR protein database
    result_handle = NCBIWWW.qblast("blastp", "nr", sequence)

    #sequences of the 10 species with the highest matching  score
    top_hits = 10
    blast_records = NCBIXML.parse(result_handle)
    top_species_sequences = []
    for blast_record in blast_records:
        for alignment in blast_record.alignments:
            species = alignment.hit_def.split("[")[1].split("]")[0].strip()
            if species != "Homo sapiens":
                for hsp in alignment.hsps:
                    top_species_sequences.append((species, hsp.expect, alignment.hit_id, hsp.query))

    #sort top hits
    top_species_sequences.sort(key=lambda x: x[1])
    top_species_sequences = top_species_sequences[:top_hits]

    #write the fasta file
    with open("sequences_to_analyse.fasta", 'w') as file:
        for _, _, identifier, sequence in top_species_sequences:
            file.write(f">{identifier}\n{sequence}\n")
    print("'sequences_to_analyse.fasta' created successfully!")

    return top_species_sequences


blast_analysis("sequence.fasta")


#MULTIPLE SEQUENCE ALIGNMENT-----------------------------------------------------------------------#


def msa(fasta_file):

    #read sequences from the FASTA file
    with open(fasta_file, 'r') as f:
        sequences = f.readlines()
    sequences = [line.strip() for line in sequences if line.strip()]

    #prepare data for the request
    data = {
        "email": "pedroalexleite2001@gmail.com",
        "sequence": "\n".join(sequences),
        "outfmt": "clustal_num",
        "stype": "protein"
    }
    url = "https://www.ebi.ac.uk/Tools/services/rest/clustalo/run"
    response = requests.post(url, data=data)

    #check if the request was successful
    if response.ok:
        #polling loop to check job status and retrieve results when job is finished
        job_id = response.text.strip()
        print("Job submitted successfully. Job ID:", job_id)
        while True:
            status_url = f"https://www.ebi.ac.uk/Tools/services/rest/clustalo/status/{job_id}"
            status_response = requests.get(status_url)
            if status_response.text.strip() in ["RUNNING", "PENDING"]:
                print("Job is still running...")
            elif status_response.text.strip() == "FINISHED":
                print("Job finished. Retrieving results...")
                result_url = f"https://www.ebi.ac.uk/Tools/services/rest/clustalo/result/{job_id}/aln-clustal_num"
                result_response = requests.get(result_url)

                #write alignment result to file
                with open("alignment.txt", "w") as f:
                    f.write(result_response.text)
                break
            else:
                print("Error:", status_response.text.strip())
    else:
        print("Error submitting the job:", response.text)


msa("sequences_to_analyse.fasta")



#PHYLOGENETIC TREE---------------------------------------------------------------------------------#


def phylo_tree(file):

    #read the alignment
    alignment = AlignIO.read(file, "clustal")

    #calculate the hierarchical clustering
    calculator = DistanceCalculator("identity")
    dm = calculator.get_distance(alignment)
    constructor = DistanceTreeConstructor()
    tree = constructor.upgma(dm)

    #save the figure as an image using the canvas
    fig = Figure(figsize=(6, 4))
    canvas = FigureCanvasAgg(fig)
    ax = fig.add_subplot(111)
    Phylo.draw(tree, axes=ax, do_show=False)
    canvas.print_figure('tree.png')


phylo_tree("alignment.txt")


#--------------------------------------------------------------------------------------------------#
