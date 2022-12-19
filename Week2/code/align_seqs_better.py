#!/usr/bin/env python3

"""
Script to align two sequences after taking an input from user.
It saves all the best alignments in binary format.
If no input is provided the script will run with default arguements.
All individual functions can work independently!
Enjoy!
Author: Aditi Madkaikar arm122@ic.ac.uk
Script: align_seqs_fasta.py
Descripton: Finding sequence alignment score for two fasta files
Arguements: Ideally two input fasta files. Can work without any.
Date: Dec 2022
"""

__author__ = 'Aditi Madkaiakr (arm122@ic.ac.uk)'
__version__ = '0.0.1'
__license__ = 'None'



import sys
import csv
import re
import pickle 

# Main entry point of the script. 
def entry_point(argv='abc'):
    """
    The main entry function of the script. Takes arguements from the system or user. 
    When called independently, it will work only with names of two fasta files are arguements. 
    Else it will continue with defaults
    """
    if len(argv) != 3:
        print("Too few or too many arguements...")
        print("Continuing with defaults..")
        seq1, seq2 = fasta_parser().values()
    else:
        try:
            arg1 = argv[1]
            arg2 = argv[2]
            re.search(r'\w+\.fasta\s\w+\.fasta', arg1 + " " + arg2).group()
            print("Good arguements given. Will continue with these.")
            seq1, seq2 = fasta_parser().values()
        except:
            print("Going back to defaults since wrong input was given")
            seq1, seq2 = fasta_parser().values()
    req = seq_order(seq1, seq2)
    
    return 0


#####################
# code to read FASTA file
#####################
def fasta_parser(args= ['../data/407228326.fasta', '../data/407228412.fasta']):
    """ 
    This is a Fasta parser. Parses Fasta files and returns a dictionary of parsed files.
    This function can take any number of inputs. If none are given it will proceed with defaults.
    """
    hre=re.compile('>(\S+)')
    lre=re.compile('^(\S+)$')
    seqs_dict = {}
    
    for i in range(len(args)):
        f=open(args[i],'r')
        lines=f.readlines()
        
        gene = {}
        for line in lines:
                outh = hre.search(line)
                if outh:
                        id=outh.group(1)
                else:
                        outl=lre.search(line)
                        if(id in gene.keys()):
                                gene[id] += outl.group(1)
                        else:
                                gene[id]  =outl.group(1)
        seqs_dict["Seq%s" % (i+1)] = gene[id]
        f.close()
    return seqs_dict

# Function to calcualte score at every point
def calculate_score(s1='ATCGA', s2='ATCG', l1=5, l2=4, startpoint=0):
    """
    This function calculates the best source when s1, s2, l1, l2 and the startpoint are provided. 
    It aligns the shorter sequence at the startpoint location of the longer sequence and calculates the number of matching base pairs
    If you want to import this function as a module, assign the longer sequence s1, and the shorter to s2. 
    l1 is length of the longest, l2 that of the shortest.
    It has default arguements.
    """
    
    matched = "" # to hold string displaying alignements
    score = 0
    #import ipdb; ipdb.set_trace()
    for i in range(l2):
        if (i + startpoint) < l1:
            if s1[i + startpoint] == s2[i]: # if the bases match
                matched = matched + "*"
                score = score + 1
            else:
                matched = matched + "-"
    return score

# Function to order the given sequences by length
def seq_order(seq1='ATCG', seq2='ATCGA'):
    """
    Orders sequences in terms based on their lengths. 
    It takes two arguements and returns the ordered sequences and their lengths. 
    """
    l1 = len(seq1)
    l2 = len(seq2)
    if l1 >= l2:
        s1 = seq1
        s2 = seq2
    else:
        s1 = seq2
        s2 = seq1
        l1, l2 = l2, l1 # swap the two lengths
    
    if __name__ == '__main__':
        my_best_align = None
        my_best_score = -1
        dict_to_save = dict()

        for i in range(l1): # Note that you just take the last alignment with the highest score
            z = calculate_score(s1, s2, l1, l2, i)
            if z >= my_best_score:
                my_best_align = "." * i + s1 # think about what this is doing!
                my_best_score = z
                num = i  
                list_to_save = [my_best_align, s1, 'Best score: ', my_best_score]
            dict_to_save[num] = list_to_save

        f = open('../results/better_aligned_seqs.p', 'wb')
        pickle.dump(dict_to_save, f)
        f.close()

    else:
        choice = input("Do you want to continue to calculating alignment score? [y/n]")
        print("Thie choice you made is", choice)
        
        if choice.lower() == "y":
            my_best_align = None
            my_best_score = -1
            dict_to_save = dict()
   
            for i in range(l1): # Note that you just take the last alignment with the highest score
                z = calculate_score(s1, s2, l1, l2, i)
                if z > my_best_score:
                    my_best_align = "." * i + s1 # think about what this is doing!
                    my_best_score = z 
                    num = i
                    list_to_save = [my_best_align, s1, 'Best score: ', my_best_score]
                dict_to_save[num] = list_to_save
            return dict_to_save
        else:
            return "The ordered sequences and their lengths are:", s1, s2, l1, l2
    return 0

# Main thingy
if __name__ == '__main__':
    """Makes sure the main function is called from the command line"""
    status = entry_point(sys.argv)
    sys.exit(status)
