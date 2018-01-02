# calculate Hamming distance between two DNA strands

# function that asks user to input dna strand
# checks for accuracy - only GCTA are input
def obtain_dna(counter):
    repeat = 1

    while repeat == 1:
        print("For DNA strand", counter)
        user_input = list(input("Please enter the sequence(consisting of the nucleotides 'G', 'C', 'A', 'T'): "))
        user_input = list(''.join(user_input).upper())

        for i in user_input:
            if i not in "GCTA":
                print("Please enter the DNA sequence correctly. ")
            else:
                repeat = 0

    return user_input

# obtain two strands from user using obtain_dna function
# check to make sure they are of equal length
while True:
    strand1 = obtain_dna(1)
    strand2 = obtain_dna(2)

    print("\n")

    if len(strand1) == len(strand2):
        break
    else:
        print("Please enter two strands of equal length. \n ")

# check for number of mutations (or hamming difference)
# print out result
counter = 0
number_mutations = 0

while counter < len(strand1):
    if strand1[counter] == strand2[counter]:
        counter += 1
    else:
        number_mutations += 1
        counter += 1

print("Hamming distance (number of mutations) =", number_mutations)
