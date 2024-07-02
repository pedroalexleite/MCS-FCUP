#Python3 compress_2.py ficheiro.txt ficheiro.lz
#Python3 compress_2.py teste01-allones.128.bin ficheiro.lz
import sys
from bitstring import Bits, BitArray
import math

def decimalToBinary(n):
    if n==0: return ''
    else:
        return decimalToBinary(n//2) + str(n%2)

def decimalToBinary2(x):
    return str(bin(x).replace('0b', ''))

input = Bits(filename=sys.argv[1])
input_string = input.bin

dictionary = {}
dictionary_size = 1
dictionary[input_string[0]] = dictionary_size

output_string = ''
output_string += input_string[0]
current_string = ''
for current_char in input_string[1:]:
    sub_string = current_string + current_char
    if sub_string in dictionary:
        current_string = sub_string
    else:
        limit = abs(math.ceil(math.log(dictionary_size+1,2)))
        if(len(sub_string)>=2):
            y = sub_string[:-1]
            b = sub_string[len(sub_string)-1]
            pos = ''
            pos_bin = ''
            total = ''
            if y in dictionary:
                pos = dictionary[y]
                pos_bin = decimalToBinary(pos)
            limit = limit-len(pos_bin)
            aux_string = ''
            for j in range(limit):
                aux_string += '0'
            pos_bin = aux_string + pos_bin
            total = str(pos_bin) + str(b)
        else:
            aux_string = ''
            for j in range(limit):
                aux_string += '0'
            total = aux_string + str(sub_string)
        output_string += total
        dictionary_size += 1
        dictionary[sub_string] = dictionary_size
        current_string = ''

if(current_string != ''):
    output_string += current_string
    output_string += '0'
else:
    output_string += '1'

file_output = open(sys.argv[2], "wb")
#file_output.write(Bits('0b' + output_string).tobytes())
#file_output.close()
#file_output = open(sys.argv[2], "ab")
#file_output.write(Bits('0b' + decimalToBinary2((len(Bits('0b' + output_string)) % 8)).zfill(8)).tobytes())
#file_output.close()

x = '0b' + str(output_string)
BitArray(x).tofile(file_output)
file_output.close()
