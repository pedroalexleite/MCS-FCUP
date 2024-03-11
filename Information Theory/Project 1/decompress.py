#Python3 decompress_2.py ficheiro.lz ficheiro2.txt
#Python3 decompress_2.py ficheiro.lz teste02-allones.128.bin
import sys
from bitstring import Bits, BitArray
import math

def binaryToDecimal(n):
    decimal = 0
    i = 0
    while(n != 0):
        dec = n % 10
        decimal = decimal + dec * pow(2, i)
        n = n//10
        i += 1
    return decimal

input = Bits(filename=sys.argv[1])
input_string = input.bin

#padding = int(input_string[-8:],2)
#input_string = input_string[0:len(input_string)-8]
#if padding != 0:
    #remove = 8 - padding
    #input_string = input_string[0:len(input_string)-remove]

last_bit = input_string[-1]
input_string = input_string[0:len(input_string)-1]

new_dictionary = []
new_dictionary.append('')
new_dictionary.append(input_string[0])
new_dictionary_size = 1

current_string = ''
output_string = ''
output_string += input_string[0]
for current_char in input_string[1:]:
    sub_string = current_string + current_char
    limit = abs(math.ceil(math.log(new_dictionary_size+1,2)))+1
    if (len(sub_string) != limit):
        current_string = sub_string
    else:
        y = sub_string[:-1]
        b = sub_string[len(sub_string)-1]
        y_dec = binaryToDecimal(int(y))
        if (y_dec == 0):
            total = str(b)
            output_string += total
            new_dictionary_size += 1
            new_dictionary.append(total)
            current_string = ''
            total = ''
        else:
            if (y_dec < len(new_dictionary)):
                y_dic = new_dictionary[y_dec]
                total = str(y_dic) + str(b)
                output_string += total
                new_dictionary_size += 1
                new_dictionary.append(total)
                current_string = ''
                total = ''

if(last_bit == '0'):
    output_string += sub_string
else:
    limit = abs(math.ceil(math.log(new_dictionary_size+1,2)))+1
    if (len(sub_string) != limit):
        current_string = sub_string
    else:
        y = sub_string[:-1]
        b = sub_string[len(sub_string)-1]
        y_dec = binaryToDecimal(int(y))
        if (y_dec == 0):
            total = str(b)
            output_string += total
        else:
            if(y_dec < len(new_dictionary)):
                y_dic = new_dictionary[y_dec]
                total = str(y_dic) + str(b)
                output_string += total

file_output = open(sys.argv[2], 'wb')
#file_output.write(Bits('0b' + output_string).tobytes())
#file_output.close()

x = '0b' + str(output_string)
BitArray(x).tofile(file_output)
file_output.close()
#file_output = open(sys.argv[2], 'a')
#file_output.write('\0')
#file_output.close()
