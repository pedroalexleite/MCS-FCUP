import math
import random

def generate_code(k, eps):
    p = []
    power = 1
    while((k*(eps**power))>=1):
        for j in range(int(9*(eps**power)*k)):
            p.append(random.sample(range(0, k), int(1/(eps**power))))
        power += 1

    return p

def encode(k, p, w):
    w_int = []
    for i in range(len(w)):
        w_int.append(int(w[i]))

    parity = []
    for i in range(len(p)):
        parity.append(0)

    for i in range(len(p)):
        for j in p[i]:
            parity[i] = parity[i] ^ w_int[j]

    string = w_int + parity
    encoded_string = ''
    for i in range(len(string)):
        encoded_string += str(string[i])

    return encoded_string

def decode(k, p, y):
    y_int = []
    for i in range(len(y)):
        if((y[i] == '0') or (y[i] == '1')):
            y_int.append(int(y[i]))
        if(y[i] == '?'):
            y_int.append(-1)

    counter_list = []
    old_parity = []
    parity = []
    for i in range(len(p)):
        counter_list.append(0)
        old_parity.append(y_int[len(y_int)-len(p)+i])
        parity.append(0)

    for i in range (len(p)):
        if old_parity[i] != -1:
            counter = 0
            for j in p[i]:
                if y_int[j] == -1:
                    counter = counter + 1
            counter_list[i] = counter

    while 1 in counter_list:
        for i in range(len(p)):
            if counter_list[i] == 1:
                index = 0
                flag = 0
                for j in p[i]:
                    if y_int[j] == -1:
                        flag = 1
                        flag_in = j
                    else:
                        parity[i] = parity[i] ^ y_int[j]
                if flag == 1:
                    if((1 ^ parity[i]) == old_parity[i]):
                        y_int[flag_in] = 1
                    else:
                        y_int[flag_in] = 0

        for i in range (len(p)):
            if old_parity[i] != -1:
                counter = 0
                for j in p[i]:
                    if y_int[j] == -1:
                        counter = counter + 1
                counter_list[i] = counter


    decoded_string = ''
    flag = 0
    for i in range(len(y_int)-len(p)):
        if y_int[i] == -1:
            flag = 1
        decoded_string += str(y_int[i])

    if flag == 1:
        return "None"
    else:
        return decoded_string

def transmit(x, eps):
    exemple = ''
    for char in x:
        if (random.random() < eps):
            exemple += '?'
        else:
            exemple += char

    return exemple

def test(k, eps):
    p = generate_code(k, eps)

    w = ''
    for i in range(k):
        w += str(random.randint(0, 1))

    counter = 0
    for i in range(100):
        if (decode(k,p,transmit(encode(k,p,w),eps)) == w):
            counter += 1

    return counter

k = 4096
eps = 0.1
result = test(k, eps)

print("With k = "+str(k)+" and eps = "+str(eps)+", the efficiency is "+str(result)+"%")
