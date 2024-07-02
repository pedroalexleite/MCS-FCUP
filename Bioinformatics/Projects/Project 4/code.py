#INTRODUCTION--------------------------------------------------------------------------------------#


#Pedro Leite - 201906697 - Bioinformática - M:CC
#Task1 (1, 2), Task2 (10), Task3 (11)


#Python3 pwm.py
import math


#TASKS---------------------------------------------------------------------------------------------#


class NumMatrix:

    def __init__(self, rows, cols):
        self.rows = rows
        self.cols = cols
        self.matrix = [[0.0 for _ in range(cols)] for _ in range(rows)]


    def __getitem__(self, idx):
        return self.matrix[idx]


    def __setitem__(self, idx, value):
        self.matrix[idx] = value


    def __len__(self):
        return self.rows


    def __repr__(self):
        repr_str = ""
        for row in self.matrix:
            repr_str += " ".join(f"{x:.2f}" for x in row) + "\n"
        return repr_str


class PWM:

    #TASK 1 - EX1 & EX2
    def __init__(self, lst_of_words):

        self.lst_of_words = lst_of_words

        if len(lst_of_words) == 0:
            self.bio_type = "DNA"
            self.alphabet = "ACGT"
            self.word_length = 6
            self.pwm = NumMatrix(self.word_length, len(self.alphabet))
            return

        all_symbols = set("".join(lst_of_words))
        self.word_length = len(lst_of_words[0])

        if all_symbols.issubset(set("ACGT")):
            self.bio_type = "DNA"
            self.alphabet = "ACGT"
        elif all_symbols.issubset(set("ACGU")):
            self.bio_type = "RNA"
            self.alphabet = "ACGU"
        else:
            self.bio_type = "Protein"
            self.alphabet = "".join(sorted(all_symbols))

        if not all(len(word) == self.word_length for word in lst_of_words):
            raise ValueError("All words must have the same length.")

        freq_matrix = NumMatrix(self.word_length, len(self.alphabet))

        for word in lst_of_words:
            for i, symbol in enumerate(word):
                if symbol not in self.alphabet:
                    raise ValueError(f"Symbol '{symbol}' not in specified alphabet.")
                symbol_index = self.alphabet.index(symbol)
                freq_matrix[i][symbol_index] += 1

        for i in range(self.word_length):
            row_sum = sum(freq_matrix[i])
            for j in range(len(self.alphabet)):
                freq_matrix[i][j] /= row_sum if row_sum != 0 else 1

        transposed_matrix = NumMatrix(len(self.alphabet), self.word_length)
        for row in range(self.word_length):
            for col in range(len(self.alphabet)):
                transposed_matrix[col][row] = freq_matrix[row][col]

        self.pwm = transposed_matrix


    def display_pwm(self):
        print("Word Length:", self.word_length, "\n")
        print("Bio Type:", self.bio_type, "\n")
        print("Alphabet:", self.alphabet, "\n")
        print("Frequencies:")
        print(self.pwm)


    #TASK 2 - EX10
    def log_odds(self, background):
        pwm = self.pwm
        new_pwm = NumMatrix(len(pwm), len(pwm[0]))

        if len(self.lst_of_words) != 0:
            for i, word in enumerate(self.lst_of_words):
                if i >= len(pwm[0]):
                    continue
                for j, letter in enumerate(word):
                    if j >= len(pwm):
                        continue
                    if letter in background:
                        if pwm[j][i] == 0:
                            value = float('-inf')
                        else:
                            value = math.log(pwm[j][i]/background[letter], 2)
                    else:
                        if pwm[j][i] == 0:
                            value = float('-inf')
                        else:
                            value = math.log(pwm[j][i]/0.25, 2)
                    new_pwm[j][i] = round(value, 2)
        else:
            for i in range(len(pwm[0])):
                for j in range(len(pwm)):
                    new_pwm[j][i] = float('-inf')

        print("Log Odds:")
        for row in new_pwm:
            formatted_row = " ".join(f"{value:.2f}" for value in row)
            print(formatted_row)

        return new_pwm


#TASK 3 - EX11
def test(lst_of_words, background_custom):

    print("\n#--------------DEFAULT PWM-------------#\n")
    pwm_default = PWM([])
    pwm_default.display_pwm()
    background_default = {}
    pwm_default.log_odds(background_default)
    print()

    print("#--------------CUSTOM PWM--------------#\n")
    pwm_custom = PWM(lst_of_words)
    pwm_custom.display_pwm()
    pwm_custom.log_odds(background_custom)
    print()


lst_of_words = ["TATAAAA", "TATAAAT", "TATATAT", "TATAAGG", "TATAATG", "CATAAAA", "CCTATAA", "TATAATC"]
background_custom = {'A':0.3, 'C':0.2, 'G':0.2, 'T':0.3}
test(lst_of_words, background_custom)


#---------------------------------------------------------------------------------------------------#
