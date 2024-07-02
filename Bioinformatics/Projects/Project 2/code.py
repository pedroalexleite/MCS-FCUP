def score_pos(c1, c2, sm, g):
    if c1 == "-" or c2=="-":
        return g
    else:
        return sm[c1+c2]


def needleman_Wunsch (seq1, seq2, sm, g):
    S = [[0]]
    T = [[0]]

    for j in range(1, len(seq2)+1):
        S[0].append(g * j)
        T[0].append(3)

    for i in range(1, len(seq1)+1):
        S.append([g * i])
        T.append([2])

    for i in range(0, len(seq1)):
        for j in range(len(seq2)):
            s1 = S[i][j] + score_pos (seq1[i], seq2[j], sm, g);
            s2 = S[i][j+1] + g
            s3 = S[i+1][j] + g
            S[i+1].append(max(s1, s2, s3))
            T[i+1].append(max3t(s1, s2, s3))

    return (S, T)


def max3t (v1, v2, v3):
    if v1 > v2:
        if v1 > v3:
            return 1
        else:
            return 3
    else:
        if v2 > v3:
            return 2
        else:
            return 3


def recover_align (T, seq1, seq2):
    res = ["", ""]
    i = len(seq1)
    j = len(seq2)

    while i>0 or j>0:
        if T[i][j]==1:
            res[0] = seq1[i-1] + res[0]
            res[1] = seq2[j-1] + res[1]
            i -= 1
            j -= 1
        elif T[i][j] == 3:
            res[0] = "-" + res[0]
            res[1] = seq2[j-1] + res[1]
            j -= 1
        else:
            res[0] = seq1[i-1] + res[0]
            res[1] = "-" + res[1]
            i -= 1

    return res


def smith_Waterman (seq1, seq2, sm, g):
    S = [[0]]
    T = [[0]]
    maxscore = 0

    for j in range(1, len(seq2)+1):
        S[0].append(0)
        T[0].append(0)

    for i in range(1, len(seq1)+1):
        S.append([0])
        T.append([0])

    for i in range(0, len(seq1)):
        for j in range(len(seq2)):
            s1 = S[i][j] + score_pos (seq1[i], seq2[j], sm, g);
            s2 = S[i][j+1] + g
            s3 = S[i+1][j] + g
            b = max(s1, s2, s3)
            if b <= 0:
                S[i+1].append(0)
                T[i+1].append(0)
            else:
                S[i+1].append(b)
                T[i+1].append(max3t(s1, s2, s3))
                if b > maxscore:
                    maxscore = b

    return (S, T, maxscore)


def recover_align_local (S, T, seq1, seq2):
    res = ["", ""]
    i, j = max_mat(S)

    while T[i][j]>0:
        if T[i][j]==1:
            res[0] = seq1[i-1] + res[0]
            res[1] = seq2[j-1] + res[1]
            i -= 1
            j -= 1
        elif T[i][j] == 3:
            res[0] = "-" + res[0];
            res[1] = seq2[j-1] + res[1]
            j -= 1
        elif T[i][j] == 2:
            res[0] = seq1[i-1] + res[0]
            res[1] = "-" + res[1]
            i -= 1

    return res


def max_mat(mat):
    maxval = mat[0][0]
    maxrow = 0
    maxcol = 0

    for i in range(len(mat)):
        for j in range(len(mat[0])):
            if mat[i][j] > maxval:
                maxval = mat[i][j]
                maxrow = i
                maxcol = j

    return (maxrow, maxcol)


def print_mat (mat):
    for i in range(0, len(mat)):
        print(mat[i])


def read_submat_file (filename):
    sm = {}
    f = open(filename, "r")
    line = f.readline()
    tokens = line.split("\t")
    ns = len(tokens)
    alphabet = []

    for i in range(0, ns):
        alphabet.append(tokens[i][0])
    for i in range(0,ns):
        line = f.readline();
        tokens = line.split("\t");
        for j in range(0, len(tokens)):
            k = alphabet[i]+alphabet[j]
            sm[k] = int(tokens[j])

    return sm


def create_submat (match, mismatch, alphabet):
    sm = {}
    for c1 in alphabet:
        for c2 in alphabet:
            if (c1 == c2):
                sm[c1+c2] = match
            else:
                sm[c1+c2] = mismatch
    return sm


def test():
    sm = create_submat(1, 0, "ACGT")
    seq1 = "TTACGG"
    seq2 = "TAACGG"

    print("GLOBAL ALIGNMENT")
    print("")
    res = needleman_Wunsch(seq1, seq2, sm, -2)
    S = res[0]
    T = res[1]

    print("Score Matrix:")
    print_mat(S)
    print("")


    print("Traceback Matrix:")
    print_mat(T)
    print("")

    i, j = max_mat(S)
    best_score = S[i][j]
    print ("The best score:", str(best_score))
    print("")


    alig = recover_align(T, seq1, seq2)
    print("The best alignment:", alig[0], "+", alig[1])
    print("")

    print("No multiple best alignments.")
    print("")

    print("--------------------------------")
    print("")

    print("LOCAL ALIGNMENT")
    print("")
    res = smith_Waterman(seq1, seq2, sm, -2)
    S = res[0]
    T = res[1]

    print("Score Matrix:")
    print_mat(S)
    print("")

    print("Traceback Matrix:")
    print_mat(T)
    print("")

    i, j = max_mat(S)
    best_score = S[i][j]
    print ("The best score:", str(best_score))
    print("")

    alinL = recover_align_local(S, T, seq1, seq2)
    print("The best alignment:", alinL[0], "+", alinL[1])
    print("")

    print("No multiple best alignments.")
    print("")

test()
