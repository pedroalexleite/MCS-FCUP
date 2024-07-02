#INTRODUCTION--------------------------------------------------------------------------------------#


#Pedro Leite - 201906697 - Bioinformática - M:CC
#Pedro Carvalho - 201906291 - Bioinformática - M:CC


#Python3 classify_proteins.py –a zincfinger.fasta –b globin.fasta –k 2
import sys
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC
from sklearn.ensemble import RandomForestClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import cross_val_score
from sklearn.metrics import accuracy_score, recall_score, precision_score, f1_score


a = sys.argv[2]
b = sys.argv[4]
k = sys.argv[6]


#PROTOCOL (1 to 4)---------------------------------------------------------------------------------#


#1: Read Files and Create Dictionaries
def read_file(file_path):

    sequences = {}
    with open(file_path, 'r') as file:
        current_id = None
        current_sequence = ""
        for line in file:
            if line.startswith(">"):
                if current_id is not None:
                    sequences[current_id] = current_sequence
                current_id = line.split("|")[1]
                current_sequence = ""
            else:
                current_sequence += line.strip()

        if current_id is not None:
            sequences[current_id] = current_sequence

    return sequences

globin_dict = read_file("globin.fasta")
zincfinger_dict = read_file("zincfinger.fasta")


#2: Generates All the 2-Mers of Amino-Acids
def generate_2mers(amino_acids):
    dipeptides = [a1 + a2 for a1 in amino_acids for a2 in amino_acids]
    return dipeptides


amino_acids = ['A', 'R', 'N', 'D', 'C', 'Q', 'E', 'G', 'H', 'I',
              'L', 'K', 'M', 'F', 'P', 'S', 'T', 'W', 'Y', 'V']
all_2mers = generate_2mers(amino_acids)


#3: Dataframe With FFP Values
def calculate_ffp(sequence, all_2mers):
    ffp_values = []
    total_2mers = len(sequence)-1
    for dipeptide in all_2mers:
        frequency = sequence.count(dipeptide) / total_2mers
        ffp_values.append(frequency)

    return ffp_values

def create_ffp_dataframe(sequences, all_2mers):
    ffp_data = []
    for seq_id, sequence in sequences.items():
        ffp_values = calculate_ffp(sequence, all_2mers)
        ffp_data.append(ffp_values)

    column_names = ['{}{}'.format(a1, a2) for a1 in amino_acids for a2 in amino_acids]
    ffp_df = pd.DataFrame(ffp_data, columns=column_names, index=list(sequences.keys()))

    return ffp_df

globin_ffp_df = create_ffp_dataframe(globin_dict, all_2mers)
zincfinger_ffp_df = create_ffp_dataframe(zincfinger_dict, all_2mers)


#4: Add Column With the Type of Protein (Globin=1 and ZincFinger=0)
def add_class_column(ffp_df, protein_type):
    ffp_df['class'] = protein_type
    return ffp_df

globin_ffp_df = add_class_column(globin_ffp_df, 1)
zincfinger_ffp_df = add_class_column(zincfinger_ffp_df, 0)


#PROTOCOL (5)-------------------------------------------------------------------------------------#


#5: Classification Pipeline


#Merge Dataframes
df = pd.concat([globin_ffp_df, zincfinger_ffp_df])
df.reset_index(drop=True, inplace=True)


#Split Data Into Features and Target
features = df.drop(columns=['class'])
target = df['class']


#Split Data Into Training (80%) and Testing (20%)
x_train, x_test, y_train, y_test = train_test_split(features, target, test_size=0.2, random_state=42)


#A: Machine Learning Algorithms

#I: Support Vector Machines
svm_classifier = SVC(kernel='linear')
svm_classifier.fit(x_train, y_train)
y_pred_svm = svm_classifier.predict(x_test)

#II: Random Forests
rf_classifier = RandomForestClassifier(random_state=42)
rf_classifier.fit(x_train, y_train)
y_pred_rf = rf_classifier.predict(x_test)

#III: Naive Bayes
nb_classifier = GaussianNB()
nb_classifier.fit(x_train, y_train)
y_pred_nb = nb_classifier.predict(x_test)


#B: Cross-Validation (Stratified K-Fold)
skf = StratifiedKFold(n_splits=10, shuffle=True, random_state=42)

#I: Support Vector Machines
svm_cv = cross_val_score(svm_classifier, features, target, cv=skf)

#II: Random Forests
rf_cv = cross_val_score(rf_classifier, features, target, cv=skf)

#III: Naive Bayes
nb_cv = cross_val_score(nb_classifier, features, target, cv=skf)


#C: Evaluate (Accuracy, Recall, Precision, F1-Score)

#I: Support Vector Machines
svm_accuracy = accuracy_score(y_test, y_pred_svm)
svm_recall = recall_score(y_test, y_pred_svm, average=None)
svm_precision = precision_score(y_test, y_pred_svm, average=None)
svm_f1 = f1_score(y_test, y_pred_svm, average=None)

#II: Random Forests
rf_accuracy = accuracy_score(y_test, y_pred_rf)
rf_recall = recall_score(y_test, y_pred_rf, average=None)
rf_precision = precision_score(y_test, y_pred_rf, average=None)
rf_f1 = f1_score(y_test, y_pred_rf, average=None)

#III: Naive Bayes
nb_accuracy = accuracy_score(y_test, y_pred_nb)
nb_recall = recall_score(y_test, y_pred_nb, average=None)
nb_precision = precision_score(y_test, y_pred_nb, average=None)
nb_f1 = f1_score(y_test, y_pred_nb, average=None)


#D: Average and Standard Deviation Across the 10 Folds

#I: Support Vector Machines
svm_cv_mean = svm_cv.mean()
svm_cv_std = svm_cv.std()

#II: Random Forests
rf_cv_mean = rf_cv.mean()
rf_cv_std= rf_cv.std()

#III: Naive Bayes
nb_cv_mean = nb_cv.mean()
nb_cv_std = nb_cv.std()


#E: Table with the Results
results_dict = {
    'Model': ['SVM', 'Random Forest', 'Naive Bayes'],
    'Accuracy': [svm_accuracy, rf_accuracy, nb_accuracy],
    'Recall (Class 0)': [svm_recall[0], rf_recall[0], nb_recall[0]],
    'Recall (Class 1)': [svm_recall[1], rf_recall[1], nb_recall[1]],
    'Precision (Class 0)': [svm_precision[0], rf_precision[0], nb_precision[0]],
    'Precision (Class 1)': [svm_precision[1], rf_precision[1], nb_precision[1]],
    'F1-Score (Class 0)': [svm_f1[0], rf_f1[0], nb_f1[0]],
    'F1-Score (Class 1)': [svm_f1[1], rf_f1[1], nb_f1[1]],
    'CV Mean Accuracy': [svm_cv_mean, rf_cv_mean, nb_cv_mean],
    'CV Std Dev Accuracy': [svm_cv_std, rf_cv_std, nb_cv_std]
}
results_df = pd.DataFrame(results_dict)
results_df.to_csv('results.csv', index=False)


#--------------------------------------------------------------------------------------------------#
