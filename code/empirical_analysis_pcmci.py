import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import warnings
from collections import Counter
from sklearn.impute import KNNImputer

import tigramite
from tigramite import data_processing as pp
from tigramite.toymodels import structural_causal_processes as toys
from tigramite import plotting as tp
from tigramite.pcmci import PCMCI


from tigramite.independence_tests.parcorr import ParCorr
from tigramite.independence_tests.gpdc import GPDC
from tigramite.independence_tests.cmiknn import CMIknn
from tigramite.independence_tests.cmisymb import CMIsymb
from tigramite.independence_tests.gsquared import Gsquared

# import warnings

# # Suppress specific warnings from Tigramite
# warnings.filterwarnings(
#     "ignore", 
#     message="In analysis mode 'single', 'data'.shape =*", 
#     module="tigramite.data_processing"
# )

# Load the dataset
data = pd.read_csv("data_lea.csv", sep=";")

# Preprocess the data
data.replace("NA", np.nan, inplace=True)  # Replace 'NA' or 'NaN' with np.nan
data = data.apply(pd.to_numeric, errors="coerce")  # Convert to numeric
print("Missing values per column before imputation:")
print(data.isnull().sum())

# Impute missing values using KNN
imputer = KNNImputer(n_neighbors=5)
data_imputed = pd.DataFrame(imputer.fit_transform(data), columns=data.columns)
data_imputed = data_imputed.round()  # Ensure binary values
print("Missing values per column after imputation:")
print(data_imputed.isnull().sum())


# Rename columns for symptoms
rename_mapping = {
    "mood": "sad",
    "concentration": "con",
    "self_worth": "glt",
    "suicidal_ideation": "sui",
    "tiredness": "ene",
    "sleep": "slp",
    "pleasure": "anh",
    "psychomotor": "mot",
    "appetite": "app",
}
data_imputed.rename(columns=rename_mapping, inplace=True)



# Define symptom columns
symptom_columns = ["sad", "con", "glt", "sui", "ene", "slp", "anh", "mot", "app"]


# Calculate sumscore for each patient
data_imputed['sumscore'] = data_imputed[symptom_columns].sum(axis=1)

# Filter out patients with a sumscore of 4 or less
filtered_data = data_imputed[data_imputed['sumscore'] >= 4]

# Create a Tigramite DataFrame for filtered data
symptom_data = filtered_data[symptom_columns].values
time_index = filtered_data["sessionN"].values
dataframe = pp.DataFrame(data=symptom_data, datatime={0: time_index}, var_names=symptom_columns)

# Split data by patient
patients = filtered_data["PatID.x"].unique()

# Minimum observations required
MIN_OBSERVATIONS = 4

# Filter patients based on the sumscore and minimum observations
filtered_patients = [
    patient for patient in patients
    if filtered_data[filtered_data["PatID.x"] == patient].shape[0] >= MIN_OBSERVATIONS
]

print(f"Number of patients with sufficient data (sumscore > 4 and at least {MIN_OBSERVATIONS} observations): {len(filtered_patients)}")

# Initialize dictionary to store patient results
patient_results = {}

## Run PCMCI
# # Process each patient
# for patient in filtered_patients:
#     patient_data = data_imputed[data_imputed["PatID.x"] == patient]
#     data_shape = patient_data[symptom_columns].shape

#     # Check data validity
#     if data_shape[0] < MIN_OBSERVATIONS:
#         print(f"Skipping Patient {patient}: Insufficient data points ({data_shape[0]} rows).")
#         continue

#     try:
#         symptom_data = patient_data[symptom_columns].values
#         time_index = patient_data["sessionN"].values

#         # Create Tigramite DataFrame
#         tigramite_dataframe = pp.DataFrame(
#             data=symptom_data,
#             datatime={0: time_index},
#             var_names=symptom_columns
#         )

#         # Run PCMCI
#         pcmci = PCMCI(dataframe=tigramite_dataframe, cond_ind_test=Gsquared())
#         results = pcmci.run_pcmci(tau_max=3, pc_alpha=0.1)
#         patient_results[patient] = results

#     except Exception as e:
#         print(f"Error for Patient {patient}: {e}")


## Run PCMCI+
# Process each patient
for patient in filtered_patients:
    patient_data = data_imputed[data_imputed["PatID.x"] == patient]
    data_shape = patient_data[symptom_columns].shape

    if data_shape[0] < MIN_OBSERVATIONS:
        print(f"Skipping Patient {patient}: Insufficient data points ({data_shape[0]} rows).")
        continue

    try:
        symptom_data = patient_data[symptom_columns].values
        time_index = patient_data["sessionN"].values

        # Create Tigramite DataFrame
        tigramite_dataframe = pp.DataFrame(
            data=symptom_data,
            datatime={0: time_index},
            var_names=symptom_columns
        )

        # Run PCMCI+
        pcmci = PCMCI(dataframe=tigramite_dataframe, cond_ind_test=Gsquared(), verbosity=1)

        tau_max = 3
        pc_alpha = 0.01
        pcmci.verbosity = 2

        results = pcmci.run_pcmciplus(
            tau_min=0,  # Contemporaneous links
            tau_max=tau_max,
            pc_alpha=pc_alpha
        )
        patient_results[patient] = results

    except Exception as e:
        print(f"Error for Patient {patient}: {e}")




# Collect all edges across all patients
# all_edges = []
# Collect directed edges across all patients (excluding circle-circle edges)
directed_edges = []
non_empty_networks = 0

# Iterate through each patient's results
for patient, results in patient_results.items():
    graph = results['graph']
    p_matrix = results['p_matrix']
    alpha_level = 0.01

    # Initialize a flag to check if there are any significant edges in this network
    significant_edges_exist = False

    # Loop through variables to extract directed edges (excluding circle-circle)
    for i, target_var in enumerate(symptom_columns):
        for j, source_var in enumerate(symptom_columns):
            if i == j:
                continue  # Skip self-loops
            
            # Check for significant edges across all lags
            edge_types = set()
            for lag in range(graph.shape[2]):  # Iterate over lags
                if p_matrix[j, i, lag] < alpha_level and graph[j, i, lag] != '':
                    edge_type = graph[j, i, lag]
                    
                    # Exclude circle-circle (undirected) edges
                    if "o-o" not in edge_type:  # This line excludes undirected edges
                        edge_types.add(edge_type)  # Record directed edge type
            
            # Combine all edge types into a single unique representation
            if edge_types:
                # Sort edge types for consistency (e.g., '-->' and '<--')
                edge_representation = f"{source_var} ({', '.join(sorted(edge_types))}) {target_var}"
                directed_edges.append(edge_representation)
                significant_edges_exist = True


    # If at least one directed edge is found, count this as a non-empty network
    if significant_edges_exist:
        non_empty_networks += 1

# Count edge frequencies
edge_counts = Counter(directed_edges)

# # Print all directed edges with their counts
# print("Directed Edges with Frequencies:")
# for edge, count in edge_counts.items():
#     print(f"{edge}: {count} occurrences")

# Calculate percentages based on the total number of edges
total_edges = len(directed_edges)
# edge_percentages = {edge: (count / non_empty_networks) * 100 for edge, count in edge_counts.items()}
edge_percentages = {edge: (count / total_edges) * 100 for edge, count in edge_counts.items()}

# Get the top 30 most common edges
most_common_edges = Counter(edge_percentages).most_common(30)

# Print the total number of edges
print(f"Total Directed Edges: {total_edges}\n")
print(f"Total Non-empty networks: {non_empty_networks}\n")

# Print frequencies and percentages for top 30 edges
print("Top 30 Directed Edge Frequencies and Percentages:")
for edge, percentage in most_common_edges:
    print(f"{edge}: {edge_counts[edge]} occurrences, {percentage:.2f}%")

# # Plot the results
# edges, percentages = zip(*most_common_edges)
# plt.figure(figsize=(10, 6))
# plt.barh(edges, percentages, color='skyblue')
# plt.xlabel("Percentage (%)")
# plt.ylabel("Edges")
# plt.title(f"Top 30 Edge Frequencies (Among {total_edges} Total Edges)")
# plt.gca().invert_yaxis()
# plt.show()


# # Predefine node positions for uniform layouts
# node_positions = {
#     "x": np.cos(np.linspace(0, 2 * np.pi, len(symptom_columns), endpoint=False)),
#     "y": np.sin(np.linspace(0, 2 * np.pi, len(symptom_columns), endpoint=False)),
# }

# # Plot only for specific patients
# patients_to_plot = [15, 19, 20, 27, 28, 100]

# # Loop through specified patients
# for patient in patients_to_plot:
#     if patient in patient_results:
#         print(f"Plotting graph for Patient {patient}")
#         results = patient_results[patient]
        
#         # Create plot
#         fig, ax = tp.plot_graph(
#             val_matrix=results['val_matrix'],
#             graph=results['graph'],
#             var_names=symptom_columns,
#             link_colorbar_label='Cross-MCI',
#             node_colorbar_label=None,  # No colorbar for nodes
#             node_pos=node_positions,
#             arrow_linewidth=5,  # Adjust edge thickness here
#             figsize=(6, 6),
#         )
#         ax.set_title(f"Patient {patient} - Causal Graph")
#         figures.append(fig)  # Store for later use

#         # Show the updated plot
#         plt.show()
#     else:
#         print(f"No results available for Patient {patient}. Skipping graph.")


