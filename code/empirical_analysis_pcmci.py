import numpy as np
import matplotlib
from matplotlib import pyplot as plt
import sklearn
import pandas as pd

import tigramite
from tigramite import data_processing as pp
from tigramite.toymodels import structural_causal_processes as toys

from tigramite import plotting as tp
from tigramite.plotting import plot_graph
from tigramite.pcmci import PCMCI
from tigramite.lpcmci import LPCMCI

from tigramite.independence_tests.parcorr import ParCorr
from tigramite.independence_tests.robust_parcorr import RobustParCorr
from tigramite.independence_tests.parcorr_wls import ParCorrWLS 
from tigramite.independence_tests.gpdc import GPDC
from tigramite.independence_tests.cmiknn import CMIknn
from tigramite.independence_tests.cmisymb import CMIsymb
from tigramite.independence_tests.gsquared import Gsquared
from tigramite.independence_tests.regressionCI import RegressionCI





# Load the CSV file into a DataFrame
data = pd.read_csv("data_lea.csv", sep =";")

# Replace 'NA' or string 'NaN' with np.nan for consistent handling
data.replace("NA", np.nan, inplace=True)

# Convert all columns to numeric where possible
data = data.apply(pd.to_numeric, errors="coerce")

# Fill or drop NaNs
# Option 1: Fill NaNs with zeros (for binary data, this assumes 'absent' symptom)
data.fillna(0, inplace=True)

# Option 2: Drop rows with NaNs (if you prefer to exclude incomplete data)
# data.dropna(inplace=True)

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
    "appetite": "app"
}
data.rename(columns=rename_mapping, inplace=True)

# Updated symptom columns
symptom_columns = [
    "sad", "con", "glt", "sui", "ene", "slp", "anh", "mot", "app"
]


# Extract symptom data
symptom_data = data[symptom_columns].values

# Extract the time index
time_index = data["sessionN"].values

# Specify variable names
var_names = symptom_columns

# Create Tigramite DataFrame
dataframe = pp.DataFrame(
    symptom_data,
    datatime={0: time_index},
    var_names=var_names
)

print(dataframe)

## each patient version
# Split the dataset by patient
patients = data["PatID.x"].unique()


# Dictionary to store results for each patient
patient_results = {}

# Loop through each patient
for patient in patients:
    print(f"Running PCMCI for Patient {patient}")
    
    # Filter data for the current patient
    patient_data = data[data["PatID.x"] == patient]
    
    # Ensure the patient has enough data points
    if patient_data.shape[0] < 6:  # Assuming tau_max=3 requires at least 4 rows
        print(f"Skipping Patient {patient} due to insufficient time points.")
        continue
    
    # Prepare symptom data and time index
    patient_symptoms = patient_data[symptom_columns].values
    time_index = patient_data["sessionN"].values
    
    # Check if the patient_symptoms array is valid
    if patient_symptoms.size == 0:
        print(f"Skipping Patient {patient} due to empty data.")
        continue
    
    # Create Tigramite DataFrame for the patient
    tigramite_dataframe = pp.DataFrame(
        patient_symptoms,
        datatime={0: time_index},
        var_names=symptom_columns
    )
    
    # Initialize PCMCI with Gsquared
    gsquared = Gsquared()
    pcmci = PCMCI(dataframe=tigramite_dataframe, cond_ind_test=gsquared)
    
    # Run PCMCI
    try:
        results = pcmci.run_pcmci(tau_max=3, pc_alpha=0.1)
        
        # Store the results
        patient_results[patient] = results
        
        # Print significant links for this patient
        print(f"Results for Patient {patient}:")
        pcmci.print_significant_links(
            p_matrix=results['p_matrix'],
            val_matrix=results['val_matrix'],
            alpha_level=0.05
        )

    except ValueError as e:
        print(f"Error for Patient {patient}: {e}")
        continue

# Plot the graphs for the specified patients
# for patient in patients_to_plot:
#     if patient in patient_results:
#         print(f"Plotting graph for Patient {patient}")
#         results = patient_results[patient]
#         fig, ax = tp.plot_graph(
#             figsize=(6, 6),
#             val_matrix=results['val_matrix'],
#             graph=results['graph'],
#             var_names=symptom_columns,
#             link_colorbar_label='Cross-MCI',
#             node_colorbar_label=None,
#             node_aspect=None,  # Default node aspect
#         )
#         # Customize the nodes to be white with black rims
#         for node in ax.collections:  # Nodes are matplotlib collections
#             node.set_facecolor("white")  # Set node fill color to white
#             node.set_edgecolor("black")  # Set node rim color to black

#         plt.title(f"Patient {patient} - Causal Graph")
#         plt.show()
#     else:
#         print(f"No results available for Patient {patient}. Skipping graph.")
    




from collections import Counter

# Dictionary to store all significant edges across patients
all_edges = []

# Loop through results for each patient
for patient, results in patient_results.items():
    val_matrix = results['val_matrix']
    p_matrix = results['p_matrix']
    alpha_level = 0.05  # Significance threshold
    
    # Extract significant edges
    significant_edges = []
    for i, target_var in enumerate(symptom_columns):
        for j, source_var in enumerate(symptom_columns):
            for lag in range(1, 4):  # tau_max = 3
                if p_matrix[j, i, lag] < alpha_level:
                    edge = (f"{source_var} (lag {lag}) -> {target_var}")
                    significant_edges.append(edge)
    
    # Store edges for this patient
    all_edges.extend(significant_edges)

# Count the frequency of each edge
edge_counts = Counter(all_edges)

# Print the most common edges
print("\nMost Common Edges Across Patients:")
for edge, count in edge_counts.most_common(30):
    print(f"{edge}: {count} occurrences")




# Dictionary to store all significant edges across non-empty networks
all_edges = []
non_empty_networks = 0  # Counter for non-empty networks

# Loop through results for each patient
for patient, results in patient_results.items():
    val_matrix = results['val_matrix']
    p_matrix = results['p_matrix']
    alpha_level = 0.05  # Significance threshold

    # Check if there are any significant edges (excluding self-loops)
    significant_edges_exist = False
    for i, target_var in enumerate(symptom_columns):
        for j, source_var in enumerate(symptom_columns):
            if i == j:
                continue  # Skip self-loops
            for lag in range(1, 4):  # tau_max = 3
                if p_matrix[j, i, lag] < alpha_level:
                    significant_edges_exist = True
                    edge = (f"{source_var} (lag {lag}) -> {target_var}")
                    all_edges.append(edge)

    # If significant edges exist, count this graph as non-empty
    if significant_edges_exist:
        non_empty_networks += 1

# Count the frequency of each edge
edge_counts = Counter(all_edges)
# Number of non empty networks
print("Num. of non-empty network:", non_empty_networks)

# Calculate percentages based on non-empty networks
edge_percentages = {edge: (count / non_empty_networks) * 100 for edge, count in edge_counts.items()}

# Get the top 10 most frequent edges
most_common_edges = Counter(edge_percentages).most_common(30)

# Extract edges and percentages
edges, percentages = zip(*most_common_edges)

# Plot the results
plt.figure(figsize=(10, 6))
plt.barh(edges, percentages, color='skyblue')
plt.xlabel("Percentage (%)")
plt.ylabel("Edges")
plt.title(f"Top 30 Edge Frequencies (Among {non_empty_networks} Non-Empty Networks Based on Significant Edges)")
plt.gca().invert_yaxis()
plt.show()

# Print the percentages for top edges
print("\nTop 30 Edge Frequencies as Percentages (Based on Significant Edges):")
for edge, percentage in most_common_edges:
    print(f"{edge}: {percentage:.2f}%")


# # Identify cycles
# print("\nPotential Cycles Detected:")
# cycles = []
# for edge in edge_counts.keys():
#     source, target = edge.split("->")
#     if target.strip() in source:  # Simple heuristic for a cycle
#         cycles.append(edge)
# for cycle in cycles:
#     print(cycle)


# Get the top 10 most common edges
most_common_edges = edge_counts.most_common(30)
edges, counts = zip(*most_common_edges)

# Plot the results
plt.figure(figsize=(10, 6))
plt.barh(edges, counts, color='skyblue')
plt.xlabel("Frequency")
plt.ylabel("Edges")
plt.title("Most Common Edges Across Patients")
plt.gca().invert_yaxis()
plt.show()







# Predefine node positions for uniform layouts
node_positions = {
    "x": np.cos(np.linspace(0, 2 * np.pi, len(symptom_columns), endpoint=False)),
    "y": np.sin(np.linspace(0, 2 * np.pi, len(symptom_columns), endpoint=False)),
}

# Plot only for specific patients
patients_to_plot = [15, 19, 20, 27, 28, 100]

# Store figures for later combination (optional)
figures = []

# Loop through specified patients
for patient in patients_to_plot:
    if patient in patient_results:
        print(f"Plotting graph for Patient {patient}")
        results = patient_results[patient]
        
        # Create plot
        fig, ax = tp.plot_graph(
            val_matrix=results['val_matrix'],
            graph=results['graph'],
            var_names=symptom_columns,
            link_colorbar_label='Cross-MCI',
            node_colorbar_label=None,  # No colorbar for nodes
            node_pos=node_positions,
            arrow_linewidth=5,  # Adjust edge thickness here
            figsize=(6, 6),
        )
        ax.set_title(f"Patient {patient} - Causal Graph")
        figures.append(fig)  # Store for later use

        # Show the updated plot
        plt.show()
    else:
        print(f"No results available for Patient {patient}. Skipping graph.")



# ## aggregated version
# # Aggregate symptoms by session
# symptom_columns = [
#     "mood", "concentration", "self_worth", "suicidal_ideation", "tiredness",
#     "sleep", "pleasure", "psychomotor", "appetite"
# ]

# # Group by sessionN and aggregate (e.g., mean or sum)
# aggregated_data = data.groupby("sessionN")[symptom_columns].mean()


# # Fill NaNs in the aggregated data (if needed)
# aggregated_data.fillna(0, inplace=True)

# # Prepare the Tigramite DataFrame
# time_index = aggregated_data.index.values
# var_names = symptom_columns
# aggregated_symptom_data = aggregated_data.values

# dataframe = pp.DataFrame(
#     aggregated_symptom_data,
#     datatime={0: time_index},
#     var_names=var_names
# )

# print("Prepared aggregated Tigramite dataframe.")

# # tp.plot_timeseries(dataframe); plt.show()



