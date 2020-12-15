#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 30 13:14:46 2020

@author: mattia
"""

import numpy as np
import igraph
from scipy import sparse
from glob import glob
from scipy.io import loadmat
import networkx as nx
import numpy as np
from sklearn.preprocessing import normalize
from scipy import sparse

Q_0 = 1. / np.log(2)

class Network():

    def __init__(self, adjacency):

        assert not (adjacency != adjacency.T).nnz
        assert not adjacency.diagonal().sum() 
        assert isinstance(adjacency, sparse.csr_matrix)

        self._cache = {'complexity': [], 'entropy': None, 'linkdensity': None}
        self._adjacency = adjacency
        self._traverse_probabilities = normalize(adjacency, norm='l1', axis=1)
        self._N = adjacency.shape[0] 
        self._triu_indices = np.triu_indices(adjacency.shape[0], k=1)
        self._normalization = np.log(adjacency.shape[0] - 1)

    def entropy(self, normalized=True):

        number_of_nodes = self._N
        adjacency = self._adjacency
        traverse_probs = self._traverse_probabilities

        if self._cache['entropy'] is not None:
            if normalized:
                return self._cache['entropy'] / self._normalization
            else:
                return self._cache['entropy'] 

        entropy = self._shannon_entropy(traverse_probs)
        self._cache['entropy'] = entropy

        if normalized:
            entropy = entropy / self._normalization

        return entropy


    def _shannon_entropy(self, probalities):
        
        number_of_nodes = self._N

        nonzero_probs = probalities.data.flatten()

        entropy = np.array(np.log(nonzero_probs)) * np.array(nonzero_probs)
        entropy = - entropy.sum() / number_of_nodes 
    
        return entropy


    def reference_probabilities(self):

        linking_probability = self.link_density()
        number_of_nodes = self._N

        ind0, ind1 = self._triu_indices
        
        random_indices = np.random.random(len(ind0)) < linking_probability
        ind0 = ind0[random_indices]
        ind1 = ind1[random_indices]
        reference_edges = (np.concatenate((ind0, ind1)), np.concatenate((ind1,
                                                                        ind0))) 

        reference_adjacency = sparse.csr_matrix(
            (np.ones(2 * len(ind0)), reference_edges), 
            shape=(number_of_nodes, number_of_nodes))

        reference_traverse_probs = normalize(reference_adjacency, 
                                             norm='l1', 
                                             axis=1)
        return reference_traverse_probs


    def complexity(self):   
        traverse_probs = self._traverse_probabilities

        network_entropy = self.entropy(normalized=False)
        normalized_network_entropy = self.entropy()

        reference_probs = self.reference_probabilities()
        reference_entropy = self._shannon_entropy(reference_probs)

        joint_probabilities = 0.5 * (traverse_probs + reference_probs)
        joint_entropy = self._shannon_entropy(joint_probabilities)

        complexity = normalized_network_entropy * Q_0 
        complexity *= joint_entropy - (network_entropy + reference_entropy) / 2 
        
        self._cache['complexity'].append(complexity)

        return complexity
      

    def previous_complexities(self):
        return self._cache['complexity']

    
    def average_complexity(self, with_std=True, n_samples=None):
        complexities = self._cache['complexity']

        if n_samples is None:
            n_samples = len(complexitie)

        while len(complexities) < n_samples:
            self.complexity()
       
        avg_complexity = np.mean(complexities[:n_samples])
        if with_std:
            return avg_complexity, np.std(complexities[:n_samples])
        else:
            return avg_complexity


    def link_density(self):
       
        if self._cache['linkdensity'] is not None:
            return self._cache['linkdensity']

        adjacency = self._adjacency
        N = self._N
        link_density = adjacency.sum() / (N * (N - 1))
        self._cache['link_density'] = link_density
        
        return link_density


    def number_of_nodes(self):
        return self._N

def adjacency_from_edges(edges, number_of_nodes=None):
    edges = np.array(edges).T
    if number_of_nodes is None:
        number_of_nodes = edges.max() + 1
    adjacency = sparse.csr_matrix((np.ones(edges.shape[1]), edges), 
                                  shape=(number_of_nodes, number_of_nodes))
    return adjacency


def clean_adjacency(adjacency, verbose=True):

    if (adjacency.diagonal().sum()) > 0:
        adjacency = adjacency.tolil()
        for _ in range(adjacency.shape[0]):
            adjacency[_, _] = 0
        adjacency = adjacency.tocsr()

    if (adjacency != adjacency.T).sum() > 0:
        if verbose:
            print("Making adjacency symmetric")
        adjacency = adjacency + adjacency.T
        adjacency[adjacency > 0] = 1
    
    if (adjacency.sum(axis=0) == 0).any():
        if verbose:
            print("Removing isolated nodes")
        nz_row, nz_col = adjacency.nonzero()
        nz_row = np.unique(nz_row)
        nz_col = np.unique(nz_col)
        adjacency = adjacency[nz_row, :][:, nz_col]
    
    return adjacency


def load(input_file, verbose=True):

    ending = input_file.split(".")[-1]
    if ending == "txt":
        adjacency = load_txt(input_file)
    elif ending == "gr":
        adjacency = load_gr(input_file)
    elif ending == "mat":
        adjacency = load_mat(input_file)
    elif ending == "gml" or ending == "graphml":
        adjacency = load_gml(input_file)
    elif ending == "tsv":
        adjacency = load_tsv(input_file)

    adjacency = clean_adjacency(adjacency)

    adjacency = sparse.csr_matrix(adjacency)
    return adjacency


def load_tsv(file_name):
    edges = []
    with open(file_name) as input_file:
        for row in input_file:
            if row[0] != "%":
                if "\t" in row:
                    separator = "\t"
                else:
                    separator = " "
                row = row[:-1].split(separator)
                node_i = row[0]
                node_j = row[1]
                edges.append([int(node_i), int(node_j)])

    adjacency = adjacency_from_edges(edges)

    return adjacency


def load_txt(file_name):
    edges = []
    with open(file_name) as input_file:
        for row in input_file:
            node_i, node_j = row.split(" ")
            edges.append([int(node_i), int(node_j)])

    adjacency = adjacency_from_edges(edges)

    return adjacency


def load_gr(file_name):
    edges = []
    with open(file_name) as input_file:
        for row in input_file:
            if row[0] == "a":
                prefix, node_i, node_j, _ = row.split(" ")
                if prefix == "a":
                    edges.append([int(node_i), int(node_j)])

    adjacency = adjacency_from_edges(edges)

    return adjacency


def load_mat(file_name):
    input_data = loadmat(file_name)
    for (key, value) in input_data.items():
        if key[:2] != "__" and value.ndim == 2:
            if value.shape[0] == value.shape[1]:
                print("Found possible adjacency matrix with key "+key)
                edges = np.array(np.nonzero(value)).T
                number_of_nodes = value.shape[0]

    adjacency = adjacency_from_edges(edges, number_of_nodes)

    return adjacency


def load_gml(input_file):
    net = igraph.load(input_file)
    edges = net.get_edgelist()
    adjacency = adjacency_from_edges(edges)

    return adjacency


def scale_free_network(number_of_nodes, powerlaw_exponent):
    sequence = nx.utils.random_sequence.powerlaw_sequence(number_of_nodes,
                                                          powerlaw_exponent)
    sequence = np.round(sequence).astype(int)

    # Ensure that the total number of stubs is even
    if (sequence.sum() % 2) != 0:
        sequence[np.random.randint(number_of_nodes)] += 1

    # Initiate the network
    network = nx.configuration_model(sequence)

    adjacency = adjacency_from_edges(list(network.edges()), number_of_nodes)

    adjacency = clean_adjacency(adjacency)
    adjacency = sparse.csr_matrix(adjacency)

    return adjacency


def watts_strogatz_network(number_of_nodes, number_neighbors,
                            rewiring_probability):
    network = igraph.Graph.Watts_Strogatz(dim=1, size=number_of_nodes,
                                          nei=number_neighbors,
                                          p=rewiring_probability)
    adjacency = adjacency_from_edges(network.get_edgelist())

    adjacency = clean_adjacency(adjacency)
    adjacency = sparse.csr_matrix(adjacency)

    return adjacency




import os
os.chdir('/Users/mattia/Desktop/all_data')
import pandas as pd

for filename in os.listdir('/Users/mattia/Desktop/all_data')[507:560]:
     name = filename
     net = Network(adjacency=load(filename, verbose = True))
     entropy = net.entropy()
     complexity = net.complexity()
     avg_complexity, std_complexity = net.average_complexity(n_samples=100)
     measures = {'Name': [name],
                 'Entropy': [entropy],
                 'Complexity': [complexity],
                 'AverageComplexity': [avg_complexity]
                 }
     df = pd.DataFrame(measures, columns = ['Name','Entropy', 'Complexity','AverageComplexity'])
     if filename == 'metabolic_networks_from_kegg_(2006)_26.txt':
         df.to_csv('results.csv')
     else:
         df.to_csv('results.csv', mode='a', header=False)

os.listdir('/Users/mattia/Desktop/all_data')[506]








