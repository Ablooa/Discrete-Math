import networkx as nx

def find_impostor(edgelist, pseudocenters):
    G = nx.Graph()
    G.add_edges_from(edgelist)
    
    pseudocenters_neighbors = {center: set(G.neighbors(center)) for center in pseudocenters}
    
    all_neighbors = set(neighbor for neighbors in pseudocenters_neighbors.values() for neighbor in neighbors)
    
    for center, neighbors in pseudocenters_neighbors.items():
        neighbors -= set(pseudocenters)
        if not neighbors:
            return center 
    
    for neighbor in all_neighbors:
        connected_pseudocenters = [center for center, neighbors in pseudocenters_neighbors.items() if neighbor in neighbors]
        
        if len(connected_pseudocenters) > 1:
            for center in connected_pseudocenters:
                pseudocenters_neighbors[center].remove(neighbor)

    for center, neighbors in pseudocenters_neighbors.items():
        if not neighbors:
            return center

    return None
