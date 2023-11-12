# egocenters-template
Template Repository for HW#3

## Task for HW3

The [SNAP](https://snap.stanford.edu/data/egonets-Facebook.html) database called `facebook_combined.txt` includes a union of 10 *ego networks* from Facebook. An [ego network](http://www.analytictech.com/networks/egonet.htm) is a subgraph of the full friendship graph which includes a central node (ego) together with the vertices to which the ego is connected directly, and edges among them. Thus, each vertex in our dataset is either one of the 10 ego network centers, or is directly connected to (at least) one of them.

The task is as follows. You are given a set of 11 vertices, and it is guaranteed that this set includes all 10 ego network centers, plus one extra vertex, which we shall call the **impostor**. Your task is to distinguish the impostor and return it.

### Technical Details

You should implement your solution in a file called `egocenters.py`, as a function `find_impostor(edgelist, pseudocenters)`. Here `edgelist` is the list of edges of the graph, like this: `[(1,2), (3,5), (1,3)]`. Vertices are numbered by natural numbers. Next, `pseudocenters` is the list of the ego centers plus the impostor.

You are advised to use NetworkX. Generating a graph from `edgelist` is performed as follows:
```
    G = nx.Graph()
    G.add_edges_from(edgelist)
 ```
 
 The tests automatically read the `edgelist` from `facebook_combined.txt` and perform several tests of your program, with the vertices shuffled randomly.
