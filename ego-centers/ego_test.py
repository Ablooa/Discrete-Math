#! /usr/bin/python3

import egocenters as cc
import random
import pytest

centers = [ 0, 107, 1684, 1912, 3437, 348, 3980, 414, 686, 698 ]
pseudocenters = [ 0, 107, 1684, 1912, 3437, 348, 612, 3980, 414, 686, 698 ]

real_impostor = 612

f = open('facebook_combined.txt')
lines = f.readlines()

def spl2(x) :
    y = x.split()
    return (int(y[0]),int(y[1]))


edges = list(map(spl2, lines))

imp = cc.find_impostor(edges, pseudocenters)
print (imp)




@pytest.mark.parametrize('execution_number', range(6))
def test_impostor(execution_number):    
    vertices = list(range(0,4039))
    random.shuffle(vertices)
    newedges = list(map(lambda uv : (vertices[uv[0]], vertices[uv[1]]), edges))
    newpseudocenters = list(map(lambda u : vertices[u], pseudocenters))
    newimp = cc.find_impostor(newedges, newpseudocenters)
    print (str(newimp) + " " + str(vertices[real_impostor]))
    assert (newimp == vertices[real_impostor])
    

