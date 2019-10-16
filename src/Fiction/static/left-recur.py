import graphviz_artist as ga
import graphviz_artist.attr as a

"""
A -> b c
A -> A d
"""
g = ga.Graph(directed=True)
g.update(attrs=(a.HorizontalGraph, ))

A = g.new(a.Label("start of  A"))

b = g.new(a.Label("b"))
c = g.new(a.Label("c"))
d = g.new(a.Label("d"))

B = g.new(a.Label("push scope of B"))

B_ = g.new(a.Label("pop scope of B"))

A_ = g.new(a.Label("end of  A"))

_ = A > b > c > A_
_ = A_[a.Label("left recur")] > B > d > B_[a.Label("left recursion reduction")] > A

g.view()




