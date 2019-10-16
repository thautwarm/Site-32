import graphviz_artist as ga
import graphviz_artist.attr as a

g = ga.Graph(directed=True)
g.update(attrs=(a.HorizontalGraph, ))

math = g.new(a.Label("math : Python的一个library"))
pi = g.new(a.Label("pi : math.pi"))
r = g.new(a.Label("r : 1"))
h = g.new(a.Label("h : 1"))
S = g.new(a.Label("S : 2 * S_top + S_side"))
S_top = g.new(a.Label("S_top : pi * r ** 2"))
S_side = g.new(a.Label("S_side : C * h "))
C = g.new(a.Label("C : 2 * pi * r"))

_ = math > pi > r > h > S_top > C > S_side > S

g.view()