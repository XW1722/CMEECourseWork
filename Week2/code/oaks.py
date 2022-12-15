"""
In-class exercise which finds just those taxa that are oaks trees from a list of species.
"""

__appname__ = "oaks.py"

taxa = ['Quercus robur','Fraxus excelsior', 'Pinus sylvestris','Quercus cerris', 'Quercus petraea']
def is_an_oak(name):
    """defines the function which checks if the name starts with quercus"""
    return name.lower().startswith('quercus')

##Using for loops
oaks_loops = set()
for species in taxa:
    if is_an_oak(species):
        oaks_loops.add(species)
print(oaks_loops)

#Using list comprehensions
oaks_lc = set([species for species in taxa if is_an_oak(species)])
print(oaks_lc)

#Get names in UPPER CASE using for loops
oaks_loops = set()
for species in taxa:
    if is_an_oak(species):
        oaks_loops.add(species.upper())
print(oaks_loops)

#Get names in UPPER CASE using list comprehensions
oaks_lc = set([species.upper for species in taxa if is_an_oak(species)])
print(oaks_lc)