"""This script includes the content of printing a combination of input string
and the pre-defined elements."""

__appname__ = "lc1.py"

birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
         )

#(1) Write three separate list comprehensions that create three different
# lists containing the latin names, common names and mean body masses for
# each species in birds, respectively. 

latin_names = [i[0] for i in birds]
print("Latin names: \n",latin_names)
common_names = [i[1] for i in birds]
print("Common names: \n",latin_names)
mean_body_masses = [i[2] for i in birds]
print("Mean Body Masses: \n", mean_body_masses)

# (2) Now do the same using conventional loops (you can choose to do this 
# before 1 !). 

for i in birds:
    latin_names1 = i[0]
    print("Latin names: \n",latin_names1)

for i in birds:
    common_names1 = i[1]
    print("Common names: \n",common_names1)

for i in birds:
    mean_body_masses1 = i[2]
    print("Mean Body Masses: \n", mean_body_masses1)
