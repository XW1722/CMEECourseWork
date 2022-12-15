"""prints a combination of string and pre-defined elements"""

__appname__ = "tuple.py"

birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
        )

for latin_name,common_name,mass in birds:
    print("Latin name: ",latin_name," Common name: ",common_name," Mass: ",mass, end='\n')
