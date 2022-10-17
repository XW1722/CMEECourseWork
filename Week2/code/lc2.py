rainfall = (('JAN',111.4),
            ('FEB',126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG',140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV',128.4),
            ('DEC',142.2),
           )


# Step 1: Monthes and rainfall values when the amount of rain was greater than 100mm:
great_rainfall = [i for i in rainfall if i[1] > 100]

# Step 2: Months when the amount of rain is less than 50 mm
months = [i[0] for i in rainfall if i[1] < 50]

# Step 3: Conventional loops
great_rainfall1 = []
for i in rainfall:
    if i[1] > 100:
        great_rainfall1.append(i)
print(great_rainfall)

months1 = []
for i in rainfall:
    if i[1] < 50:
        months1.append(i[0])
        print(months1)


