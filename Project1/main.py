#
# Shayan Rasheed
# CS 341 - Project 1
# A program that outputs stats for the CTA database
# as well as performing different commands
# that can plot the data as a graph
#

import sqlite3
import matplotlib.pyplot as figure


###########################################################  
#
# print_stats
#
# Given a connection to the CTA database, executes various
# SQL queries to retrieve and output basic stats.
#
def print_stats(dbConn):
    dbCursor = dbConn.cursor()
    
    print("General stats:")
    
    # Print number of stations
    dbCursor.execute("Select count(*) From Stations;")
    row = dbCursor.fetchone();
    print("  # of stations:", f"{row[0]:,}")

    # Print number of stops
    dbCursor.execute("Select count(*) From Stops;")
    row = dbCursor.fetchone();
    print("  # of stops:", f"{row[0]:,}")

    # Print number of ride entries
    dbCursor.execute("Select count(*) From Ridership;")
    row = dbCursor.fetchone();
    print("  # of ride entries:", f"{row[0]:,}")

    # Print the data range
    dbCursor.execute("Select Max(Date(Ride_date)), Min(Date(Ride_date)) From Ridership;")
    row = dbCursor.fetchone();
    print("  date range:", f"{row[1]}", "-", f"{row[0]}")

    # Print the total Ridership
    dbCursor.execute("Select Sum(Num_Riders) From Ridership;")
    row = dbCursor.fetchone();
    total = row[0]
    print("  Total ridership:", f"{total:,}")

    # Weekday Ridership
    dbCursor.execute("Select Sum(Num_Riders) From Ridership Where Type_of_Day = 'W';")
    row = dbCursor.fetchone();
    print("  Weekday ridership:", f"{row[0]:,}", f"({float(row[0])/total*100:.2f}%)")

    # Saturday Ridership
    dbCursor.execute("Select Sum(Num_Riders) From Ridership Where Type_of_Day = 'A';")
    row = dbCursor.fetchone();
    print("  Saturday ridership:", f"{row[0]:,}", f"({float(row[0])/total*100:.2f}%)")

    # Sunday/Holiday Ridership
    dbCursor.execute("Select Sum(Num_Riders) From Ridership Where Type_of_Day = 'U';")
    row = dbCursor.fetchone();
    print("  Sunday/holiday ridership:", f"{row[0]:,}", f"({float(row[0])/total*100:.2f}%)")

# Function Command 1:
# Retrieves list of station names based
# on a partial name inputted by user
def command1(dbConn):
  print()
  name = input("Enter partial station name (wildcards _ and %): ")

  dbCursor = dbConn.cursor()

  sql = f"Select Station_ID, Station_Name From Stations Where Station_Name Like '{name}' Order by Station_Name Asc;"

  dbCursor.execute(sql)
  row = dbCursor.fetchone();
  
  # If query did not return anything,
  # then no station was found
  if row is None:
    print("**No stations found...")
  else:
    while row is not None:
      print(f"{row[0]} : {row[1]}")
      row = dbCursor.fetchone()

# Function Command 2:
# Outputs the ridership at each station
# as well as its percentage out of the total ridership
def command2(dbConn):
  print("** ridership all stations **")

  dbCursor = dbConn.cursor()

  # Get the total ridership to calculate the percentage
  dbCursor.execute("Select Sum(Num_Riders) From Ridership;")
  row = dbCursor.fetchone();
  total = row[0]

  dbCursor.execute("""Select Station_Name, Sum(Num_Riders)
    From Ridership 
    Join Stations On Ridership.Station_ID = Stations.Station_ID
    Group By Station_Name Order By Station_Name Asc""")
  row = dbCursor.fetchone();

  while row is not None:
    print(f"{row[0]} : {row[1]:,} ({float(row[1])/total*100:.2f}%)")
    row = dbCursor.fetchone()

  
# Function Command 3:
# Performs both Commands 3 and 4
# Prints out the top ten or low 10 stations
# based on their ridership as well as their percentage
# of total ridership
def command3(dbConn, order):
  # Distinguish between command 3 and command 4
  if order == "Desc":
    print("** top-10 stations **")
  elif order == "Asc":
    print("** least-10 stations **")

  dbCursor = dbConn.cursor()

  dbCursor.execute("Select Sum(Num_Riders) From Ridership;")
  row = dbCursor.fetchone();
  total = row[0]

  dbCursor.execute(f"""Select Station_Name, Sum(Num_Riders)
    From Ridership 
    Join Stations On Ridership.Station_ID = Stations.Station_ID
    Group By Station_Name Order By Sum(Num_Riders) {order}
    Limit 10""")
  row = dbCursor.fetchone();

  while row is not None:
    print(f"{row[0]} : {row[1]:,} ({float(row[1])/total*100:.2f}%)")
    row = dbCursor.fetchone()

# Function command 5:
# Outputs all stop names in a line color
# as well as accessibility and direction
def command5(dbConn):
  print()

  dbCursor = dbConn.cursor()

  color = input("Enter a line color (e.g. Red or Yellow): ")

  # Check for purple express
  if color.capitalize() == "Purple-express":
    color = "Purple-Express"
  else:
    color = color.capitalize()

  dbCursor.execute(f"""Select Stop_Name, Direction, ADA
    From Stops
    Join StopDetails On Stops.Stop_ID = StopDetails.Stop_ID
    Join Lines On StopDetails.Line_ID = Lines.Line_ID
    Where Color = '{color}'
    Order by Stop_Name Asc""")

  row = dbCursor.fetchone();

  if row is None:
    print("**No such line...")
  else:
    while row is not None:
      if int(row[2]) == 1:
        accesibility = 'yes'
      else:
        accesibility = 'no'
      print(f"{row[0]} : direction = {row[1]} (accessible? {accesibility})")
      row = dbCursor.fetchone()  

# Function command 6:
# Performs commands 6 and 7
# Outputs total ridership by month or by year
def command6(dbConn, time):
  # distinguish between commands 6 and 7
  if time == 'm':
    printTime = "month"
  else:
    printTime = "year"
  print(f"** ridership by {printTime} **")

  dbCursor = dbConn.cursor()

  dbCursor.execute(f"""Select strftime('%{time}', Ride_Date),
    Sum(Num_Riders) From Ridership
    Group By strftime('%{time}', Ride_Date)
    Order By strftime('%{time}', Ride_Date) Asc""")

  rows = dbCursor.fetchall();

  for row in rows:
    print(f"{row[0]} : {row[1]:,}")

  print()
  choice = input("Plot? (y/n) ")

  if choice == 'y':
    x=[]
    y=[]

    for row in rows:
        x.append(row[0])
        y.append(row[1])
    
    figure.xlabel("month")
    figure.ylabel("number of riders")
    figure.title("monthly ridership")

    figure.plot(x,y)
    figure.show(block=False)

# Function perform 8
# Helper function for command 8
# Fills in the x and y arrays to be used in plot
# and prints data for a given station
def perform8(dbCursor, year, station, x, y):
  dbCursor.execute(f"""Select Date(Ride_Date), Num_Riders
  From Ridership
  Join Stations on Ridership.Station_ID = Stations.Station_ID 
  Where strftime('%Y', Ride_Date) = '{year}'
  AND Station_Name Like '{station}'
  Order by Ride_Date Asc""")

  rows = dbCursor.fetchall()
  count = 0

  while count < 5:
    print(f"{rows[count][0]} {rows[count][1]}")
    count += 1
  
  count = len(rows) - 5
  while count < len(rows):
    print(f"{rows[count][0]} {rows[count][1]}")
    count += 1

  for row in rows:
    x.append(row[0])
    y.append(row[1])

# Function command 8:
# Prints the first 5 and last 5 days of ridership
# at two stations specified by the user for a given year
# and creates a plot of the two stations
def command8(dbConn):
  print()

  dbCursor = dbConn.cursor()

  x = []
  y = []

  a = []
  b = []


  year = input("Year to compare against? ")
  print()
  station1 = input("Enter station 1 (wildcards _ and %): ")

  # Check that station 1 is valid
  dbCursor.execute(f"Select Station_ID, Station_Name from Stations where Station_Name like '{station1}'")
  rows = dbCursor.fetchall()
  if len(rows) > 1:
    print("**Multiple stations found...")
    return
  elif len(rows) == 0:
    print("**No station found...")
    return
  
  id1 = rows[0][0]
  name1 = rows[0][1]

  print()
  station2 = input("Enter station 2 (wildcards _ and %): ")

  # Check that station 2 is valid
  dbCursor.execute(f"Select Station_ID, Station_Name from Stations where Station_Name like '{station2}'")
  rows = dbCursor.fetchall()
  if len(rows) > 1:
    print("**Multiple stations found...")
    return
  elif len(rows) == 0:
    print("**No station found...")
    return

  id2 = rows[0][0]
  name2 = rows[0][1]

  # Call perform8 to print the data
  print(f"Station 1: {id1} {name1}")

  perform8(dbCursor, year, station1, x, y)

  print(f"Station 2: {id2} {name2}")

  perform8(dbCursor, year, station2, a, b)

  print()
  # Create plot
  choice = input("Plot? (y/n) ")

  if choice == 'y':
    figure.xlabel("day")
    figure.ylabel("number of riders")
    figure.title("riders each day of 2020")

    figure.plot(x,y)
    figure.plot(a,b)
    figure.show(block=False)
  
# Function command 9:
# Outputs all station names in a line color
# and plots them based on their location
def command9(dbConn):
  print()

  dbCursor = dbConn.cursor()

  color = input("Enter a line color (e.g. Red or Yellow): ")

  # Check for purple express
  if color.capitalize() == "Purple-express":
    color = "Purple-Express"
  else:
    color = color.capitalize()

  dbCursor.execute(f"""Select Distinct Station_Name, Latitude, Longitude
    From Stations
    Join Stops on Stations.Station_ID = Stops.Station_ID
    Join StopDetails On Stops.Stop_ID = StopDetails.Stop_ID
    Join Lines On StopDetails.Line_ID = Lines.Line_ID
    Where Color = '{color}'
    Order By Station_Name Asc""")

  rows = dbCursor.fetchall();

  # Check if line was found
  if len(rows) == 0:
    print("**No such line...")
    return
  else:
    for row in rows:
      print(f"{row[0]} : ({row[1]}, {row[2]})")

  print()
  # Create plot
  choice = input("Plot? (y/n) ")

  if choice == 'y':
    x = []
    y = []

    for row in rows:
      x.append(row[2])
      y.append(row[1])
    
    image = figure.imread("chicago.png")
    xydims = [-87.9277, -87.5569, 41.7012, 42.0868]
    figure.imshow(image, extent=xydims)

    figure.title(color + " line")

    if color.lower() == "purple-express":
      color = "Purple"
    
    figure.plot(x, y, "o", c=color)

    for row in rows:
      figure.annotate(row[0], (row[2], row[1]))
    
    figure.xlim([-87.9277, -87.5569])
    figure.ylim([41.7012, 42.0868])

    figure.show(block=False)


###########################################################  
#
# main
#
print('** Welcome to CTA L analysis app **')
print()

dbConn = sqlite3.connect('CTA2_L_daily_ridership.db')

print_stats(dbConn)
print()

val = input("Please enter a command (1-9, x to exit): ")

# Get commands from user until 'x' is input
while (val != 'x'):
  if val == '1':
    command1(dbConn)
  elif val == '2':
    command2(dbConn)
  elif val == '3':
    command3(dbConn, "Desc")
  elif val == '4':
    command3(dbConn, "Asc")
  elif val == '5':
    command5(dbConn)
  elif val == '6':
    command6(dbConn, 'm')
  elif val == '7':
    command6(dbConn, 'Y')
  elif val == '8':
    command8(dbConn)
  elif val == '9':
    command9(dbConn)
  else:
    print("**Error, unknown command, try again...")
  print()
  val = input("Please enter a command (1-9, x to exit): ")
  

#
# done
#
