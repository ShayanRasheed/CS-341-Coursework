#
# main.py
#
# 
#   Shayan Rasheed
#   CS 341, Spring 2022
#   Project 02
#
import sqlite3
import objecttier

##################################################################  
# 
# retrieve_movie
#
def retrieve_movie(dbConn):
    print()
    name = input("Enter movie name (wildcards _ and % supported): ")
    print()

    movies = objecttier.get_movies(dbConn, name)

    if movies is None:  # error
      print("**Internal error: retrieve_movie")
    elif len(movies) == 0:
      print("# of movies found: 0")
    elif len(movies) > 100:
      print(f"# of movies found: {len(movies)}")
      print()
      print("There are too many movies to display, please narrow your search and try again...")
    else:
      print(f"# of movies found: {len(movies)}")
      print()
      for m in movies:
        print(f"{m.Movie_ID} : {m.Title} ({m.Release_Year})")

##################################################################  
# 
# retrieve_movieDetails
#
def retrieve_movieDetails(dbConn):
    print()
    id = input("Enter movie id: ")
    print()

    movie = objecttier.get_movie_details(dbConn, id)

    if movie is None:  # error
      print("No such movie...")
    else:
      print(f"{movie.Movie_ID} : {movie.Title}")
      print(f"  Release date: {movie.Release_Date}")
      print(f"  Runtime: {movie.Runtime} (mins)")
      print(f"  Orig language: {movie.Original_Language}")
      print(f"  Budget: ${movie.Budget:,} (USD)")
      print(f"  Revenue: ${movie.Revenue:,} (USD)")
      print(f"  Num reviews: {movie.Num_Reviews}")
      print(f"  Avg rating: {movie.Avg_Rating:.2f} (0..10)")
      
      print("  Genres:", end=" ")
      for g in movie.Genres:
        print(g, end=", ")
      print()
      
      print("  Production companies:", end=" ")
      for c in movie.Production_Companies:
        print(c, end=", ")
      print()
            
      print(f"  Tagline: {movie.Tagline}")

##################################################################  
# 
# retrieve_N_movies
#
def retrieve_N_movies(dbConn):
  print()
  N = input("N? ")
  if int(N) < 1:
    print("Please enter a positive value for N...")
    return
  minReviews = input("min number of reviews? ")
  if int(minReviews) < 1:
    print("Please enter a positive value for min number of reviews...")
    return

  movies = objecttier.get_top_N_movies(dbConn, int(N), int(minReviews))

  if len(movies) == 0:
    return
  else:
    print()
    for m in movies:
      print(f"""{m.Movie_ID} : {m.Title} ({m.Release_Year}), avg rating = {m.Avg_Rating:.2f} ({m.Num_Reviews} reviews)""")

##################################################################  
# 
# insert_review
#
def insert_review(dbConn):
  print()
  rating = input("Enter rating (0..10): ")

  if int(rating) >= 0 and int(rating) < 11:
    id = input("Enter movie id: ")
    val = objecttier.add_review(dbConn, int(id), int(rating))
    if val == 1:
      print()
      print("Review successfully inserted")
    else:
      print()
      print("No such movie...")
  else:
    print("Invalid rating...")


##################################################################  
# 
# insert_tagline
#
def insert_tagline(dbConn):
  print()
  tagline = input("tagline? ")
  id = input("movie id? ")

  val = objecttier.set_tagline(dbConn, id, tagline)

  if val == 0:
    print()
    print("No such movie...")
  else:
    print()
    print("Tagline successfully set")

      

##################################################################  
#
# main
#
print('** Welcome to the MovieLens app **')
print()

dbConn = sqlite3.connect('MovieLens.db')

print("General stats:")
print(f"  # of movies: {objecttier.num_movies(dbConn):,}")
print(f"  # of reviews: {objecttier.num_reviews(dbConn):,}")

print()
cmd = input("Please enter a command (1-5, x to exit): ")

while cmd != "x":
  if cmd == "1":
    retrieve_movie(dbConn)
  elif cmd == "2":
    retrieve_movieDetails(dbConn)
  elif cmd == "3":
    retrieve_N_movies(dbConn)
  elif cmd == "4":
    insert_review(dbConn)
  elif cmd == "5":
    insert_tagline(dbConn)
  else:
      print("**Error, unknown command, try again...")

  print()
  cmd = input("Please enter a command (1-5, x to exit): ")

#
# done
#