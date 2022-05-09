#
# objecttier
#
# Builds Movie-related objects from data retrieved through 
# the data tier.
#
#   Shayan Rasheed
#   CS 341, Spring 2022
#   Project #02
#
import datatier


##################################################################
#
# Movie:
#
# Constructor(...)
# Properties:
#   Movie_ID: int
#   Title: string
#   Release_Year: string
#
class Movie:
  def __init__(self, id, title, year):
    self._Movie_ID_ = id
    self._Title_ = title
    self._Release_Year_ = year

  @property
  def Movie_ID(self):
    return self._Movie_ID_

  @property
  def Title(self):
    return self._Title_

  @property
  def Release_Year(self):
    return self._Release_Year_


##################################################################
#
# MovieRating:
#
# Constructor(...)
# Properties:
#   Movie_ID: int
#   Title: string
#   Release_Year: string
#   Num_Reviews: int
#   Avg_Rating: float
#
class MovieRating:
  def __init__(self, id, title, year, reviews, rating):
    self._Movie_ID_ = id
    self._Title_ = title
    self._Release_Year_ = year
    self._Num_Reviews_ = reviews
    self._Avg_Rating_ = rating

  @property
  def Movie_ID(self):
    return self._Movie_ID_

  @property
  def Title(self):
    return self._Title_

  @property
  def Release_Year(self):
    return self._Release_Year_

  @property
  def Num_Reviews(self):
    return self._Num_Reviews_

  @property
  def Avg_Rating(self):
    return self._Avg_Rating_


##################################################################
#
# MovieDetails:
#
# Constructor(...)
# Properties:
#   Movie_ID: int
#   Title: string
#   Release_Date: string, date only (no time)
#   Runtime: int (minutes)
#   Original_Language: string
#   Budget: int (USD)
#   Revenue: int (USD)
#   Num_Reviews: int
#   Avg_Rating: float
#   Tagline: string
#   Genres: list of string
#   Production_Companies: list of string
#
class MovieDetails:
  def __init__(self, id, title, rdate, runtime, language, budget, revenue, reviews, rating, tagline, genres, companies):
    self._Movie_ID_ = id
    self._Title_ = title
    self._Release_Date_ = rdate
    self._Runtime_ = runtime
    self._Original_Language_ = language
    self._Budget_ = budget
    self._Revenue_ = revenue
    self._Num_Reviews_ = reviews
    self._Avg_Rating_ = rating
    self._Tagline_ = tagline
    self._Genres_ = genres
    self._Production_Companies_ = companies

  @property
  def Movie_ID(self):
    return self._Movie_ID_

  @property
  def Title(self):
    return self._Title_

  @property
  def Release_Date(self):
    return self._Release_Date_

  @property
  def Runtime(self):
    return self._Runtime_

  @property
  def Original_Language(self):
    return self._Original_Language_

  @property
  def Budget(self):
    return self._Budget_

  @property
  def Revenue(self):
    return self._Revenue_

  @property
  def Num_Reviews(self):
    return self._Num_Reviews_

  @property
  def Avg_Rating(self):
    return self._Avg_Rating_

  @property
  def Tagline(self):
    return self._Tagline_

  @property
  def Genres(self):
    return self._Genres_

  @property
  def Production_Companies(self):
    return self._Production_Companies_


##################################################################
# 
# num_movies:
#
# Returns: # of movies in the database; if an error returns -1
#
def num_movies(dbConn):
  sql = "Select Count(Movie_ID) From Movies"

  row = datatier.select_one_row(dbConn, sql)

  # return -1 if movie was not found
  if row is None:
    return -1

  return row[0]


##################################################################
# 
# num_reviews:
#
# Returns: # of reviews in the database; if an error returns -1
#
def num_reviews(dbConn):
  sql = "Select Count(*) From Ratings"

  row = datatier.select_one_row(dbConn, sql)

  # return -1 in the event of an error
  if row is None:
    return -1

  return row[0]


##################################################################
#
# get_movies:
#
# gets and returns all movies whose name are "like"
# the pattern. Patterns are based on SQL, which allow
# the _ and % wildcards. Pass "%" to get all stations.
#
# Returns: list of movies in ascending order by name; 
#          an empty list means the query did not retrieve
#          any data (or an internal error occurred, in
#          which case an error msg is already output).
#
def get_movies(dbConn, pattern):
  movies = []

  sql = "Select Movie_ID, Title, strftime('%Y', Release_Date) From Movies Where Title Like ? Order by Title Asc"

  rows = datatier.select_n_rows(dbConn, sql, [pattern])

  if rows is None:
    return []
  for row in rows:
    # instantiate movie for each row and append to list
    m = Movie(row[0], row[1], row[2])
    movies.append(m)

  return movies


##################################################################
#
# get_movie_details:
#
# gets and returns details about the given movie; you pass
# the movie id, function returns a MovieDetails object. Returns
# None if no movie was found with this id.
#
# Returns: if the search was successful, a MovieDetails obj
#          is returned. If the search did not find a matching
#          movie, None is returned; note that None is also 
#          returned if an internal error occurred (in which
#          case an error msg is already output).
#
def get_movie_details(dbConn, movie_id):
  sql = """Select Title, Date(Release_Date), Runtime, Original_Language, Budget, Revenue
  From Movies
  Where Movies.Movie_ID = ?"""

  row = datatier.select_one_row(dbConn, sql, [movie_id])

  # If row has length of zero, movie doesn't exist
  if len(row) == 0:
    return None
  else:
    # Get tagline
    sql = """Select Tagline From Movie_Taglines
          Where Movie_ID = ?"""
    t = datatier.select_one_row(dbConn, sql, [movie_id])
    # Check if movie already has a tagline or not
    if len(t) == 0:
      tag = ""
    else:
      tag = t[0]

    # Get average and number of reviews
    sql = """Select AVG(Rating), Count(Rating) From Ratings
          Where Movie_ID = ?"""
    r = datatier.select_one_row(dbConn, sql, [movie_id])

    # If num reviews is zero, avg should also be zero
    if r[0] is None:
      avg = 0
    else:
      avg = r[0]

    # Getting Genres
    sql = """Select Genre_Name From Genres
          Join Movie_Genres on Genres.Genre_ID = Movie_Genres.Genre_ID
          Where Movie_ID = ?
          Order by Genre_Name Asc"""
    rows = datatier.select_n_rows(dbConn, sql, [movie_id])

    genres = []
    if rows is not None:
      for genre in rows:
        genres.append(genre[0])
    
    # Getting Companies
    sql = """Select Company_Name From Companies
          Join Movie_Production_Companies on 
          Companies.Company_ID = Movie_Production_Companies.Company_ID
          Where Movie_ID = ?
          Order by Company_Name Asc"""
    
    rows = datatier.select_n_rows(dbConn, sql, [movie_id])
    companies = []

    for company in rows:
      companies.append(company[0])
    
    # Now use all information to create movieDetails object
    m = MovieDetails(movie_id, row[0], row[1], row[2], row[3], row[4], row[5], r[1], avg, tag, genres, companies)

    return m
         

##################################################################
#
# get_top_N_movies:
#
# gets and returns the top N movies based on their average 
# rating, where each movie has at least the specified # of
# reviews. Example: pass (10, 100) to get the top 10 movies
# with at least 100 reviews.
#
# Returns: returns a list of 0 or more MovieRating objects;
#          the list could be empty if the min # of reviews
#          is too high. An empty list is also returned if
#          an internal error occurs (in which case an error 
#          msg is already output).
#
def get_top_N_movies(dbConn, N, min_num_reviews):
  movies = []

  sql = f"""Select Movies.Movie_ID, Title, strftime('%Y', Release_Date), AVG(Rating), Count(Rating) 
  From Movies 
  Join Ratings on Movies.Movie_ID = Ratings.Movie_ID
  Group by Movies.Movie_ID
  Having COUNT(Rating) >= {min_num_reviews}
  Order by AVG(Rating) Desc
  Limit {N}"""

  rows = datatier.select_n_rows(dbConn, sql)

  if rows is None:
    return []
  else:
    for row in rows:
      # Create movieRating object
      m = MovieRating(row[0], row[1], row[2], row[4], row[3])
      movies.append(m)
    return movies



##################################################################
#
# add_review:
#
# Inserts the given review --- a rating value 0..10 --- into
# the database for the given movie. It is considered an error
# if the movie does not exist (see below), and the review is
# not inserted.
#
# Returns: 1 if the review was successfully added, returns
#          0 if not (e.g. if the movie does not exist, or if
#          an internal error occurred).
#
def add_review(dbConn, movie_id, rating):
  # Check if movie_ID is valid
  sql = "Select Count(Movie_ID) from Movies where Movie_ID = ?"
  row = datatier.select_one_row(dbConn, sql, [movie_id])
  if row[0] == 0:
    return 0

  # Inset rating into table
  sql = """Insert into Ratings (Movie_ID, Rating)
        Values (?, ?)"""
  
  val = datatier.perform_action(dbConn, sql, [movie_id, rating])

  if val == -1:
    return 0
  else:
    return val


##################################################################
#
# set_tagline:
#
# Sets the tagline --- summary --- for the given movie. If
# the movie already has a tagline, it will be replaced by
# this new value. Passing a tagline of "" effectively 
# deletes the existing tagline. It is considered an error
# if the movie does not exist (see below), and the tagline
# is not set.
#
# Returns: 1 if the tagline was successfully set, returns
#          0 if not (e.g. if the movie does not exist, or if
#          an internal error occurred).
#
def set_tagline(dbConn, movie_id, tagline):
  # Check if movie_id is valid
  sql = "Select Movie_ID from Movies where Movie_ID = ?;"
  row = datatier.select_one_row(dbConn, sql, [movie_id])
  if row is None or len(row) == 0:
    return 0

  # Check if there is an existing tagline for this movie
  sql = "Select Movie_ID from Movie_Taglines where Movie_ID = ?;"
  row = datatier.select_one_row(dbConn, sql, [movie_id])
  # If there is, update the tagline
  if row is None or len(row) == 0:
    sql = """Insert into Movie_Taglines (Movie_ID, Tagline)
          Values (?, ?)"""
    val = datatier.perform_action(dbConn, sql, [movie_id, tagline])
  # Otherwise, insert a new tagline into the table
  else:
    sql = "Update Movie_Taglines Set Tagline = ? Where Movie_ID = ?;"
    val = datatier.perform_action(dbConn, sql, [tagline, movie_id])
  
  return val