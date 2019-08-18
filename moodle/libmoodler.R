#moodler
#https://channel9.msdn.com/Events/useR-international-R-User-conferences/useR-International-R-User-2017-Conference/moodler-A-new-R-package-to-easily-fetch-data-from-Moodle
#https://github.com/jchrom/moodler/tree/master/R

library(githubinstall)
githubinstall("moodler")

library(moodler)
?get_courses

.con = DBI::dbConnect(
  RMySQL::MySQL(),
  password = "8332327b12", username = "bn_moodle", dbname = "bitnami_moodle", host = "13.233.130.152"
)
# Create a database connection
con = DBI::dbConnect(RMySQL::MySQL())

DBI::dbGetQuery(.con, "SET NAMES utf8")
DBI::dbGetQuery(.con, "SET sql_mode = ''")

#https://docs.bitnami.com/virtual-machine/apps/moodle/administration/connect-remotely/


# Get a course list
courses = get_courses(con)

# Get specific modules
modules = get_course_modules(con, course.id = 2, module.type = c("quiz", "forum"))


#Extract Form data
crs = get_courses(.con)
mdl = get_course_modules(.con, 6, module.type = 'forum')
#get module data
forum = get_forum(conn=.con, forum.id = 155)
posts = get_module_data(forum)

forum$settings

#exporting graphing data
edges = extract_edges(posts)
nodes = extract_nodes(posts)

#extract quiz data
quiz = get_quiz(.con, quiz.id = 101)
quiz_data = get_module_data(quiz)

#xport ans for item analysis

