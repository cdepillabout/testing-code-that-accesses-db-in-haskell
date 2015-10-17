
# 5 Ways to Test Application Code that Accesses a Database in Haskell

This repository contains 5 separate Haskell project.  Each project demonstrates
a different way of testing application code that accesses a database.

These five Haskell projects were created for the blog post here (TODO: put in
link to blogpost), which explains the pros and cons of each approach.  Please
see the blog post for more information.  Please see below for more information
about the application in question.

The five projects are separated into two groups.  Three of the projects are in
the [`without-db/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/without-db) directory, and the other two projects are in the [`with-db/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/with-db)
directory.

The three projects in the [`without-db/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/without-db) directory do not use a database in the
tests.  The two projects in the [`with-db/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/with-db) directory do access a database in
the tests.

## [`without-db/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/without-db) directory

There are three projects in the [`without-db/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/without-db) directory.  These three projects
do not connect to an actual database in the tests.  This has the benefit of
making the tests not dependent on a database.

The three projects are as follows:

- [`without-db/free-monad/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/without-db/free-monad)
    - This approach uses a free-monad to make a DSL to describe database
      access.  Two separate interpreters for the DSL are created.  One
      interpreter is used in production and one interpreter is used for tests.
      The interpreter used in production actually interacts with the database
      (e.g. putting data into the database and getting data out of the
      database).  The interpreter used for tests simulates a database using a
      hashmap.
- [`without-db/typeclass/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/without-db/typeclass)
    - This approach abstracts out database access using a typeclass.  In
      production, there is an instance of the typeclass that accesses the
      database. For testing, there is an instance of the typeclass that
      simulates a database with a hashmap.
- [`without-db/datatype/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/without-db/datatype)
    - This is similar to the typeclass approach, but instead of using a
      typeclass, it just has a datatype that represents methods to access a
      database.  In production the datatype is filled in with methods that
      access a database, while in tests the datatype is filled in with methods
      that simulate accessing a database by using a hashmap.

## [`with-db/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/with-db) directory

There are two projects in the [`with-db/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/with-db) directory.  These two projects connect
to an actual database even in the tests.

- [`with-db/test-database/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/with-db/test-database)
    - This approach uses two separate databases.  There is a production
      database and a test database.  When unit tests are run, they are only
      accessing the test database.  When code is run in production, it is only
      accessing the production database.
- [`with-db/in-memory-database/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/with-db/in-memory-database)
    - Thie approach is similar to the previous one, except an in-memory
      database is used for the test database.

## Explanation of the Application

All 5 projects implement a similar application.  The application is a simple
REST API.  It uses [servant](https://hackage.haskell.org/package/servant) to
define the API.

The API allows the user to operate on blog posts.  The API has 4 endpoints that
correspond to
[CRUD](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete)
operations on blog posts.

### API

- Create
    - This lets you create a new blog post on the server.
    - METHOD: POST
    - URL: http://localhost:8080/create
    - BODY: blog post JSON
        - EXAMPLE: `{"title": "example title", "content": "example content"}`
    - RETURNS: id of new blog post
- Read
    - This lets you query a blog post based on id.
    - METHOD: GET
    - URL: http://localhost:8080/read/<id>
    - BODY: empty
    - RETURNS: json of blog post
- Update
    - This lets you update an existing blog post.
    - METHOD: PUT
    - URL: http://localhost:8080/update/<id>
    - BODY: blog post JSON
        - EXAMPLE: `{"title": "updated title", "content": "updated content"}`
    - RETURNS: nothing
- Update
    - This lets you delete an existing blog post.
    - METHOD: DELETE
    - URL: http://localhost:8080/delete/<id>
    - BODY: empty
    - RETURNS: nothing

### Run the API

The following shows how to build and run the API on the command line.  It is
similar for all projects.  The
[`without-db/free-monad/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/without-db/free-monad)
project is used below as an example.

The following uses the [stack](https://github.com/commercialhaskell/stack)
build tool.  You must have `stack` installed.

#### Build the API

```bash
$ cd without-db/free-monad/
$ stack build
```

#### Run the API

```bash
$ stack exec free-monad-exe

api running on port 8080...
```

#### Run Unit Tests

```bash
$ stack test
```

#### Test the API from the Command Line

You can test a REST API easily with cURL.  First, run `stack exec
free-monad-exe` in one terminal, then in another terminal use cURL like below:

```bash
# create a new blog post
$ curl -D - -H "Content-Type: application/json" -X POST -d '{"title": "example title", "content": "example content"}' http://localhost:8080/create

HTTP/1.1 201 Created
Transfer-Encoding: chunked
Date: Sat, 17 Oct 2015 23:37:32 GMT
Server: Warp/3.1.3.1
Content-Type: application/json

1

# read the blog post we just created
$ curl -D - -H "Content-Type: application/json" -X GET http://localhost:8080/read/1
HTTP/1.1 200 OK

Transfer-Encoding: chunked
Date: Sat, 17 Oct 2015 23:38:53 GMT
Server: Warp/3.1.3.1
Content-Type: application/json

{"content":"example content","title":"example title"}

# update the blog post to change the content
$ curl -D - -H "Content-Type: application/json" -X PUT -d '{"title": "example title", "content": "new content"}' http://localhost:8080/update/1

HTTP/1.1 204 No Content
Date: Sat, 17 Oct 2015 23:40:24 GMT
Server: Warp/3.1.3.1

# read the updated blog post
$ curl -D - -H "Content-Type: application/json" -X GET http://localhost:8080/read/1
HTTP/1.1 200 OK

Transfer-Encoding: chunked
Date: Sat, 17 Oct 2015 23:38:53 GMT
Server: Warp/3.1.3.1
Content-Type: application/json

{"content":"new content","title":"example title"}

# delete the blog post
$ curl -D - -H "Content-Type: application/json" -X DELETE http://localhost:8080/delete/1

HTTP/1.1 204 No Content
Date: Sat, 17 Oct 2015 23:42:07 GMT
Server: Warp/3.1.3.1

$
```
