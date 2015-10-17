
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
- `without-db/typeclass/`
    - This approach abstracts out database access using a typeclass.  In
      production, there is an instance of the typeclass that accesses the
      database. For testing, there is an instance of the typeclass that
      simulates a database with a hashmap.
- `without-db/datatype/`
    - This is similar to the typeclass approach, but instead of using a
      typeclass, it just has a datatype that represents methods to access a
      database.  In production the datatype is filled in with methods that
      access a database, while in tests the datatype is filled in with methods
      that simulate accessing a database by using a hashmap.

## [`with-db/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/with-db) directory

There are two projects in the [`with-db/`](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/tree/master/with-db) directory.  These two projects connect
to an actual database even in the tests.

- `with-db/test-database/`
    - This approach uses two separate databases.  There is a production
      database and a test database.  When unit tests are run, they are only
      accessing the test database.  When code is run in production, it is only
      accessing the production database.
- `with-db/in-memory-database/`
    - Thie approach is similar to the previous one, except an in-memory
      database is used for the test database.

## Explanation of the Application
