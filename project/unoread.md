
# Project: U No Read?

In the context of the Haskell Brownbag, we will work around a concrete project to structure our sessions. The proposed project is a basic reading list program, that we could extend if we have time and if people are interested. This project will allow us to make use of a wide range of Haskell features and libraries, to tackle diverse tasks.

A basic verion of the program should be able to do the following:

- Given a URL, fetch the page
- Basic processing of the content (extract text and title)
- List articles in the queue
- Access an article to read
- Persist the articles on disk
- Have a command-line interface to interact with the reading list

More advanced tasks:

- Add tests to our project
- Handle RSS/Atom feed
- Design a REST API to access the article
- Store articles in a database
- Basic web interface to access the reading list
- Basic recommandation of what to read
- Basic summarization of the content

The following sections will describe in detail how to implement each step. At the end of the document, you should have a fully *functional* reading list.

\newpage
## Project setup

### Stack new

We will use `stack` to create and manage our project. You can create a project with a custome structure as follows:
```sh
stack new unoread protolude \
    -p author-email:remi@cliqz.com \
    -p author-name:Rémi \
    -p category:Web \
    -p copyright:Rémi \
    -p github-username:remi-cliqz
```

You can omit the parameters and just use `stack new unoread protolude` and specify the values later. But it is much more convenient to do it now.

### Custom prelude

For this project we will make use of a **custom prelude** named [Protolude](http://www.stephendiehl.com/posts/protolude.html) instead of the default one. It will help us by:

- Having better defaults
- Avoiding unsafe functions
- Access common libraries out-of-the-box
- Everything we be explicitely imported
- Use `Text` (as `LText`) instead of `[Char]` to represent *strings*.
- Use a strict set of flags when compiling


### Project structure

You will then be able to access the `unoread` directory, which contains everything you need for your project. You can run:

  #. `stack build`
  #. `stack exec unoread-exe`

The structure should be as follows:

```sh
app/Main.hs             # Entry point
LICENSE
README.md
Setup.hs                # You can ignore
src/Lib.hs              # Exports public API
src/Lib/Prelude.hs      # Custom set of defaults for the project
stack.yaml              # Extra deps + Compiler version to use
test/Spec.hs            # Tests
unoread.cabal           # Configuration
```

Most of the time, you will only need to care about:

- **unoread.cabal**
- **app/Main.hs**
- **src/**
- **test/**

#### unoread.cabal

This is the configuration of you project. You can do things like:

- add modules to your build
- add external dependencies
- change metadata, etc.

#### app/Main.hs

The **app/** folder contains entry points of your project. So far we will use **Main.hs** as a *CLI* entrypoint for our project. Later we might want to add an API or web interface there.

#### src/

This is where most of the code will be. This contains **Lib/** and **Lib.hs** at the top-level.

- **Lib/** will contain our different modules (files) and submodules (you can nest directories there), that can be part of the public API or not.
- **Lib.hs** actually specifies what is exposed when you `import Lib` (the public API, can be thought of as the `__init__.py` file)

#### tests/

This is where we can add tests. We might see this later in the project. For now you can ignore this folder.


\newpage
## Types (Article)

A good first step in a Haskell project is to think about:

  #. Structure
  #. Types

The implementation follows later. So let's start by the core concept of our reading list, the **Article**. We will start with a simple definition, that we can improve later.

### Let's create a new module

Let's create a directory **src/Lib/Types/** in which we will declare types. And then a file **src/Lib/Types/Article.hs** in which we will define our *Article*. This file should start with:

```haskell
module Lib.Types.Article where

import Lib.Prelude
-- Or: import Lib.Prelude (Show, Eq, LText)
-- To only import what we need
-- Or: import Lib.Prelude as P (Show, Eq, LText)
-- To qualify the imports *and* only expose what we need
```

It is also possible to specify what is exported as follows:

```haskell
module Lib.Types
    ( fun1
    , fun2
    , Type1(..)
    , fun3
    -- You could also re-export a module with:
    -- module TheModule
    ) where

-- import TheModule
```

The funny syntax `type1(..)` exports both the *type* **Type1** as well as all the *constructors*[^1]

### What's in an article?

Add the following types:

```haskell
newtype Url = Url LText deriving (Show, Eq)
newtype Content = Content LText deriving (Show, Eq)

-- This is a record
data Article = Article
    { url :: Url
    , content :: Content
    } deriving (Show, Eq)
-- More or less equivalent to:
-- data Article = Article Url Content
--     deriving (Show, Eq)
-- But with accessors `url` and `content`
```

Tasks:

  #. Add the type in the **export list** of the module
  #. Export it in the **src/Lib.hs** file too
  #. Add your new module in **unoread.cabal**
  #. Use it in **app/Main.hs**

The record can be instantiated as follows:

```haskell
let u = Url "url"
    c = Content "content"
    article1 = Article{ url = u, content = c}
    article2 = Article u c
in print article1
```

Then you can test using `stack build` and `stack exec unoread-exe` to see what happens.

### Questions

  #. What could happen if we don't make use of newtypes `Url` and `Content`?
  #. What happens if you swap `url` and `content` arguments while constructing `Article`?
  #. What happens if you don't derive anything for `Url` and `Content` types?

[^1]: this is usually what you want to do, unless you want to prevent people from instantiating your type.

\newpage
## Fetching

\newpage
## Processing

\newpage
## CLI

\newpage
## Serialization

\newpage
## Storage

\newpage
## Tests

\newpage
## REST API

\newpage
## RSS

\newpage
## Web Interface

\newpage
## Recommandation

\newpage
## Summarization
