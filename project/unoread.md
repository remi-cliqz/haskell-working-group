
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

Now we're going to implement our *fetcher*. Here is what we expect:

- Give an `URL` as argument
- Get raw data as output (Should be `ByteString`)
- Gracefully handle errors (Type-safe API)

The type signature of our `fetch` function should be something like:

```haskell
fetch :: Url -> IO (Either Exception LByteString)
```

Which means we take an URL as argument, and the result is an *effectful action* (`IO`) producing either an exception, or the content of the fetched page.

There are a lot of libraries to perform HTTP/HTTPs fetching in Haskell, and the APIs can be very different. Here are a few of the options:

- HTTP package (only HTTP, no HTTPs)
- wreq
- req
- http-client
- http-conduit

They all have their pros and cons, and it's surprisingly hard to find a *simple* solution that would handle both HTTP and HTTPs. To avoid any complicated and uninteresting implementation, we will use the simpler `download-curl` package, which consists in a wrapper over `libcurl` exposing a very friendly API:

```haskell
import Network.Curl.Download.Lazy

-- First argument is URL
fetch :: String -> IO (Either String LByteString)
fetch = openLazyURI
```

To be able to use this you need to:

- Modify your `stack.yaml` to include an extra dependency:

```haskell
extra-deps:
  - download-curl-0.1.4
```

- Modify your `unoread.cabal` to include an extra entry in `build-depends`:

```haskell
  build-depends:   base >= 4.7 && < 5
                 , protolude >= 0.1.6 && < 0.2
                 , download-curl
```

Once this is done, you can create the `src/Lib/Fetch.hs` file, with the following content:

```haskell
module Lib.Fetch (fetch) where

import Lib.Prelude
import Network.Curl.Download.Lazy

-- Only needed because `String` is not exposed by
-- our custom prelude (Protolude).
type String = [Char]

fetch :: String -> IO (Either String LByteString)
fetch = openLazyURI
```

You then have to expose this as part of the public API in `src/Lib.hs`. Then you can give it a try in `ghci`:

```haskell
*Main Lib Lib.Extract Lib.Fetch Lib.Prelude Lib.Types.Article> result <- fetch "https://cliqz.com"
*Main Lib Lib.Extract Lib.Fetch Lib.Prelude Lib.Types.Article> :type result
result :: Either String LByteString
```

\newpage
## Content Extraction

The previous section didn't involve much thinking, because fetching is not the most exciting task we have to solve and it can be tricky to do it in Haskell (which is why we choose the simpler solution of using a wrapper over `curl`). This section is about extracting the actual content of a page, given the HTML content (output of our fetcher).

There are various ways to implement content extraction, and this is still an open problem. Due to the huge variety of websites on the Web, it is very difficult to come up with a method able to extract the main content on an arbitrary page.

We will first consider a super simple heuristic to extract text, which will allow us to play a bit with `bytestrings`, writing functions, composing them, getting help from types. And then we will see if we can find a more elaborate way to do it, as a bonus.

We will now create an `src/Lib/Extract.hs` module with the following public API:

```haskell
module Lib.Extract where

extract :: LByteString -> LByteString
--         ^              ^
--         raw content    text content
```

It takes the raw page content, and returns only the actual text.

### Tag Soup

First we need to be able to parse the HTML that will be returned by the fetcher. The simplest way to do this is to extract a list of tags (a *tag soup*) using the `tagsoup` package (it won't allow us to keep track of the arborescent structure of the page, but a stream of tags will be enough for our simple heuristic). Add it to your `unoread.cabal` file, then make use of the following functions (for more, have a [look at the documentation](https://hackage.haskell.org/package/tagsoup-0.14/docs/Text-HTML-TagSoup.html)):

```haskell
import Text.HTML.TagSoup

parseTags :: LByteString -> [Tag LByteString]
isTagText :: Tag LByteString -> Bool
fromTagText :: Tag LByteString -> LByteString
maybeTagText :: Tag LByteString -> Maybe LByteString
```

### How to differentiate text from the rest?

If you simply extract `TagText` tags from the *soup*, you will see that you get a lot of javascript code along with the actual text content of the page. A simple way to discard the code is to compute the ratio of number of punctuation and symboles, over the total number of chars in any block of text. Using a threshold of `0.1` should then do the trick (from empiric testing, you can come up with your own value here).

You might want to implement the following functions (it's only a suggestion, you can do it the way you like!):

```haskell
-- Get tag soup from HTML
parseHTML :: LByteString -> [Tag LByteString]

-- Only keep text tags and get the actual content of each of them
getTextTags :: [Tag LByteString] -> [LByteString]

-- Discard code blocks to only keep text
countSpecialCharacters :: LByteString -> Int
ratio :: LByteString -> Double
getContent :: [LByteString] -> [LByteString]
```

Some functions you might want to use can be found in:

- [`Data.Char`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Char.html)
- [`Data.ByteString.Lazy.Char8`](https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString-Lazy-Char8.html)
- [`Data.ByteString.Lazy`](https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString-Lazy.html)


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
